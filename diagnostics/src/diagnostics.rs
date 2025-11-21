use std::sync::Mutex;
#[cfg(feature = "tokio")]
use std::{
    sync::{atomic::AtomicUsize, Arc},
};

use bitflags::bitflags;
use colored::Colorize;

#[cfg(feature = "tokio")]
use futures::{stream::FuturesUnordered, StreamExt};
#[cfg(feature = "tokio")]
use tokio::{io::AsyncWriteExt, sync::Notify};

use crate::{
    components::DefaultLogComponentRenderer, util::IndentedWriter, Log, LogFmtOptions, LogLevel,
};

/// Central struct for writing out diagnostics messages.
pub struct Diagnostics {
    pub options: LogFmtOptions,
    pub renderer: DefaultLogRenderer,
    pub channels: Vec<(ChannelAllow, Box<Mutex<dyn std::io::Write>>)>,
}

impl Diagnostics {
    pub fn new(options: LogFmtOptions) -> Self {
        Self {
            options,
            renderer: DefaultLogRenderer::new(),
            channels: Vec::new(),
        }
    }
    pub fn add_channel(&mut self, allow: ChannelAllow, output: impl std::io::Write + 'static) {
        self.channels.push((allow, Box::new(Mutex::new(output))));
    }

    pub fn write_log(&self, log: &Log) -> std::io::Result<()> {
        // Exit if there are no channels this log will be written to.
        if self
            .channels
            .iter()
            .find(|(allow, _)| allow.contains(log.level.into()))
            .is_none()
        {
            return Ok(());
        }

        let mut log_message = Vec::new();
        self.renderer
            .render_log(log, &self.options, &mut log_message)?;

        let mut first_error = None;

        for (_, output) in self
            .channels
            .iter()
            .filter(|channel| channel.0.contains(log.level.into()))
        {
            let mut output = match output.lock() {
                Ok(output) => output,
                Err(why) => {
                    eprintln!("{}", why);
                    continue;
                }
            };
            if let Err(why) = output.write_all(&log_message) {
                if first_error.is_none() {
                    first_error = Some(why);
                }
            }
        }

        Ok(())
    }

    /// Write log but will default to printing to stderr if logging fails.
    pub fn write_log_infallible(&self, log: &Log) {
        // Exit if there are no channels this log will be written to.
        if self
            .channels
            .iter()
            .find(|(allow, _)| allow.contains(log.level.into()))
            .is_none()
        {
            return;
        }

        let mut log_message = Vec::new();
        if let Err(why) = self
            .renderer
            .render_log(log, &self.options, &mut log_message)
        {
            eprintln!("{}", why);
            return;
        }

        for (_, output) in self
            .channels
            .iter()
            .filter(|channel| channel.0.contains(log.level.into()))
        {
            let mut output = match output.lock() {
                Ok(output) => output,
                Err(why) => {
                    eprintln!("{}", why);
                    continue;
                }
            };

            if let Err(why) = output.write_all(&log_message) {
                eprintln!("{}", why);
            }
        }
    }
}

/// Central struct for writing out diagnostics messages with an async api.
#[cfg(feature = "tokio")]
pub struct AsyncDiagnostics {
    pub options: LogFmtOptions,
    pub renderer: DefaultLogRenderer,
    pub channels: Vec<(
        ChannelAllow,
        Arc<tokio::sync::Mutex<dyn tokio::io::AsyncWrite + Unpin + Send + Sync>>,
    )>,
    log_thread_counter: AtomicUsize,
    thread_done_notify: Notify,
}

#[cfg(feature = "tokio")]
impl AsyncDiagnostics {
    pub fn new(options: LogFmtOptions) -> Self {
        Self {
            options,
            renderer: DefaultLogRenderer::new(),
            channels: Vec::new(),
            log_thread_counter: AtomicUsize::new(0),
            thread_done_notify: Notify::new(),
        }
    }
    pub fn add_channel(
        &mut self,
        allow: ChannelAllow,
        output: impl tokio::io::AsyncWrite + Unpin + Send + Sync + 'static,
    ) {
        self.channels
            .push((allow, Arc::new(tokio::sync::Mutex::new(output))));
    }

    /// Wait for all logging threads to finish
    ///
    /// Only guarantees there are no more running threads immediately after finishing running.
    /// More logging threads which spawn after this are not accounted for.
    pub async fn finish(&self) {
        use std::sync::atomic::Ordering;

        while self.log_thread_counter.load(Ordering::Relaxed) > 0 {
            self.thread_done_notify.notified().await;
        }
    }

    /// Write log without blocking the current thread to wait for the write to complete
    /// 
    /// Important to use for high latency outputs such as network.
    pub fn write_log_background(self: &Arc<Self>, log: Log) {
        use std::sync::atomic::Ordering;

        let diagnostics = Arc::clone(&self);
        self.log_thread_counter.fetch_add(1, Ordering::Relaxed);
        tokio::spawn(async move {
            diagnostics.write_log_infallible(&log).await;
            diagnostics.log_thread_counter.fetch_sub(1, Ordering::Release);
            diagnostics.thread_done_notify.notify_one();
        });
    }

    pub async fn write_log(&self, log: &Log) -> std::io::Result<()> {
        // Exit if there are no channels this log will be written to.
        if self
            .channels
            .iter()
            .find(|(allow, _)| allow.contains(log.level.into()))
            .is_none()
        {
            return Ok(());
        }

        let mut log_message = Vec::new();
        self.renderer
            .render_log(log, &self.options, &mut log_message)?;

        let log_message = Arc::new(log_message);

        let mut write_futures = self
            .channels
            .iter()
            .filter(|channel| channel.0.contains(log.level.into()))
            .map(|(_, output)| {
                let output = Arc::clone(output);
                let log_message = Arc::clone(&log_message);
                async move { output.lock().await.write_all(&log_message).await }
            })
            .collect::<FuturesUnordered<_>>();

        let mut first_error = None;

        while let Some(result) = write_futures.next().await {
            if let Err(why) = result {
                if first_error.is_none() {
                    first_error = Some(why);
                }
            }
        }

        match first_error {
            Some(why) => Err(why),
            None => Ok(()),
        }
    }

    pub async fn write_log_infallible(&self, log: &Log) {
        // Exit if there are no channels this log will be written to.
        if self
            .channels
            .iter()
            .find(|(allow, _)| allow.contains(log.level.into()))
            .is_none()
        {
            return;
        }

        let mut log_message = Vec::new();

        if let Err(why) = self
            .renderer
            .render_log(log, &self.options, &mut log_message)
        {
            eprintln!("{}", why);
            return;
        }

        let log_message = Arc::new(log_message);

        let mut write_futures = self
            .channels
            .iter()
            .filter(|channel| channel.0.contains(log.level.into()))
            .map(|(_, output)| {
                let output = Arc::clone(output);
                let log_message = Arc::clone(&log_message);
                async move { output.lock().await.write_all(&log_message).await }
            })
            .collect::<FuturesUnordered<_>>();

        while let Some(result) = write_futures.next().await {
            if let Err(why) = result {
                eprintln!("{}", why);
            }
        }
    }
}

bitflags! {
    #[derive(Debug, PartialEq, Eq)]
    pub struct ChannelAllow: u8 {
        const INFO  = 0b00000001;
        const WARN  = 0b00000010;
        const ERROR = 0b00000100;
    }
}

impl From<LogLevel> for ChannelAllow {
    fn from(value: LogLevel) -> Self {
        match value {
            LogLevel::Info => Self::INFO,
            LogLevel::Warn => Self::WARN,
            LogLevel::Error => Self::ERROR,
        }
    }
}

impl TryFrom<ChannelAllow> for LogLevel {
    type Error = ();
    fn try_from(value: ChannelAllow) -> Result<Self, <Self as TryFrom<ChannelAllow>>::Error> {
        match value {
            ChannelAllow::INFO => Ok(Self::Info),
            ChannelAllow::WARN => Ok(Self::Warn),
            ChannelAllow::ERROR => Ok(Self::Error),
            _ => Err(()),
        }
    }
}

impl Default for ChannelAllow {
    fn default() -> Self {
        Self::all()
    }
}

pub trait LogRenderer {
    fn render_log(
        &self,
        log: &Log,
        options: &LogFmtOptions,
        output: &mut dyn std::io::Write,
    ) -> std::io::Result<()>;
}

#[derive(Debug)]
pub struct DefaultLogRenderer {
    component_renderer: DefaultLogComponentRenderer,
}

impl DefaultLogRenderer {
    pub fn new() -> Self {
        Self {
            component_renderer: DefaultLogComponentRenderer::new(),
        }
    }
    fn render_log_indented(
        &self,
        log: &Log,
        options: &LogFmtOptions,
        output: &mut IndentedWriter<&mut dyn std::io::Write>,
    ) -> std::io::Result<()> {
        use std::io::Write;
        write!(
            output,
            "{}: ",
            log.level
                .name()
                .color(options.get_level_color(log.level))
                .bold(),
        )?;

        let mut lines = log.message.lines();

        if let Some(line) = lines.next() {
            let mut line = line.color(options.message_color);
            line.style = options.message_style;
            write!(output, "{}\n", line)?;
        }

        for line in lines {
            let mut line = line.color(options.message_color);
            line.style = options.message_style;
            write!(output, "       {}\n", line)?;
        }

        for component in log.components.iter() {
            self.component_renderer.write(component, options, output)?;
        }

        if log.sublogs.len() >= 1 {
            write!(output, "{:indent$}", "", indent = options.sublog_indent)?;
        }

        for sublog in log.sublogs.iter() {
            output.indent(options.sublog_indent, |indented_output| {
                self.render_log_indented(sublog, options, indented_output)?;
                std::io::Result::Ok(())
            })?;
        }

        write!(output, "\n")?;
        Ok(())
    }
}

impl LogRenderer for DefaultLogRenderer {
    fn render_log(
        &self,
        log: &Log,
        options: &LogFmtOptions,
        output: &mut dyn std::io::Write,
    ) -> std::io::Result<()> {
        self.render_log_indented(log, options, &mut IndentedWriter::new(output, 0))
    }
}
