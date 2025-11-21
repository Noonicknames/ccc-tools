use crate::components::LogComponent;

#[derive(Debug, Clone, Copy, Default, Hash, PartialEq, Eq)]
pub enum LogLevel {
    /// Logs which are only meant for helpful information.
    #[default]
    Info,
    /// Logs which warn something may be wrong but could be fine.
    /// 
    /// For example use of deprecated formats.
    Warn,
    /// Logs indicating something has gone wrong.
    Error,
}

impl LogLevel {
    pub fn name(&self) -> &'static str {
        match self {
            Self::Info => "info",
            Self::Warn => "warning",
            Self::Error => "error",
        }
    }
}

#[derive(Default)]
pub struct Log {
    pub level: LogLevel,
    /// Main message shown next to the log level.
    ///
    /// # Example
    /// ```txt
    /// error: <message>
    ///
    /// ```
    pub message: String,
    pub components: Vec<LogComponent>,
    pub sublogs: Vec<Log>,
}

impl Log {
    pub fn add_component(&mut self, component: LogComponent) {
        self.components.push(component);
    }
    pub fn with_component(mut self, component: LogComponent) -> Self {
        self.add_component(component);
        self
    }
    pub fn add_sublog(&mut self, sublog: Log) {
        self.sublogs.push(sublog);
    }
    pub fn with_sublog(mut self, sublog: Log) -> Self {
        self.add_sublog(sublog);
        self
    }
}