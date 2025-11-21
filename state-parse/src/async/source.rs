use std::{collections::VecDeque, pin::Pin};

use futures::StreamExt;
use tokio::io::AsyncBufReadExt;
use tokio_stream::wrappers::LinesStream;

#[derive(Default, Debug, Hash, PartialEq, Eq)]
pub struct SourceInfo {
    pub file_path: Option<String>,
}

/// Source for parsing with [`AsyncParser`].
///
/// Create one from anything which implements [`tokio::io::AsyncRead`] with [`make_parse_source`].
#[derive(Debug)]
pub struct AsyncParseSource<R>
where
    R: tokio::io::AsyncBufRead,
{
    source: LinesStream<R>,
    line_num: usize,
    info: SourceInfo,
    lines_buffer: VecDeque<Result<String, std::io::Error>>,
}

impl<R> AsyncParseSource<R>
where
    R: tokio::io::AsyncBufRead,
{
    pub fn new(source: R, info: SourceInfo) -> Self {
        Self {
            source: LinesStream::new(source.lines()),
            line_num: 0,
            info,
            lines_buffer: VecDeque::new(),
        }
    }
    pub async fn next(self: &mut Pin<&mut Self>) -> Result<Option<String>, std::io::Error> {
        // Increment line_num
        unsafe {
            self.as_mut().get_unchecked_mut().line_num += 1;
        }

        if let Some(line) = unsafe { self.as_mut().get_unchecked_mut().lines_buffer.pop_front() } {
            line.map(|line| Some(line))
        } else {
            // This is safe since inner is pinned along with self.
            let mut inner = unsafe { self.as_mut().map_unchecked_mut(|this| &mut this.source) };

            inner
                .next()
                .await
                .map(|result| result.map(|line| line))
                .transpose()
        }
    }

    pub async fn peek<'a>(
        self: &'a mut Pin<&mut Self>,
    ) -> Result<Option<&'a String>, &'a std::io::Error> {
        if self.lines_buffer.is_empty() {
            // This is safe since inner is pinned along with self.
            let AsyncParseSource {
                source,
                lines_buffer,
                ..
            } = unsafe { self.as_mut().get_unchecked_mut() };

            let mut source = unsafe { Pin::new_unchecked(source) };

            if let Some(line) = source.next().await {
                lines_buffer.push_back(line);
            }
        }

        match self.lines_buffer.front() {
            Some(line) => line.as_ref().map(|line| Some(line)),
            None => Ok(None),
        }
    }

    /// Peek but you may at the nth line ahead.
    ///
    /// Note: `peek_n(0)` is equivalent to `peek()`.
    pub async fn peek_n<'a>(
        self: &'a mut Pin<&mut Self>,
        n: usize,
    ) -> Result<Option<&'a String>, &'a std::io::Error> {
        if self.lines_buffer.len() < n {
            // This is safe since inner is pinned along with self.
            let AsyncParseSource {
                source,
                lines_buffer,
                ..
            } = unsafe { self.as_mut().get_unchecked_mut() };
            let mut source = unsafe { Pin::new_unchecked(source) };

            while lines_buffer.len() < n {
                if let Some(line) = source.next().await {
                    lines_buffer.push_back(line);
                } else {
                    break;
                }
            }
        }

        match self.lines_buffer.get(n) {
            Some(line) => line.as_ref().map(|line| Some(line)),
            None => Ok(None),
        }
    }

    pub async fn peek_or_err<'a>(
        self: &'a mut Pin<&mut Self>,
    ) -> Result<Option<&'a String>, std::io::Error> {
        // This is safe since inner is pinned along with self.
        let AsyncParseSource {
            source,
            lines_buffer,
            ..
        } = unsafe { self.as_mut().get_unchecked_mut() };

        if lines_buffer.is_empty() {
            let mut source = unsafe { Pin::new_unchecked(source) };

            if let Some(line) = source.next().await {
                lines_buffer.push_back(line);
            }
        }

        if lines_buffer
            .front()
            .map(|this| this.is_err())
            .unwrap_or(false)
        {
            return Err(lines_buffer.pop_front().unwrap().unwrap_err());
        }

        match lines_buffer.front() {
            Some(Ok(line)) => Ok(Some(line)),
            None => Ok(None),
            Some(Err(_)) => unreachable!(),
        }
    }
    pub fn current_line_num(&self) -> usize {
        self.line_num
    }
    pub fn next_line_num(&self) -> usize {
        self.line_num
    }
    pub fn info(&self) -> &SourceInfo {
        &self.info
    }
}
