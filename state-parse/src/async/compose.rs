//! Structs used to compose parse sections together or manipulate their arguments.

use std::{future::Future, marker::PhantomData, pin::Pin};

use crate::r#async::{AsyncParseSource, AsyncSectionParser, DynAsyncSectionParser};

pub trait AsyncSectionParserBuilder<R>: AsyncSectionParser<R>
where
    R: tokio::io::AsyncBufRead,
    Self: Sized,
{
    fn map_err<F, E>(self, f: F) -> MapError<Self, F, R, E>
    where
        F: Fn(Self::Error) -> E + Sync,
    {
        MapError {
            section: self,
            map: f,
            phantom: PhantomData,
        }
    }
    fn map_output<F, O>(self, f: F) -> MapOutput<Self, F, R, O>
    where
        F: Fn(&mut O) -> &mut Self::Output + Sync,
        O: Send + Sync,
    {
        MapOutput {
            section: self,
            map: f,
            phantom: PhantomData,
        }
    }
    fn map_context<F, C>(self, f: F) -> MapContext<Self, F, R, C>
    where
        F: Fn(&C) -> &Self::Context + Sync,
        C: Sync,
    {
        MapContext {
            section: self,
            map: f,
            phantom: PhantomData,
        }
    }
    fn map_state<F, S>(self, f: F) -> MapState<Self, F, R, S>
    where
        F: Fn(&mut S) -> &mut Self::State + Sync,
        S: Sync + Send + Default,
    {
        MapState {
            section: self,
            map: f,
            phantom: PhantomData,
        }
    }
}

impl<R, T> AsyncSectionParserBuilder<R> for T
where
    R: tokio::io::AsyncBufRead,
    T: AsyncSectionParser<R>,
    Self: Sized,
{
}

pub struct MapState<T, F, R, State>
where
    R: tokio::io::AsyncBufRead,
    T: AsyncSectionParser<R>,
    F: Fn(&mut State) -> &mut T::State,
    State: Sync + Send + Default,
{
    section: T,
    map: F,
    phantom: PhantomData<(R, State)>,
}

impl<T, F, R, State> AsyncSectionParser<R> for MapState<T, F, R, State>
where
    R: Sync + tokio::io::AsyncBufRead,
    T: AsyncSectionParser<R>,
    F: Sync + Fn(&mut State) -> &mut T::State,
    State: Sync + Send + Default,
{
    type Output = T::Output;
    type Context = T::Context;
    type Error = T::Error;
    type State = State;
    fn can_start_parse(
        &self,
        context: &Self::Context,
        state: &mut Self::State,
        line: &str,
    ) -> bool {
        self.section
            .can_start_parse(context, (self.map)(state), line)
    }
    fn parse_section<'s>(
        &'s self,
        context: &'s Self::Context,
        state: &'s mut Self::State,
        source: Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        self.section
            .parse_section(context, (self.map)(state), source, output)
    }
}

pub struct MapOutput<T, F, R, Output>
where
    R: tokio::io::AsyncBufRead,
    T: AsyncSectionParser<R>,
    F: Fn(&mut Output) -> &mut T::Output,
    Output: Sync + Send,
{
    section: T,
    map: F,
    phantom: PhantomData<(R, Output)>,
}

impl<T, F, R, Output> AsyncSectionParser<R> for MapOutput<T, F, R, Output>
where
    R: tokio::io::AsyncBufRead + Sync,
    T: AsyncSectionParser<R> + Sync,
    F: Fn(&mut Output) -> &mut T::Output + Sync,
    Output: Sync + Send,
{
    type Output = Output;
    type Context = T::Context;
    type Error = T::Error;
    type State = T::State;
    fn can_start_parse(
        &self,
        context: &Self::Context,
        state: &mut Self::State,
        line: &str,
    ) -> bool {
        self.section.can_start_parse(context, state, line)
    }
    fn parse_section<'s>(
        &'s self,
        context: &'s Self::Context,
        state: &'s mut Self::State,
        source: Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        self.section
            .parse_section(context, state, source, (self.map)(output))
    }
}

pub struct MapContext<T, F, R, Context>
where
    R: tokio::io::AsyncBufRead,
    T: AsyncSectionParser<R>,
    F: Fn(&Context) -> &T::Context + Sync,
    Context: Sync,
{
    section: T,
    map: F,
    phantom: PhantomData<(R, Context)>,
}

impl<T, F, R, Context> AsyncSectionParser<R> for MapContext<T, F, R, Context>
where
    R: tokio::io::AsyncBufRead + Sync,
    T: AsyncSectionParser<R> + Sync,
    F: Fn(&Context) -> &T::Context + Sync,
    Context: Sync,
{
    type Output = T::Output;
    type Context = Context;
    type Error = T::Error;
    type State = T::State;
    fn can_start_parse(
        &self,
        context: &Self::Context,
        state: &mut Self::State,
        line: &str,
    ) -> bool {
        self.section
            .can_start_parse((self.map)(context), state, line)
    }
    fn parse_section<'s>(
        &'s self,
        context: &'s Self::Context,
        state: &'s mut Self::State,
        source: Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        self.section
            .parse_section((self.map)(context), state, source, output)
    }
}

pub struct MapError<T, F, R, Error>
where
    R: tokio::io::AsyncBufRead,
    T: AsyncSectionParser<R>,
    F: Fn(T::Error) -> Error,
{
    section: T,
    map: F,
    phantom: PhantomData<(R, Error)>,
}

impl<T, F, R, Error> AsyncSectionParser<R> for MapError<T, F, R, Error>
where
    R: tokio::io::AsyncBufRead + Send + Sync,
    T: AsyncSectionParser<R> + Sync,
    F: Fn(T::Error) -> Error + Sync,
    Error: Send + Sync + From<std::io::Error>,
{
    type Output = T::Output;
    type Context = T::Context;
    type Error = Error;
    type State = T::State;
    fn can_start_parse(
        &self,
        context: &Self::Context,
        state: &mut Self::State,
        line: &str,
    ) -> bool {
        self.section.can_start_parse(context, state, line)
    }
    fn parse_section<'s>(
        &'s self,
        context: &'s Self::Context,
        state: &'s mut Self::State,
        source: Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        async move {
            self.section
                .parse_section(context, state, source, output)
                .await
                .map_err(&self.map)
        }
    }
}

pub struct Match<R, Output, Error, Context, State, F>
where
    R: tokio::io::AsyncBufRead,
    Error: From<std::io::Error> + Send,
    State: Send + Sync + Default,
    F: Fn(MatchError) -> Result<(), Error> + Send + Sync,
{
    sections: Vec<
        Box<
            dyn DynAsyncSectionParser<
                R,
                Output = Output,
                Error = Error,
                Context = Context,
                State = State,
            >,
        >,
    >,
    on_match_error: F,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MatchError {
    NoMatchFound,
}

impl<R, Output, Error, Context, State, F> Match<R, Output, Error, Context, State, F>
where
    R: tokio::io::AsyncBufRead,
    Error: From<std::io::Error> + Send,
    State: Send + Sync + Default,
    F: Fn(MatchError) -> Result<(), Error> + Send + Sync,
{
    pub fn empty(on_match_error: F) -> Self {
        Self {
            sections: Vec::new(),
            on_match_error,
        }
    }
    pub fn add_section(
        &mut self,
        section: impl AsyncSectionParser<R, Output = Output, Error = Error, Context = Context, State = State>
            + 'static,
    ) {
        self.add_section_boxed(Box::new(section));
    }
    pub fn add_section_boxed(
        &mut self,
        section: Box<
            dyn DynAsyncSectionParser<
                R,
                Output = Output,
                Error = Error,
                Context = Context,
                State = State,
            >,
        >,
    ) {
        self.sections.push(section);
    }
    pub fn with_section(
        mut self,
        section: impl AsyncSectionParser<R, Output = Output, Error = Error, Context = Context, State = State>
            + 'static,
    ) -> Self {
        self.add_section(section);
        self
    }
    pub fn with_section_boxed(
        mut self,
        section: Box<
            dyn DynAsyncSectionParser<
                R,
                Output = Output,
                Error = Error,
                Context = Context,
                State = State,
            >,
        >,
    ) -> Self {
        self.add_section_boxed(section);
        self
    }
}

impl<R, Output, Error, Context, State, F> AsyncSectionParser<R>
    for Match<R, Output, Error, Context, State, F>
where
    R: tokio::io::AsyncBufRead + Send + Sync,
    Output: Send,
    Error: From<std::io::Error> + Send,
    Context: Sync,
    State: Send + Sync + Default,
    F: Fn(MatchError) -> Result<(), Error> + Send + Sync,
{
    type Output = Output;
    type Error = Error;
    type Context = Context;
    type State = State;
    fn can_start_parse(
        &self,
        context: &Self::Context,
        state: &mut Self::State,
        line: &str,
    ) -> bool {
        self.sections
            .iter()
            .find(|section| section.can_start_parse(context, state, line))
            .is_some()
    }
    fn parse_section<'s>(
        &'s self,
        context: &'s Self::Context,
        state: &'s mut Self::State,
        mut source: Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        async move {
            let line = match source.peek_or_err().await? {
                Some(line) => line,
                None => return Ok(()),
            };
            let section = match self
                .sections
                .iter()
                .find(|section| section.can_start_parse(context, state, line))
            {
                Some(section) => section,
                None => {
                    return (self.on_match_error)(MatchError::NoMatchFound);
                }
            };

            section.parse_section(context, state, source, output).await
        }
    }
}

/// This will repeat a sequence of parses.
pub struct Repeat<R, Output, Error, Context, State, F>
where
    R: tokio::io::AsyncBufRead + Send,
    Error: Send,
    Context: Sync,
    Output: Send + Sync,
    State: Send + Sync + Default,
    F: Fn(RepeatError) -> Result<(), Error> + Send + Sync,
{
    sections: Vec<
        Box<
            dyn DynAsyncSectionParser<
                    R,
                    Output = Output,
                    Error = Error,
                    Context = Context,
                    State = State,
                > + Send,
        >,
    >,
    on_repeat_error: F,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum RepeatError {
    PartialRepeat { last_section_idx: usize },
}

impl<R, Output, Error, Context, State, F> Repeat<R, Output, Error, Context, State, F>
where
    R: tokio::io::AsyncBufRead + Send,
    Error: Send,
    Context: Send + Sync,
    Output: Send + Sync,
    State: Send + Sync + Default,
    F: Fn(RepeatError) -> Result<(), Error> + Send + Sync,
{
    pub fn empty(on_repeat_error: F) -> Self {
        Self {
            sections: Vec::new(),
            on_repeat_error,
        }
    }
    pub fn add_section(
        &mut self,
        section: impl AsyncSectionParser<R, Output = Output, Error = Error, Context = Context, State = State>
            + Send
            + 'static,
    ) {
        self.add_section_boxed(Box::new(section));
    }
    pub fn add_section_boxed(
        &mut self,
        section: Box<
            dyn DynAsyncSectionParser<
                    R,
                    Output = Output,
                    Error = Error,
                    Context = Context,
                    State = State,
                > + Send,
        >,
    ) {
        self.sections.push(section);
    }
    pub fn with_section(
        mut self,
        section: impl AsyncSectionParser<R, Output = Output, Error = Error, Context = Context, State = State>
            + Send
            + 'static,
    ) -> Self {
        self.add_section(section);
        self
    }
    pub fn with_section_boxed(
        mut self,
        section: Box<
            dyn DynAsyncSectionParser<
                    R,
                    Output = Output,
                    Error = Error,
                    Context = Context,
                    State = State,
                > + Send,
        >,
    ) -> Self {
        self.add_section_boxed(section);
        self
    }
}

impl<R, Output, Error, Context, State, F> AsyncSectionParser<R>
    for Repeat<R, Output, Error, Context, State, F>
where
    R: tokio::io::AsyncBufRead + Send,
    Context: Sync,
    Error: Send + From<std::io::Error>,
    Output: Send + Sync,
    State: Send + Sync + Default,
    F: Fn(RepeatError) -> Result<(), Error> + Send + Sync,
{
    type Context = Context;
    type Error = Error;
    type Output = Output;
    type State = State;
    fn can_start_parse(
        &self,
        context: &Self::Context,
        state: &mut Self::State,
        line: &str,
    ) -> bool {
        self.sections
            .first()
            .map(|first_section| first_section.can_start_parse(context, state, line))
            .unwrap_or(false)
    }
    fn parse_section<'s>(
        &'s self,
        context: &'s Self::Context,
        state: &'s mut Self::State,
        mut source: Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        async move {
            // No need to check this one since this is guaranteed to be parseable by Self::can_start_parse
            for (idx, section) in self.sections.iter().enumerate() {
                let mut source = source.as_mut();
                let Some(line) = source.peek_or_err().await? else {
                    // TODO: This means end of file, I may want to express eof information in the error.
                    return (self.on_repeat_error)(RepeatError::PartialRepeat {
                        last_section_idx: idx,
                    });
                };

                if section.can_start_parse(context, state, line) {
                    section
                        .parse_section(context, state, source, output)
                        .await?;
                } else {
                    return (self.on_repeat_error)(RepeatError::PartialRepeat {
                        last_section_idx: idx,
                    });
                }
            }

            loop {
                // Check if the first section can parse
                let Some(line) = source.peek_or_err().await? else {
                    break;
                };

                if !self
                    .sections
                    .first()
                    .map(|first_section| first_section.can_start_parse(context, state, line))
                    .unwrap_or(false)
                {
                    break;
                }

                for (idx, section) in self.sections.iter().enumerate() {
                    let mut source = source.as_mut();
                    let Some(line) = source.peek_or_err().await? else {
                        // TODO: This means end of file, I may want to express eof information in the error.
                        return (self.on_repeat_error)(RepeatError::PartialRepeat {
                            last_section_idx: idx,
                        });
                    };

                    if section.can_start_parse(context, state, line) {
                        section
                            .parse_section(context, state, source, output)
                            .await?;
                    } else {
                        return (self.on_repeat_error)(RepeatError::PartialRepeat {
                            last_section_idx: idx,
                        });
                    }
                }
            }

            Ok(())
        }
    }
}

/// This will repeat a sequence of parses, it will consume until the end of the file searching for matches.
pub struct RepeatConsume<R, Output, Error, Context, State, F>
where
    R: tokio::io::AsyncBufRead + Send,
    Error: Send,
    Context: Sync,
    Output: Send + Sync,
    State: Send + Sync + Default,
    F: Fn(RepeatError) -> Result<(), Error> + Send + Sync,
{
    sections: Vec<
        Box<
            dyn DynAsyncSectionParser<
                    R,
                    Output = Output,
                    Error = Error,
                    Context = Context,
                    State = State,
                > + Send,
        >,
    >,
    on_repeat_error: F,
}

impl<R, Output, Error, Context, State, F> RepeatConsume<R, Output, Error, Context, State, F>
where
    R: tokio::io::AsyncBufRead + Send,
    Error: Send,
    Context: Sync,
    Output: Send + Sync,
    State: Send + Sync + Default,
    F: Fn(RepeatError) -> Result<(), Error> + Send + Sync,
{
    pub fn empty(on_repeat_error: F) -> Self {
        Self {
            sections: Vec::new(),
            on_repeat_error,
        }
    }
    pub fn is_empty(&self) -> bool {
        self.sections.is_empty()
    }
    pub fn len(&self) -> usize {
        self.sections.len()
    }
    pub fn add_section(
        &mut self,
        section: impl AsyncSectionParser<R, Output = Output, Error = Error, Context = Context, State = State>
            + Send
            + 'static,
    ) {
        self.add_section_boxed(Box::new(section));
    }
    pub fn add_section_boxed(
        &mut self,
        section: Box<
            dyn DynAsyncSectionParser<
                    R,
                    Output = Output,
                    Error = Error,
                    Context = Context,
                    State = State,
                > + Send,
        >,
    ) {
        self.sections.push(section);
    }
    pub fn with_section(
        mut self,
        section: impl AsyncSectionParser<R, Output = Output, Error = Error, Context = Context, State = State>
            + Send
            + 'static,
    ) -> Self {
        self.add_section(section);
        self
    }
    pub fn with_section_boxed(
        mut self,
        section: Box<
            dyn DynAsyncSectionParser<
                    R,
                    Output = Output,
                    Error = Error,
                    Context = Context,
                    State = State,
                > + Send,
        >,
    ) -> Self {
        self.add_section_boxed(section);
        self
    }
}

impl<R, Output, Error, Context, State, F> AsyncSectionParser<R>
    for RepeatConsume<R, Output, Error, Context, State, F>
where
    R: tokio::io::AsyncBufRead + Send,
    Context: Sync,
    Error: Send + From<std::io::Error>,
    Output: Send + Sync,
    State: Send + Sync + Default,
    F: Fn(RepeatError) -> Result<(), Error> + Send + Sync,
{
    type Context = Context;
    type Error = Error;
    type Output = Output;
    type State = State;
    fn can_start_parse(
        &self,
        _context: &Self::Context,
        _state: &mut Self::State,
        _line: &str,
    ) -> bool {
        true
    }
    fn parse_section<'s>(
        &'s self,
        context: &'s Self::Context,
        state: &'s mut Self::State,
        mut source: Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        async move {
            if self.is_empty() {
                return Ok(());
            }
            'outer: loop {
                for (idx, section) in self.sections.iter().enumerate() {
                    let mut source = source.as_mut();
                    loop {
                        let Some(line) = source.peek_or_err().await? else {
                            if idx == 0 {
                                break 'outer;
                            } else {
                                // TODO: This means end of file, I may want to express eof information in the error.
                                return (self.on_repeat_error)(RepeatError::PartialRepeat {
                                    last_section_idx: idx,
                                });
                            }
                        };
                        if section.can_start_parse(context, state, line) {
                            break;
                        }
                        source.next().await.unwrap(); // Shouldn't be possible to fail if it was peeked.
                    }

                    section
                        .parse_section(context, state, source, output)
                        .await?;
                }
            }

            Ok(())
        }
    }
}

pub struct UnorderedConsume<R, Output, Error, Context, State>
where
    R: tokio::io::AsyncBufRead,
    Error: From<std::io::Error> + Send,
    State: Send + Sync + Default,
{
    sections: Vec<
        Box<
            dyn DynAsyncSectionParser<
                R,
                Output = Output,
                Error = Error,
                Context = Context,
                State = State,
            >,
        >,
    >,
}

impl<R, Output, Error, Context, State> UnorderedConsume<R, Output, Error, Context, State>
where
    R: tokio::io::AsyncBufRead,
    Error: From<std::io::Error> + Send,
    State: Send + Sync + Default,
{
    pub fn empty() -> Self {
        Self {
            sections: Vec::new(),
        }
    }
    pub fn add_section(
        &mut self,
        section: impl AsyncSectionParser<R, Output = Output, Error = Error, Context = Context, State = State>
            + 'static,
    ) {
        self.add_section_boxed(Box::new(section));
    }
    pub fn add_section_boxed(
        &mut self,
        section: Box<
            dyn DynAsyncSectionParser<
                R,
                Output = Output,
                Error = Error,
                Context = Context,
                State = State,
            >,
        >,
    ) {
        self.sections.push(section);
    }
    pub fn with_section(
        mut self,
        section: impl AsyncSectionParser<R, Output = Output, Error = Error, Context = Context, State = State>
            + 'static,
    ) -> Self {
        self.add_section(section);
        self
    }
    pub fn with_section_boxed(
        mut self,
        section: Box<
            dyn DynAsyncSectionParser<
                R,
                Output = Output,
                Error = Error,
                Context = Context,
                State = State,
            >,
        >,
    ) -> Self {
        self.add_section_boxed(section);
        self
    }
}

impl<R, Output, Error, Context, State> AsyncSectionParser<R>
    for UnorderedConsume<R, Output, Error, Context, State>
where
    Output: Send,
    R: tokio::io::AsyncBufRead + Send,
    Error: From<std::io::Error> + Send,
    Context: Sync,
    State: Send + Sync + Default,
{
    type Output = Output;
    type Error = Error;
    type Context = Context;
    type State = State;
    fn can_start_parse(
        &self,
        context: &Self::Context,
        state: &mut Self::State,
        line: &str,
    ) -> bool {
        self.sections
            .iter()
            .find(|section| section.can_start_parse(context, state, line))
            .is_some()
    }
    fn parse_section<'s>(
        &'s self,
        context: &'s Self::Context,
        state: &'s mut Self::State,
        mut source: Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + 's {
        async move {
            let mut done_cache = vec![false; self.sections.len()];

            'outer: for _ in 0..self.sections.len() {
                'inner: loop {
                    let line = match source.peek_or_err().await? {
                        Some(line) => line,
                        None => break 'outer,
                    };

                    if let Some((idx, section)) = self
                        .sections
                        .iter()
                        .enumerate()
                        .filter(|(idx, _)| !done_cache[*idx])
                        .find(|(_, section)| section.can_start_parse(context, state, line))
                    {
                        done_cache[idx] = true;
                        section
                            .parse_section(context, state, source.as_mut(), output)
                            .await?;
                        break 'inner;
                    } else {
                        source.next().await?;
                    }
                }
            }

            // If not all sections are done, run on_unexpected_end_of_file.
            for section in done_cache
                .iter()
                .zip(self.sections.iter())
                .filter_map(|(done, section)| if !*done { Some(section) } else { None })
            {
                section
                    .on_unexpected_end_of_file(
                        context,
                        state,
                        source.info(),
                        source.current_line_num(),
                    )
                    .await?;
            }

            Ok(())
        }
    }
}
