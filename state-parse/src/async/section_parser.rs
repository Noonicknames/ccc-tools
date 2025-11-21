use std::{future::Future, pin::Pin};

use futures::{future::BoxFuture, FutureExt};

use crate::r#async::{AsyncParseSource, SourceInfo};

/// Can be used to parse a section.
pub trait AsyncSectionParser<R>: Sync
where
    R: tokio::io::AsyncBufRead,
{
    /// The struct which the parser writes out to.
    ///
    /// A mutable reference of this type will be fed into methods.
    type Output: Send;

    /// The error which the parser returns.
    ///
    /// These should be errors which cause the whole parser to return early such that later sections are ignored, not recoverable ones.
    type Error: Send + From<std::io::Error>;

    /// Any context which may be used to configure the parser or auxilliary output such as logging.
    ///
    /// Difference between this and [`AsyncSectionParser::Output`] is that an immutable reference is fed into methods instead of a mutable one.
    type Context: Sync;

    /// The state of the parser.
    type State: Send + Sync + Default;

    /// Returns whether the line indicates the start of the section this parses.
    fn can_start_parse(&self, context: &Self::Context, state: &mut Self::State, line: &str) -> bool;

    /// Parse the section of the source, only consuming part of the source related to this section.
    fn parse_section<'s>(
        &'s self,
        context: &'s Self::Context,
        state: &'s mut Self::State,
        source: Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's;

    /// Function which is run when the next section to be parsed is never encountered before the end of the file.
    fn on_unexpected_end_of_file<'s>(
        &'s self,
        context: &'s Self::Context,
        state: &'s mut Self::State,
        source_info: &'s SourceInfo,
        line_num: usize,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        _ = (context, state, line_num, source_info);
        async move { Ok(()) }
    }
}

/// Can be used to parse a section.
///
/// Dyn compatible version of [`AsyncSectionParser`].
pub trait DynAsyncSectionParser<R>: Sync
where
    R: tokio::io::AsyncBufRead,
{
    /// The struct which the parser writes out to.
    ///
    /// A mutable reference of this type will be fed into methods.
    type Output: Send;

    /// The error which the parser returns.
    ///
    /// These should be errors which cause the whole parser to return early such that later sections are ignored, not recoverable ones.
    type Error: Send;

    /// Any context which may be used to configure the parser or auxilliary output such as logging.
    ///
    /// Difference between this and [`AsyncSectionParser::Output`] is that an immutable reference is fed into methods instead of a mutable one.
    type Context: Sync;

    /// The state of the parser.
    type State: Send + Sync + Default;

    /// Returns whether the line indicates the start of the section this parses.
    fn can_start_parse(&self, context: &Self::Context, state: &mut Self::State, line: &str) -> bool;

    /// Parse the section of the source, only consuming part of the source related to this section.
    ///
    /// # Why box?
    /// Must return a boxed future to be dyn compatible.
    fn parse_section<'s>(
        &'s self,
        context: &'s Self::Context,
        state: &'s mut Self::State, 
        source: Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> BoxFuture<'s, Result<(), Self::Error>>;

    /// Function which is run when the next section to be parsed is never encountered before the end of the file.
    fn on_unexpected_end_of_file<'s>(
        &'s self,
        context: &'s Self::Context,
        state: &'s mut Self::State, 
        source_info: &'s SourceInfo,
        line_num: usize,
    ) -> BoxFuture<'s, Result<(), Self::Error>> {
        _ = (context, state, line_num, source_info);
        async move { Ok(()) }.boxed()
    }
}

impl<T, R> DynAsyncSectionParser<R> for T
where
    R: tokio::io::AsyncBufRead,
    T: AsyncSectionParser<R>,
{
    type Context = <T as AsyncSectionParser<R>>::Context;

    type Error = <T as AsyncSectionParser<R>>::Error;

    type Output = <T as AsyncSectionParser<R>>::Output;

    type State = <T as AsyncSectionParser<R>>::State;

    fn can_start_parse(&self, context: &Self::Context, state: &mut Self::State, line: &str) -> bool {
        <T as AsyncSectionParser<R>>::can_start_parse(self, context, state, line)
    }

    fn parse_section<'s>(
        &'s self,
        context: &'s Self::Context,
        state: &'s mut Self::State, 
        source: Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> BoxFuture<'s, Result<(), Self::Error>> {
        <T as AsyncSectionParser<R>>::parse_section(&self, context, state, source, output).boxed()
    }
    fn on_unexpected_end_of_file<'s>(
        &'s self,
        context: &'s Self::Context,
        state: &'s mut Self::State, 
        source_info: &'s SourceInfo,
        line_num: usize,
    ) -> BoxFuture<'s, Result<(), Self::Error>> {
        <T as AsyncSectionParser<R>>::on_unexpected_end_of_file(
            self,
            context,
            state,
            source_info,
            line_num,
        )
        .boxed()
    }
}
