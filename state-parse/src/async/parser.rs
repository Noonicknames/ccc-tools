use std::{future::Future, marker::PhantomData, pin::Pin};

use crate::r#async::{AsyncParseSource, AsyncSectionParser, DynAsyncSectionParser, SourceInfo};

/// A parser composed of sections which will be parsed sequentially.
pub struct AsyncParser<'a, R, Output, Error, Context, State, Config>
where
    Config: AsyncParserConfig<Context = Context, Error = Error>,
{
    sections: Vec<
        Box<
            dyn DynAsyncSectionParser<
                    R,
                    Output = Output,
                    Error = Error,
                    Context = Context,
                    State = State,
                > + Send
                + 'a,
        >,
    >,
    config: Config,
}

pub trait AsyncParserConfig {
    type Context;
    type Error;
    fn convert_io_error(
        &self,
        context: &Self::Context,
        error: std::io::Error,
        source_info: &SourceInfo,
        line_num: Option<usize>,
    ) -> impl Future<Output = Self::Error>;
}

pub struct DefaultAsyncParserConfig<C, E> {
    _phantom: PhantomData<(C, E)>,
}
impl<C, E> DefaultAsyncParserConfig<C, E> {
    pub const fn new() -> Self {
        Self {
            _phantom: PhantomData,
        }
    }
}

impl<C, E> AsyncParserConfig for DefaultAsyncParserConfig<C, E>
where
    E: From<std::io::Error>,
{
    type Context = C;
    type Error = E;
    fn convert_io_error(
        &self,
        _context: &Self::Context,
        error: std::io::Error,
        _source_info: &SourceInfo,
        _line_num: Option<usize>,
    ) -> impl Future<Output = Self::Error> {
        async { error.into() }
    }
}

impl<'a, R, Output, Error, Context, State, Config>
    AsyncParser<'a, R, Output, Error, Context, State, Config>
where
    Config: AsyncParserConfig<Context = Context, Error = Error>,
{
    /// Return an empty parser.
    ///
    /// Add parse sections with [`AsyncParser::add_section`].
    pub const fn empty(config: Config) -> Self {
        Self {
            sections: Vec::new(),
            config,
        }
    }
}

impl<'a, R, Output, Error, Context, State, Config>
    AsyncParser<'a, R, Output, Error, Context, State, Config>
where
    Config: AsyncParserConfig<Context = Context, Error = Error>,
    R: tokio::io::AsyncBufRead,
{
    pub fn add_section(
        &mut self,
        section: impl AsyncSectionParser<
                R,
                Output = Output,
                Error = Error,
                Context = Context,
                State = State,
            > + Send
            + 'a,
    ) {
        self.add_section_boxed(Box::new(section));
    }

    /// Add a parsing section without re-boxing.
    pub fn add_section_boxed(
        &mut self,
        section: Box<
            dyn DynAsyncSectionParser<
                    R,
                    Output = Output,
                    Error = Error,
                    Context = Context,
                    State = State,
                > + Send
                + 'a,
        >,
    ) {
        self.sections.push(section);
    }
}

impl<'a, R, Output, Error, Context, State, Config> AsyncParser<'a, R, Output, Error, Context, State, Config>
where
    Config: AsyncParserConfig<Context = Context, Error = Error>,
    R: tokio::io::AsyncBufRead + Send,
    Error: Send,
    Context: Sync,
    Output: Send,
    State: Send + Sync + Default,
{
    /// Parse a source, keeping default values if the parser does not change values of output.
    pub async fn parse_default(
        &self,
        context: &Context,
        source: Pin<&mut AsyncParseSource<R>>,
    ) -> Result<Output, Error>
    where
        Output: Default,
    {
        let mut output = Output::default();

        self.parse(context, source, &mut output).await?;

        Ok(output)
    }

    /// Parse a source into the output.
    pub async fn parse(
        &self,
        context: &Context,
        mut source: Pin<&mut AsyncParseSource<R>>,
        output: &mut Output,
    ) -> Result<(), Error> {
        let mut state = State::default();
        for section in self.sections.iter() {
            loop {
                let line = match source.peek_or_err().await {
                    Ok(Some(out)) => out,
                    Ok(None) => {
                        section
                            .on_unexpected_end_of_file(
                                context,
                                &mut state,
                                source.info(),
                                source.current_line_num(),
                            )
                            .await?;
                        break;
                    }
                    Err(err) => {
                        return Err(self
                            .config
                            .convert_io_error(context, err, source.info(), None)
                            .await);
                    }
                };

                if section.can_start_parse(context, &mut state, line) {
                    break;
                }

                if let Err(err) = source.next().await {
                    return Err(self
                        .config
                        .convert_io_error(
                            context,
                            err,
                            source.info(),
                            Some(source.current_line_num()),
                        )
                        .await);
                }
            }
            section
                .parse_section(context, &mut state, source.as_mut(), output)
                .await?;
        }
        Ok(())
    }
}
