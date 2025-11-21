use std::{future::Future, num::ParseFloatError, pin::Pin, sync::Arc};

use diagnostics::{
    diagnostics::{AsyncDiagnostics, ChannelAllow},
    info, LogFmtOptions,
};
use futures::pin_mut;
use state_parse::r#async::{
    compose::AsyncSectionParserBuilder, AsyncParseSource, AsyncParser, AsyncSectionParser,
    DefaultAsyncParserConfig, SourceInfo,
};
use tokio::io::{AsyncBufRead, BufReader};

#[derive(Debug)]
pub enum Units {
    Atomic,
    CentimetreSquared,
    MetreSquared,
}

impl Units {
    pub fn from_str(str: &str) -> Option<Self> {
        Some(match str {
            "a0^2" => Self::Atomic,
            "cm^2" => Self::CentimetreSquared,
            "m^2" => Self::MetreSquared,
            _ => return None,
        })
    }
}

#[derive(Debug)]
pub enum MyError {
    ParseFloat(ParseFloatError),
    UnexpectedEndOfLine,
    Io(std::io::Error),
}

impl From<ParseFloatError> for MyError {
    fn from(value: ParseFloatError) -> Self {
        Self::ParseFloat(value)
    }
}

impl From<std::io::Error> for MyError {
    fn from(value: std::io::Error) -> Self {
        Self::Io(value)
    }
}

#[derive(Debug)]
pub struct ParseOutput {
    units: Units,
    cs: Vec<f32>,
}

pub struct ParseUnits;

impl<R> AsyncSectionParser<R> for ParseUnits
where
    R: AsyncBufRead + Send,
{
    type Output = Units;
    type Error = MyError;
    type Context = ();
    type State = ();
    fn can_start_parse(
        &self,
        _context: &Self::Context,
        _state: &mut Self::State,
        line: &str,
    ) -> bool {
        line.split_ascii_whitespace()
            .nth(3)
            .map(|word| word == "Units:")
            .unwrap_or(false)
    }

    fn parse_section<'s>(
        &'s self,
        _context: &'s Self::Context,
        _state: &'s mut Self::State,
        mut stream: Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + 's {
        async move {
            let line = stream.next().await?.unwrap();

            let mut columns = line.split_ascii_whitespace();

            for _ in 0..4 {
                columns.next();
            }

            let Some(unit_str) = columns.next() else {
                return Err(MyError::UnexpectedEndOfLine);
            };

            match Units::from_str(unit_str) {
                Some(unit) => *output = unit,
                None => return Err(MyError::UnexpectedEndOfLine),
            }

            Ok(())
        }
    }
}

pub struct ParseCs;

impl<R> AsyncSectionParser<R> for ParseCs
where
    R: AsyncBufRead + Send,
{
    type Output = Vec<f32>;
    type Error = MyError;
    type Context = Arc<AsyncDiagnostics>;
    type State = ();
    fn can_start_parse(
        &self,
        _context: &Self::Context,
        _state: &mut Self::State,
        line: &str,
    ) -> bool {
        line.starts_with("J=  0")
    }

    fn parse_section<'s>(
        &'s self,
        context: &'s Self::Context,
        _state: &'s mut Self::State,
        mut stream: Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + 's {
        async move {
            // Skip the header
            let _ = stream.next().await;
            let _ = stream.next().await;

            loop {
                if !stream
                    .as_mut()
                    .peek_or_err()
                    .await?
                    .map(|line| line.starts_with("  0"))
                    .unwrap_or(false)
                {
                    break;
                }
                let line = stream.next().await?.unwrap();

                let mut columns = line.split_ascii_whitespace();

                _ = columns.next();

                let to = columns.next().ok_or(MyError::UnexpectedEndOfLine)?;
                let from = columns.next().ok_or(MyError::UnexpectedEndOfLine)?;

                if to == "s2S" && from == "<-s2S" {
                    output.push(
                        columns
                            .next()
                            .ok_or(MyError::UnexpectedEndOfLine)?
                            .parse()?,
                    )
                } else {
                    context.write_log_background(info!("Ignored transition {} {}", to, from));
                }
            }

            Ok(())
        }
    }
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let mut diagnostics = AsyncDiagnostics::new(LogFmtOptions::default());
    diagnostics.add_channel(ChannelAllow::all(), tokio::io::stdout());

    let diagnostics = Arc::new(diagnostics);

    let mut file = BufReader::new(
        tokio::fs::OpenOptions::new()
            .read(true)
            .open("totalcs_1.0000E+02_J")
            .await
            .unwrap(),
    );

    // Parser with default io error handling behaviour
    let mut parser = AsyncParser::empty(DefaultAsyncParserConfig::new());

    // Output with default values
    let mut output = ParseOutput {
        units: Units::CentimetreSquared,
        cs: Vec::new(),
    };

    parser.add_section(
        ParseUnits
            .map_context(|_| &())
            .map_output(|output: &mut ParseOutput| &mut output.units),
    );
    parser.add_section(ParseCs.map_output(|output: &mut ParseOutput| &mut output.cs));

    let file_source = AsyncParseSource::new(
        &mut file,
        SourceInfo {
            file_path: Some("totalcs_1.0000E+02_J".to_owned()),
        },
    );
    pin_mut!(file_source);

    parser
        .parse(&diagnostics, file_source, &mut output)
        .await
        .unwrap();

    println!("{:?}", output);
}
