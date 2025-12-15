use std::{collections::HashMap, future::Future, num::ParseFloatError, sync::Arc};

use diagnostics::{
    components::snippet::{
        Footer, FooterKind, LineHighlight, LineHighlightTheme, Snippet, SnippetChunk, SnippetLine,
        SnippetLineKind,
    },
    diagnostics::AsyncDiagnostics,
    error, warn, Log, LogComponent,
};
use state_parse::r#async::{AsyncParseSource, AsyncSectionParser, SourceInfo};
use unicode_width::UnicodeWidthStr;

use crate::{
    config::{PartialColumn, SingleState, SingleTransition, Units},
    extract::{ExtractResultsError, TransitionsSet},
    results::SingleEnergyResults,
    util::ColumnSlice,
};

#[derive(Default, Debug, Clone)]
pub struct PartialCsContext {
    pub partial_wave: u32,
    pub column: PartialColumn,
    pub units: Units,
    pub transitions_set: Arc<TransitionsSet>,
}

#[derive(Default, Debug, Clone)]
pub struct PartialCsState {
    units_from: Option<Units>,
}

pub struct ParseUnits;

impl<R> AsyncSectionParser<R> for ParseUnits
where
    R: tokio::io::AsyncBufRead + Send,
{
    type Context = (Arc<PartialCsContext>, Arc<AsyncDiagnostics>);
    type Error = ExtractResultsError;
    type Output = SingleEnergyResults;
    type State = PartialCsState;
    fn can_start_parse(
        &self,
        _context: &Self::Context,
        _state: &mut Self::State,
        _line: &str,
    ) -> bool {
        // // If a check is really needed, somewhat redundant since it should be the first line in the file anyways.
        // line.split_ascii_whitespace().nth(3) == Some("Units:")
        true
    }
    fn parse_section<'s>(
        &'s self,
        _context: &'s Self::Context,
        state: &'s mut Self::State,
        mut source: std::pin::Pin<&'s mut AsyncParseSource<R>>,
        _output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        async move {
            let line = source.next().await.unwrap().unwrap(); // Should be guaranteed to be valid if it satisfies [`Self::can_start_parse`]
            let units_str = match line.split_ascii_whitespace().nth(4) {
                Some(units_str) => units_str,
                None => {
                    return Err(ExtractResultsError::ParseUnits {
                        file_path: source
                            .info()
                            .file_path
                            .clone()
                            .unwrap_or("<unknown>".to_owned()),
                        line: line,
                        line_num: source.current_line_num(),
                        failed_units_str: "<not-found>".to_string(),
                    })
                }
            };

            let units = match Units::from_str(units_str) {
                Some(units) => units,
                None => {
                    return Err(ExtractResultsError::ParseUnits {
                        file_path: source
                            .info()
                            .file_path
                            .clone()
                            .unwrap_or("<unknown>".to_owned()),
                        failed_units_str: units_str.to_owned(),
                        line: line,
                        line_num: source.current_line_num(),
                    })
                }
            };
            state.units_from = Some(units);
            Ok(())
        }
    }

    fn on_unexpected_end_of_file<'s>(
        &'s self,
        _context: &'s Self::Context,
        _state: &'s mut Self::State,
        source_info: &'s SourceInfo,
        line_num: usize,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        async move {
            Err(ExtractResultsError::UnexpectedEndOfFile {
                file_path: source_info
                    .file_path
                    .clone()
                    .unwrap_or("<unknown>".to_owned()),
                line_num,
            })
        }
    }
}

pub struct ParseEnergy;

impl<R> AsyncSectionParser<R> for ParseEnergy
where
    R: tokio::io::AsyncBufRead + Send,
{
    type Context = (Arc<PartialCsContext>, Arc<AsyncDiagnostics>);
    type Error = ExtractResultsError;
    type Output = SingleEnergyResults;
    type State = PartialCsState;
    fn can_start_parse(
        &self,
        _context: &Self::Context,
        _state: &mut Self::State,
        _line: &str,
    ) -> bool {
        // // If a check is really needed...
        // let mut columns = line.split_ascii_whitespace();
        // columns.nth(1) == Some("eV") && columns.next() == Some("on")

        true // I want this to be scanning the next line after ParseUnit is done with the first
    }
    fn parse_section<'s>(
        &'s self,
        _context: &'s Self::Context,
        _state: &'s mut Self::State,
        mut source: std::pin::Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        async move {
            let line = source.next().await.unwrap().unwrap(); // Should be guaranteed to be valid if it satisfies [`Self::can_start_parse`]

            let energy_str = match line.split_ascii_whitespace().next() {
                Some(energy_str) => energy_str,
                None => {
                    return Err(ExtractResultsError::InvalidLine {
                        file_path: source
                            .info()
                            .file_path
                            .clone()
                            .unwrap_or("<unknown>".to_owned()),
                        line,
                        line_num: source.current_line_num(),
                        context: "Expected more columns.".to_owned(),
                    })
                }
            };

            let energy = match energy_str.parse() {
                Ok(energy) => energy,
                Err(err) => {
                    return Err(ExtractResultsError::ParseEnergy {
                        file_path: source
                            .info()
                            .file_path
                            .clone()
                            .unwrap_or("<unknown>".to_owned()),
                        failed_energy_str: energy_str.to_owned(),
                        line,
                        line_num: source.current_line_num(),
                        err,
                    })
                }
            };

            output.energy = energy;
            Ok(())
        }
    }

    fn on_unexpected_end_of_file<'s>(
        &'s self,
        _context: &'s Self::Context,
        _state: &'s mut Self::State,
        source_info: &'s SourceInfo,
        line_num: usize,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        async move {
            Err(ExtractResultsError::UnexpectedEndOfFile {
                file_path: source_info
                    .file_path
                    .clone()
                    .unwrap_or("<unknown>".to_owned()),
                line_num,
            })
        }
    }
}

pub struct ParseIonCs;

impl<R> AsyncSectionParser<R> for ParseIonCs
where
    R: tokio::io::AsyncBufRead + Send,
{
    type Context = (Arc<PartialCsContext>, Arc<AsyncDiagnostics>);
    type Error = ExtractResultsError;
    type Output = SingleEnergyResults;
    type State = PartialCsState;
    fn can_start_parse(
        &self,
        (context, _diagnostics): &Self::Context,
        _state: &mut Self::State,
        line: &str,
    ) -> bool {
        // Example line
        //   0  9.539E+01eV on t2P TNBCS,+extra, spin asymmetry:  3.243E-01  1.522E+01 -7.574E-02
        // Must start with partial wave number
        if !line.starts_with(&format!("{:>3}", context.partial_wave)) {
            return false;
        }

        // Identify the start of this section with "TNBCS,+extra, spin asymmetry:"
        line.slice_columns(4..=6)
            .map(|slice| slice == "TNBCS,+extra, spin asymmetry:")
            .unwrap_or(false)
    }
    fn parse_section<'s>(
        &'s self,
        (context, diagnostics): &'s Self::Context,
        parse_state: &'s mut Self::State,
        mut source: std::pin::Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        async move {
            // Find line containing TICS
            // Example Line
            // J=  0 1.000E+02eV on s2S TICS, +extra, spin asym:  2.172E-02  1.415E+00  1.000E+00

            let line = loop {
                let Some(line) = source.peek_or_err().await? else {
                    // Recoverable error, this only means no ionisation cross-section
                    diagnostics.write_log_background(
                        warn!("Missing total ionization cross-section (TICS) line, no TICS value extracted.")
                    );
                    return Ok(());
                };

                // Check is TICS line.
                if line.split_whitespace().nth(3) == Some("on")
                    && line.slice_columns(5..=8) == Some("TICS, +extra, spin asym:")
                {
                    // Do not call next, this is also the start of the single cross-sections
                    break line.clone();
                }

                source
                    .next()
                    .await
                    .expect("This shouldn't fail since we have peeked it.");
            };

            let state_str = match line.split_whitespace().nth(4) {
                Some(state_str) => state_str,
                None => {
                    diagnostics.write_log_background(invalid_ion_columns_log(
                        source.info().file_path.clone(),
                        source.current_line_num(),
                        line,
                    ));
                    // Not fatal, let the parser extract other cross-sections
                    return Ok(());
                }
            };

            let state = match SingleState::from_str(state_str) {
                Ok(state) => state,
                Err(err) => {
                    diagnostics.write_log_background(
                        error!("Invalid state")
                            .with_component(LogComponent::Snippet(
                                Snippet::empty(source.info().file_path.clone()).with_chunk(
                                    SnippetChunk::empty(source.current_line_num()).with_line(
                                        SnippetLine::new(line.clone(), SnippetLineKind::Normal)
                                            .with_highlight_auto(state_str, err, LineHighlightTheme::ERROR),
                                    ))
                                .with_footer(Footer::new(
                                    "Example valid line `  0 1.000E+02eV on s2S  TICS(0,0): 2.19E-02 = 5.65E-03 + 5.90E-03 + 8.00E-03 + 1.82E-03 + 4.94E-04 + 4.28E-05`".to_owned(), 
                                    FooterKind::Note
                                ))
                                .with_footer(Footer::new(
                                    "The program searches for a line starting with the specified partial wave number and TICS in the 5th column".to_owned(), 
                                    FooterKind::Note
                                ))
                            ))
                        );
                    // Not fatal, let the parser extract other cross-sections
                    return Ok(());
                }
            };

            let tics_str = match line.split_whitespace().nth(9) {
                Some(tics_str) => tics_str,
                None => {
                    diagnostics.write_log_background(
                        error!("Failed to extract total ionization cross-section (TICS)")
                            .with_component(LogComponent::Snippet(
                                Snippet::empty(source.info().file_path.clone()).with_chunk(
                                    SnippetChunk::empty(source.current_line_num()).with_line(
                                        SnippetLine::new_with_highlight(
                                            line.clone(),
                                            SnippetLineKind::Normal,
                                            Some(LineHighlight::new(
                                                line.len(),
                                                2,
                                                "Expected more whitespace separated columns"
                                                    .to_owned(),
                                                LineHighlightTheme::INFO,
                                            )),
                                        ),
                                    ),
                                )
                                .with_footer(Footer::new(
                                    "Example valid line `  0 1.000E+02eV on s2S  TICS(0,0): 2.19E-02 = 5.65E-03 + 5.90E-03 + 8.00E-03 + 1.82E-03 + 4.94E-04 + 4.28E-05`".to_owned(), 
                                    FooterKind::Note
                                ))
                                .with_footer(Footer::new(
                                    "The program searches for a line starting with the specified partial wave number and TICS in the 5th column".to_owned(), 
                                    FooterKind::Note
                                ))
                            )),
                    );
                    // Not fatal, let the parser extract other cross-sections
                    return Ok(());
                }
            };

            let mut tics = match tics_str.parse::<f64>() {
                Ok(tics) => tics,
                Err(err) => {
                    diagnostics.write_log_background(
                        error!("Failed to parse cross-section")
                            .with_component(LogComponent::Snippet(
                                Snippet::empty(source.info().file_path.clone()).with_chunk(
                                    SnippetChunk::empty(source.current_line_num()).with_line(
                                        SnippetLine::new(line.clone(), SnippetLineKind::Normal)
                                            .with_highlight_auto(tics_str, err.to_string(), LineHighlightTheme::ERROR),
                                    ),
                                )
                                .with_footer(Footer::new(
                                    "Example valid line `  0 1.000E+02eV on s2S  TICS(0,0): 2.19E-02 = 5.65E-03 + 5.90E-03 + 8.00E-03 + 1.82E-03 + 4.94E-04 + 4.28E-05`".to_owned(), 
                                    FooterKind::Note
                                ))
                                .with_footer(Footer::new(
                                    "The program searches for a line starting with the specified partial wave number and TICS in the 5th column".to_owned(), 
                                    FooterKind::Note
                                ))
                            )),
                    );
                    // Not fatal, let the parser extract other cross-sections
                    return Ok(());
                }
            };

            if tics == 0.0 {
                return Ok(());
            }

            tics = tics * Units::conversion_factor(parse_state.units_from.unwrap(), context.units);

            output.to_ion_cross_sections.insert(state, tics);

            Ok(())
        }
    }

    fn on_unexpected_end_of_file<'s>(
        &'s self,
        (context, diagnostics): &'s Self::Context,
        _state: &'s mut Self::State,
        source_info: &'s SourceInfo,
        _line_num: usize,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        async move {
            diagnostics.write_log_background(warn!(
                "No valid J={} partial cross-sections found in `{}`",
                context.partial_wave,
                source_info.file_path.as_deref().unwrap_or("<unknown>")
            ));
            Ok(())
        }
    }
}

pub struct ParseCs;

impl<R> AsyncSectionParser<R> for ParseCs
where
    R: tokio::io::AsyncBufRead + Send,
{
    type Context = (Arc<PartialCsContext>, Arc<AsyncDiagnostics>);
    type Error = ExtractResultsError;
    type Output = SingleEnergyResults;
    type State = PartialCsState;
    fn can_start_parse(
        &self,
        (context, _diagnostics): &Self::Context,
        _state: &mut Self::State,
        line: &str,
    ) -> bool {
        // Example line
        // J=  0 9.000E+00eV on s2S TICS, +extra, spin asym:  0.000E+00  0.000E+00  0.000E+00
        line.starts_with("J=")
            && if line.len() > 3 {
                line[2..]
                    .split_ascii_whitespace()
                    .next()
                    .map(|partial_wave_str| {
                        partial_wave_str
                            .parse()
                            .ok()
                            .map(|partial_wave: u32| partial_wave == context.partial_wave)
                            .unwrap_or(false)
                    })
                    .unwrap_or(false)
            } else {
                false
            }
    }
    fn parse_section<'s>(
        &'s self,
        (context, diagnostics): &'s Self::Context,
        state: &'s mut Self::State,
        mut source: std::pin::Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        async move {
            _ = source.next().await; // Skip the header lines
            _ = source.next().await;
            let mut transitions_order_counter = HashMap::new();

            let header = format!("{:>3}", context.partial_wave);
            let mut extracted_count = 0; // Number of columns extracted
            loop {
                // Example line
                //  J   trans    cross section    extrap      PCS(V)       PCS(T) S=0   PCS(T) S=1   energy     ovlp  ip
                //   0 s2S <-s2S  3.85861E-17  1.23754E-16  1.22769E-16  3.85861E-17  0.00000E+00 -1.84830E+00  1.0000 0
                let line = match source
                    .next()
                    .await?
                    .filter(|line| line.starts_with(&header))
                {
                    Some(line) => line,
                    None => break,
                };

                let mut columns = line.split_ascii_whitespace();
                columns.next(); // Skip this one, we already know the partial wave number

                // Next two columns example s2P <-s2S, need to get rid of the arrow
                let Some((to, from)) = columns.next().zip(columns.next()) else {
                    diagnostics.write_log_background(invalid_transition_columns_log(
                        source.info().file_path.clone(),
                        source.current_line_num(),
                        line.to_owned(),
                    ));
                    continue;
                };

                let Some(mut transition) = SingleTransition::new(to, from.trim_start_matches("<-")).ok() else {
                    diagnostics.write_log_background(invalid_transition_log(
                        source.info().file_path.clone(),
                        source.current_line_num(),
                        line.to_owned(),
                        line.slice_columns(1..=2).unwrap(),
                    ));
                    continue;
                };

                // Checking for repetition 'order'
                // I will be assuming only the order of the 'to' state changes.
                let order = transitions_order_counter
                    .entry(transition.clone())
                    .or_insert(0);
                transition.to.n.set_order(*order);
                *order += 1;

                // Only process transitions we are interested in.
                if !context.transitions_set.single.contains(&transition) {
                    continue;
                }

                // Go to the column selected by the context.
                for _ in 3..context.column.col_num() {
                    columns.next();
                }

                let Some(cs_str) = columns.next() else {
                    diagnostics.write_log_background(error!("{}", ExtractResultsError::InvalidLine { 
                        file_path: source.info().file_path.clone().unwrap_or_else(|| "<unknown>".to_owned()),
                        line: line.to_owned(),
                        line_num: source.current_line_num(),
                        context: "Extracting cross-section, line should look like '  0 S3P <-s2S  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00  0.00000E+00 -1.21827E-01  1.0000 0'".to_owned(), 
                    }));
                    continue;
                };

                let mut cs = match cs_str.parse::<f64>() {
                    Ok(cs) => cs,
                    Err(err) => {
                        diagnostics.write_log_background(invalid_crosssection_log(
                            source.info().file_path.clone(),
                            source.current_line_num(),
                            line.to_owned(),
                            cs_str,
                            err,
                        ));

                        continue;
                    }
                };

                if cs == 0.0 {
                    continue;
                }

                cs = cs * Units::conversion_factor(state.units_from.unwrap(), context.units);

                output.cross_sections.insert(transition, cs);
                extracted_count += 1;

                // Each file should only have one result for each transition.
                if extracted_count >= context.transitions_set.single.len() {
                    break;
                }
            }
            Ok(())
        }
    }

    fn on_unexpected_end_of_file<'s>(
        &'s self,
        (context, diagnostics): &'s Self::Context,
        _state: &'s mut Self::State,
        source_info: &'s SourceInfo,
        _line_num: usize,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        async move {
            diagnostics.write_log_background(warn!(
                "No valid J={} partial cross-sections found in `{}`",
                context.partial_wave,
                source_info.file_path.as_deref().unwrap_or("<unknown>")
            ));
            Ok(())
        }
    }
}

fn invalid_crosssection_log(
    file_path: Option<String>,
    line_num: usize,
    line: String,
    failed_cs_str: &str,
    err: ParseFloatError,
) -> Log {
    error!("Invalid cross-section `{}`", failed_cs_str).with_component(LogComponent::Snippet(
        Snippet::empty(file_path)
            .with_chunk(
                SnippetChunk::empty(line_num).with_line(SnippetLine::new_with_highlight(
                    line.to_owned(),
                    SnippetLineKind::Normal,
                    LineHighlight::new_auto(
                        &line,
                        &failed_cs_str,
                        format!("{}", err),
                        LineHighlightTheme::ERROR,
                    ),
                )),
            )
            .with_footer(Footer::new(
                "Example valid cross-section `7.04231E-17`".to_owned(),
                FooterKind::Note,
            )),
    ))
}

fn invalid_ion_columns_log(file_path: Option<String>, line_num: usize, line: String) -> Log {
    let footer_1 = Footer::new(
        "Example valid line `  0 1.000E+02eV on s2S  TICS(0,0): 2.19E-02 = 5.65E-03 + 5.90E-03 + 8.00E-03 + 1.82E-03 + 4.94E-04 + 4.28E-05`".to_owned(),
        FooterKind::Note
    );
    let footer_2 = Footer::new(
        "The program searches for a line starting with the specified partial wave number and TICS in the 5th column".to_owned(),
        FooterKind::Note
    );
    let snippet = Snippet::empty(file_path)
        .with_chunk(
            SnippetChunk::empty(line_num).with_line(SnippetLine::new_with_highlight(
                line.to_owned(),
                SnippetLineKind::Normal,
                Some(LineHighlight::new(
                    line.len(),
                    2,
                    "Expected more whitespace separated columns".to_owned(),
                    LineHighlightTheme::INFO,
                )),
            )),
        )
        .with_footer(footer_1)
        .with_footer(footer_2);
    error!("Failed to extract total ionization cross-section (TICS)")
        .with_component(LogComponent::Snippet(snippet))
}

// Rather chunky so extract out
fn invalid_transition_log(
    file_path: Option<String>,
    line_num: usize,
    line: String,
    transition: &str,
) -> Log {
    let snippet = Snippet::empty(file_path)
        .with_chunk(
            SnippetChunk::empty(line_num).with_line(SnippetLine::new_with_highlight(
                line.to_owned(),
                SnippetLineKind::Normal,
                LineHighlight::new_auto(
                    &line,
                    &transition,
                    "invalid transition".to_owned(),
                    LineHighlightTheme::ERROR,
                ),
            )),
        )
        .with_footer(Footer::new(
            "Example valid transition `s2P <-s2S`".to_owned(),
            FooterKind::Note,
        ));
    error!("Invalid transition `{}`", transition).with_component(LogComponent::Snippet(snippet))
}

// Rather chunky so extract out
fn invalid_transition_columns_log(file_path: Option<String>, line_num: usize, line: String) -> Log {
    let snippet = Snippet::empty(file_path)
        .with_chunk(
            SnippetChunk::empty(line_num).with_line(SnippetLine::new_with_highlight(
                line.to_owned(),
                SnippetLineKind::Normal,
                Some(LineHighlight::new(
                    line.width(),
                    1,
                    "expected more columns".to_owned(),
                    LineHighlightTheme::INFO,
                )),
            )),
        )
        .with_footer(Footer::new(
            "Example valid transition `s2P <-s2S`".to_owned(),
            FooterKind::Note,
        ))
        .with_footer(Footer::new(
            "A space is expected, `s2P<-s2S` is invalid".to_owned(),
            FooterKind::Note,
        ));
    error!("Unexpected end of line while parsing transition")
        .with_component(LogComponent::Snippet(snippet))
}
