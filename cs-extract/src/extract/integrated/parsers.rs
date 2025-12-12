use std::{collections::HashMap, future::Future, num::ParseFloatError, sync::Arc};

use diagnostics::{
    components::snippet::{
        Footer, FooterKind, LineHighlight, LineHighlightTheme, Snippet, SnippetChunk, SnippetLine,
        SnippetLineKind,
    },
    diagnostics::AsyncDiagnostics,
    error, info, warn, Log, LogComponent,
};
use state_parse::r#async::{AsyncParseSource, AsyncSectionParser, SourceInfo};
use tokio::io::AsyncBufRead;
use unicode_width::UnicodeWidthStr;

use crate::{
    config::{SingleState, SingleTransition, Units},
    extract::{ExtractResultsError, TransitionsSet},
    results::SingleEnergyResults,
    util::ColumnSlice,
};

pub struct IntegratedCsContext {
    pub units: Units,
    pub transitions_set: Arc<TransitionsSet>,
    pub extrapolated: bool,
}

#[derive(Default)]
pub struct IntegratedCsState {
    pub units_from: Option<Units>,
}

pub struct ParseEnergyUnits;

impl<R> AsyncSectionParser<R> for ParseEnergyUnits
where
    R: AsyncBufRead + Send,
{
    type Context = ();
    type Error = ExtractResultsError;
    type Output = f64;
    type State = Option<Units>;
    fn can_start_parse(
        &self,
        _context: &Self::Context,
        _state: &mut Self::State,
        _line: &str,
    ) -> bool {
        true // First line in the file
    }
    fn parse_section<'s>(
        &'s self,
        _context: &'s Self::Context,
        state: &'s mut Self::State,
        mut source: std::pin::Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        // Example line:
        //    0 866   1 Units: a0^2     electron - B II    1.0000E+02 eV on s2S
        async move {
            let line = source.next().await.unwrap().unwrap(); // Should be guaranteed by can_start_parse
            let mut columns = line.split_ascii_whitespace();

            let units_str = match columns.nth(4) {
                Some(units_str) => units_str,
                None => {
                    return Err(ExtractResultsError::InvalidLine {
                        file_path: source
                            .info()
                            .file_path
                            .clone()
                            .unwrap_or_else(|| "<unknown>".to_owned()),
                        line,
                        line_num: source.current_line_num(),
                        context: "Expected more whitespace separated columns".to_owned(),
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
                            .unwrap_or_else(|| "<unknown>".to_owned()),
                        failed_units_str: units_str.to_owned(),
                        line,
                        line_num: source.current_line_num(),
                    })
                }
            };

            *state = Some(units);

            let energy_str: &str = match columns.nth(4) {
                Some(energy_str) => energy_str,
                None => {
                    return Err(ExtractResultsError::InvalidLine {
                        file_path: source
                            .info()
                            .file_path
                            .clone()
                            .unwrap_or_else(|| "<unknown>".to_owned()),
                        line,
                        line_num: source.current_line_num(),
                        context: "Expected more whitespace separated columns".to_owned(),
                    })
                }
            };

            let energy = match energy_str.parse::<f64>() {
                Ok(energy) => energy,
                Err(err) => {
                    return Err(ExtractResultsError::ParseEnergy {
                        file_path: source
                            .info()
                            .file_path
                            .clone()
                            .unwrap_or_else(|| "<unknown>".to_owned()),
                        failed_energy_str: energy_str.to_owned(),
                        line,
                        line_num: source.current_line_num(),
                        err,
                    })
                }
            };

            *output = energy;
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

pub struct ParseBornCs;

impl<R> AsyncSectionParser<R> for ParseBornCs
where
    R: AsyncBufRead + Send,
{
    type Context = (Arc<IntegratedCsContext>, Arc<AsyncDiagnostics>);
    type Error = ExtractResultsError;
    type Output = SingleEnergyResults;
    type State = IntegratedCsState;

    fn can_start_parse(
        &self,
        context: &Self::Context,
        state: &mut Self::State,
        line: &str,
    ) -> bool {
        // Example line to look for
        //   transition   BornPCS        BornICS      last PCS(V)    last PCS(T) canstop
        line.starts_with(
            "  transition   BornPCS        BornICS      last PCS(V)    last PCS(T) canstop",
        )
    }

    fn parse_section<'s>(
        &'s self,
        (context, diagnostics): &'s Self::Context,
        state: &'s mut Self::State,
        mut source: std::pin::Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        async move {
            let mut source = source.as_mut();

            let mut transitions_order_counter = HashMap::new();
            let mut extracted_count = 0;
            loop {
                match source.peek().await {
                    Ok(Some(line)) => {
                        let Some(transition) = line.slice_columns(0..=1) else {
                            break;
                        };
                        if !SingleTransition::is_valid_transition_str(transition) {
                            break;
                        }
                    }
                    Ok(None) => break,
                    Err(_) => {
                        // Next line will handle this.
                    }
                };

                let line = match source.next().await {
                    Ok(Some(line)) => line,
                    Ok(None) => break,
                    Err(err) => {
                        return Err(ExtractResultsError::OtherIoError {
                            file_path: source.info().file_path.clone(),
                            line: None,
                            err,
                        });
                    }
                };
                let mut columns = line.split_ascii_whitespace();

                // Next two columns example s2P <-s2S, need to get rid of the arrow
                let Some((to, from)) = columns.next().zip(columns.next()) else {
                    diagnostics.write_log_background(
                        error!("Unexpected end of line while parsing transition").with_component(
                            LogComponent::Snippet(
                                Snippet::empty(Some(
                                    source
                                        .info()
                                        .file_path
                                        .clone()
                                        .unwrap_or_else(|| "<unknown>".to_owned()),
                                ))
                                .with_chunk(
                                    SnippetChunk::empty(source.current_line_num()).with_line(
                                        SnippetLine::new_with_highlight(
                                            line.to_owned(),
                                            SnippetLineKind::Normal,
                                            Some(LineHighlight::new(
                                                line.width(),
                                                1,
                                                "expected more columns".to_owned(),
                                                LineHighlightTheme::INFO,
                                            )),
                                        ),
                                    ),
                                )
                                .with_footer(Footer::new(
                                    "Example valid transition `s2P <-s2S`".to_owned(),
                                    FooterKind::Note,
                                ))
                                .with_footer(Footer::new(
                                    "A space is expected, `s2P<-s2S` is invalid".to_owned(),
                                    FooterKind::Note,
                                )),
                            ),
                        ),
                    );
                    continue;
                };

                let Some(mut transition) = SingleTransition::new(to, &from[2..]).ok() else {
                    diagnostics.write_log_background(
                        error!("Invalid transition").with_component(LogComponent::Snippet(
                            Snippet::empty(Some(
                                source
                                    .info()
                                    .file_path
                                    .clone()
                                    .unwrap_or_else(|| "<unknown>".to_owned()),
                            ))
                            .with_chunk(SnippetChunk::empty(source.current_line_num()).with_line(
                                SnippetLine::new_with_highlight(
                                    line.to_owned(),
                                    SnippetLineKind::Normal,
                                    LineHighlight::new_auto(
                                        &line,
                                        line.slice_columns(0..=1).unwrap(),
                                        "invalid transition".to_owned(),
                                        LineHighlightTheme::ERROR,
                                    ),
                                ),
                            ))
                            .with_footer(Footer::new(
                                "Example valid transition `s2P <-s2S`".to_owned(),
                                FooterKind::Note,
                            )),
                        )),
                    );
                    continue;
                };

                // Checking for repetition 'order'
                // I will be assuming only the order of the 'to' state changes.
                let order = match transitions_order_counter.get_mut(&transition) {
                    Some(order) => {
                        *order += 1;
                        *order
                    }
                    None => {
                        transitions_order_counter.insert(transition.clone(), 0);
                        0
                    }
                };
                transition.to.n.set_order(order);

                // Only process transitions we are interested in.
                if !context.transitions_set.single.contains(&transition) {
                    continue;
                }

                // Integrated is after next column
                _ = columns.next();

                let Some(cs_str) = columns.next() else {
                    diagnostics.write_log_background(error!("{}", ExtractResultsError::InvalidLine { 
                        file_path: source.info().file_path.clone().unwrap_or_else(|| "<unknown>".to_owned()),
                        line: line.to_owned(),
                        line_num: source.current_line_num(),
                        context: "Extracting cross-section, line should look like `s2S <-s2S    1.00539E-16    1.00549E-16    1.00000E+00    1.00000E+00 -1.848E+00  1.695E+01`".to_owned(), 
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
        context: &'s Self::Context,
        state: &'s mut Self::State,
        source_info: &'s SourceInfo,
        line_num: usize,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        _ = (context, state, line_num, source_info);
        async move { Ok(()) }
    }
}

pub struct ParseIonCs;

impl<R> AsyncSectionParser<R> for ParseIonCs
where
    R: AsyncBufRead + Send,
{
    type Context = (Arc<IntegratedCsContext>, Arc<AsyncDiagnostics>);
    type Error = ExtractResultsError;
    type Output = SingleEnergyResults;
    type State = IntegratedCsState;
    fn can_start_parse(
        &self,
        _context: &Self::Context,
        _state: &mut Self::State,
        line: &str,
    ) -> bool {
        // Example line to look for
        //                     CS        summed l=    0          1          2          3          4
        line.starts_with("                    CS        summed l=")
    }

    fn parse_section<'s>(
        &'s self,
        (context, diagnostics): &'s Self::Context,
        parse_state: &'s mut Self::State,
        mut source: std::pin::Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        async move {
            // Example line to look for
            // 1.000E+00eV on s2S TICS: s, t-s, +, a  0.000E+00  0.000E+00  0.000E+00  0.000E+00

            let mut source = source.as_mut();

            // Find line containing TICS
            let line = loop {
                let Some(line) = source.peek_or_err().await? else {
                    // Recoverable error, this only means no ionisation cross-section
                    diagnostics.write_log_background(
                        warn!("Missing total ionization cross-section (TICS) line, no TICS value extracted.")
                    );
                    return Ok(());
                };

                // Check is TICS line.
                if line.slice_columns(1..=1) == Some("on")
                    && line.slice_columns(3..=7) == Some("TICS: s, t-s, +, a")
                {
                    break source.next().await.unwrap().unwrap();
                }

                source
                    .next()
                    .await
                    .expect("This shouldn't fail since we have peeked it.");
            };

            let mut columns = line.split_ascii_whitespace();
            _ = columns.next();
            _ = columns.next();

            let state_str = match columns.next() {
                Some(state_str) => state_str,
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

            let from_state = match SingleState::from_str(state_str) {
                Ok(from_state) => from_state,
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

            let tics_str = match columns.nth(5) {
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

            if tics != 0.0 {
                tics =
                    tics * Units::conversion_factor(parse_state.units_from.unwrap(), context.units);

                output.to_ion_cross_sections.insert(from_state, tics);
            }

            // Just find end of section now
            while let Some(line) = source.next().await? {
                if line == "-------------------------------------------------------------------------------" {
                    break;
                }
            }

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

pub struct ParseSingleCs;

impl<R> AsyncSectionParser<R> for ParseSingleCs
where
    R: AsyncBufRead + Send,
{
    type Context = (Arc<IntegratedCsContext>, Arc<AsyncDiagnostics>);
    type Error = ExtractResultsError;
    type Output = SingleEnergyResults;
    type State = IntegratedCsState;
    fn can_start_parse(
        &self,
        _context: &Self::Context,
        _state: &mut Self::State,
        line: &str,
    ) -> bool {
        // Line to look for, however white space may not be consistent
        //                     CS        summed l=    0          1          2          3          4
        // OR
        // transition  cross section  extrapolated     overlap       spin asym    energy     energydiff
        line.starts_with("                    CS        summed l=")
            || line.slice_columns(0..1) == Some("transition")
                && line.slice_columns(1..=2) == Some("cross section")
                && line.slice_columns(3..4) == Some("extrapolated")
    }
    fn parse_section<'s>(
        &'s self,
        (context, diagnostics): &'s Self::Context,
        state: &'s mut Self::State,
        mut source: std::pin::Pin<&'s mut AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        // Example line:
        // s2S <-s2S    1.00539E-16    1.00549E-16    1.00000E+00    1.00000E+00 -1.848E+00  1.695E+01
        async move {
            loop {
                match source.next().await? {
                    Some(line) => {
                        if line.slice_columns(0..1) == Some("transition")
                            && line.slice_columns(1..=2) == Some("cross section")
                            && line.slice_columns(3..4) == Some("extrapolated")
                        {
                            // Found the header
                            break;
                        }
                    }
                    None => {
                        diagnostics.write_log_background(error!("Header for cross-sections not found").with_sublog(info!("Finding header `transition  cross section  extrapolated     overlap       spin asym    energy     energydiff`")));
                        return Ok(());
                    }
                }
            }

            let mut transitions_order_counter = HashMap::new();

            let mut extracted_count = 0;
            loop {
                match source.peek().await {
                    Ok(Some(line)) => {
                        let Some(transition) = line.slice_columns(0..=1) else {
                            break;
                        };
                        if !SingleTransition::is_valid_transition_str(transition) {
                            break;
                        }
                    }
                    Ok(None) => break,
                    Err(_) => {
                        // Next line will handle this.
                    }
                };

                let line = match source.next().await {
                    Ok(Some(line)) => line,
                    Ok(None) => break,
                    Err(err) => {
                        return Err(ExtractResultsError::OtherIoError {
                            file_path: source.info().file_path.clone(),
                            line: None,
                            err,
                        });
                    }
                };

                let mut columns = line.split_ascii_whitespace();

                // Next two columns example s2P <-s2S, need to get rid of the arrow
                let Some((to, from)) = columns.next().zip(columns.next()) else {
                    diagnostics.write_log_background(
                        error!("Unexpected end of line while parsing transition").with_component(
                            LogComponent::Snippet(
                                Snippet::empty(Some(
                                    source
                                        .info()
                                        .file_path
                                        .clone()
                                        .unwrap_or_else(|| "<unknown>".to_owned()),
                                ))
                                .with_chunk(
                                    SnippetChunk::empty(source.current_line_num()).with_line(
                                        SnippetLine::new_with_highlight(
                                            line.to_owned(),
                                            SnippetLineKind::Normal,
                                            Some(LineHighlight::new(
                                                line.width(),
                                                1,
                                                "expected more columns".to_owned(),
                                                LineHighlightTheme::INFO,
                                            )),
                                        ),
                                    ),
                                )
                                .with_footer(Footer::new(
                                    "Example valid transition `s2P <-s2S`".to_owned(),
                                    FooterKind::Note,
                                ))
                                .with_footer(Footer::new(
                                    "A space is expected, `s2P<-s2S` is invalid".to_owned(),
                                    FooterKind::Note,
                                )),
                            ),
                        ),
                    );
                    continue;
                };

                let Some(mut transition) = SingleTransition::new(to, &from[2..]).ok() else {
                    diagnostics.write_log_background(
                        error!("Invalid transition").with_component(LogComponent::Snippet(
                            Snippet::empty(Some(
                                source
                                    .info()
                                    .file_path
                                    .clone()
                                    .unwrap_or_else(|| "<unknown>".to_owned()),
                            ))
                            .with_chunk(SnippetChunk::empty(source.current_line_num()).with_line(
                                SnippetLine::new_with_highlight(
                                    line.to_owned(),
                                    SnippetLineKind::Normal,
                                    LineHighlight::new_auto(
                                        &line,
                                        line.slice_columns(0..=1).unwrap(),
                                        "invalid transition".to_owned(),
                                        LineHighlightTheme::ERROR,
                                    ),
                                ),
                            ))
                            .with_footer(Footer::new(
                                "Example valid transition `s2P <-s2S`".to_owned(),
                                FooterKind::Note,
                            )),
                        )),
                    );
                    continue;
                };

                // Checking for repetition 'order'
                // I will be assuming only the order of the 'to' state changes.
                let order = match transitions_order_counter.get_mut(&transition) {
                    Some(order) => {
                        *order += 1;
                        *order
                    }
                    None => {
                        transitions_order_counter.insert(transition.clone(), 0);
                        0
                    }
                };
                transition.to.n.set_order(order);

                // Only process transitions we are interested in.
                if !context.transitions_set.single.contains(&transition) {
                    continue;
                }

                // Extrapolated is next column
                if context.extrapolated {
                    _ = columns.next();
                }

                let Some(cs_str) = columns.next() else {
                    diagnostics.write_log_background(error!("{}", ExtractResultsError::InvalidLine { 
                        file_path: source.info().file_path.clone().unwrap_or_else(|| "<unknown>".to_owned()),
                        line: line.to_owned(),
                        line_num: source.current_line_num(),
                        context: "Extracting cross-section, line should look like `s2S <-s2S    1.00539E-16    1.00549E-16    1.00000E+00    1.00000E+00 -1.848E+00  1.695E+01`".to_owned(), 
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
