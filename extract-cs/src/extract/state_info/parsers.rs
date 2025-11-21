use std::{
    collections::{HashMap, HashSet},
    future::Future,
    sync::Arc,
};

use diagnostics::{
    components::snippet::{
        LineHighlightTheme, Snippet, SnippetChunk, SnippetLine, SnippetLineKind,
    },
    diagnostics::AsyncDiagnostics,
    error, LogComponent,
};
use state_parse::r#async::AsyncSectionParser;

use crate::{
    config::SingleState,
    extract::state_info::{ExtractStateInfoError, StateInfo},
};

pub struct StateInfoContext {
    pub states_set: Arc<HashSet<SingleState>>,
}

pub struct ParseStateInfo;

impl<R> AsyncSectionParser<R> for ParseStateInfo
where
    R: tokio::io::AsyncBufRead + Send,
{
    type Context = (Arc<StateInfoContext>, Arc<AsyncDiagnostics>);
    type Error = ExtractStateInfoError;
    type Output = HashMap<SingleState, StateInfo>;
    type State = ();

    fn can_start_parse(
        &self,
        _context: &Self::Context,
        _state: &mut Self::State,
        _line: &str,
    ) -> bool {
        true // Just parses the entire file
    }
    fn on_unexpected_end_of_file<'s>(
        &'s self,
        _context: &'s Self::Context,
        _state: &'s mut Self::State,
        _source_info: &'s state_parse::r#async::SourceInfo,
        _line_num: usize,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        async move { Ok(()) } // Would only happen if the file is empty
    }

    fn parse_section<'s>(
        &'s self,
        (context, diagnostics): &'s Self::Context,
        _state: &'s mut Self::State,
        mut source: std::pin::Pin<&'s mut state_parse::r#async::AsyncParseSource<R>>,
        output: &'s mut Self::Output,
    ) -> impl Future<Output = Result<(), Self::Error>> + Send + 's {
        async move {
            let mut transitions_order_counter = HashMap::new();
            loop {
                let line = match source.next().await? {
                    Some(line) => line,
                    None => return Ok(()),
                };

                let mut columns = line.split_whitespace();

                let first = match columns.next() {
                    Some(first) => first,
                    None => continue,
                };

                // Lines with state information begin with + or -
                if !(first == "+" || first == "-") {
                    continue;
                }

                // Skip next two columns
                columns.nth(1);

                let state_str = match columns.next() {
                    Some(state_str) => state_str,
                    None => continue,
                };

                // This means the state was generated but not included in the calculation, shouldn't be useful anyways.
                if state_str == "---" {
                    continue;
                }

                let mut state = match SingleState::from_str(state_str) {
                    Ok(state) => state,
                    Err(err) => {
                        let snippet = Snippet::empty(source.info().file_path.clone()).with_chunk(
                            SnippetChunk::empty(source.current_line_num())
                                .with_line(SnippetLine::new(line.clone(), SnippetLineKind::Normal))
                                .with_highlight_auto(
                                    source.current_line_num(),
                                    state_str,
                                    format!("{}", err),
                                    LineHighlightTheme::ERROR,
                                ),
                        );
                        diagnostics.write_log_background(
                            error!("Failed to parse state `{}`", state_str)
                                .with_component(LogComponent::Snippet(snippet)),
                        );
                        continue;
                    }
                };

                let order = transitions_order_counter.entry(state.clone()).or_insert(0);
                state.n.set_order(*order);
                *order += 1;

                if !context.states_set.contains(&state) {
                    continue;
                }

                // Skip next three columns
                columns.nth(2);

                let energy_str = match columns.next() {
                    Some(energy_str) => energy_str,
                    None => continue,
                };

                let energy = match energy_str.parse::<f64>() {
                    Ok(energy) => energy,
                    Err(err) => {
                        let snippet = Snippet::empty(source.info().file_path.clone()).with_chunk(
                            SnippetChunk::empty(source.current_line_num())
                                .with_line(SnippetLine::new(line.clone(), SnippetLineKind::Normal))
                                .with_highlight_auto(
                                    source.current_line_num(),
                                    energy_str,
                                    format!("{}", err),
                                    LineHighlightTheme::ERROR,
                                ),
                        );
                        diagnostics.write_log_background(
                            error!("Failed to parse energy `{}`", energy_str)
                                .with_component(LogComponent::Snippet(snippet)),
                        );
                        continue;
                    }
                };

                output.insert(state, StateInfo { energy });
            }
        }
    }
}
