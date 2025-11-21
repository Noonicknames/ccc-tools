use std::{
    collections::HashSet,
    io::Write,
    num::ParseFloatError,
    path::{Path, PathBuf},
    sync::Arc,
};

use colored::Colorize;
use diagnostics::{
    components::snippet::{
        LineHighlight, LineHighlightTheme, Snippet, SnippetChunk, SnippetLine, SnippetLineKind,
    },
    diagnostics::AsyncDiagnostics,
    error, info, warn, Log, LogComponent,
};

use futures::{stream::FuturesUnordered, StreamExt};
use tokio::io::{AsyncReadExt, AsyncWriteExt};

use crate::{
    bench_time,
    config::{Config, ConfigSerde, CsKind, ResultSet, SingleState, SingleTransition, State, Transition, Units},
    prep::{
        integrated::get_integrated_results,
        partial::get_partial_results,
        state_info::{ExtractStateInfoError, get_state_info},
    },
    results::CsResults,
    util::{FilesSource, ensure_folder_exists},
};

mod integrated;
mod partial;
mod state_info;

#[derive(Debug, thiserror::Error)]
pub enum PrepCmdError {
    #[error("Failed to open config file '{file_path}': {err}")]
    OpenConfig {
        file_path: String,
        err: std::io::Error,
    },
    #[error("Config file '{file_path}' is not valid utf8: {err}")]
    ConfigInvalidUtf8 {
        file_path: String,
        err: std::io::Error,
    },
    #[error("Failed to parse config file '{file_path}': {err}")]
    ConfigParse {
        file_path: String,
        start_line: usize,
        lines: String,
        err: ron::error::SpannedError,
    },
    #[error(transparent)]
    PrepareFoldersError(#[from] PrepareFoldersError),
}

impl PrepCmdError {
    pub fn to_log(&self) -> Log {
        match self {
            Self::ConfigParse {
                file_path,
                start_line,
                lines,
                err,
            } => error!("Failed to parse config file").with_component(LogComponent::Snippet(
                Snippet::empty(Some(file_path.clone())).with_chunk(
                    SnippetChunk::from_str(&lines, SnippetLineKind::Normal, *start_line)
                        .with_highlight(
                            err.position.line,
                            LineHighlight::new(
                                err.position.col - 1,
                                1,
                                err.to_string(),
                                LineHighlightTheme::ERROR,
                            ),
                        ),
                ),
            )),
            _ => error!("{}", self),
        }
    }
}

/// Run upon calling the 'prep' subcommand.
pub async fn cmd_prep(
    file_path: &str,
    diagnostics: &Arc<AsyncDiagnostics>,
) -> Result<(), PrepCmdError> {
    let config = get_config(file_path).await?;

    let root_dir = PathBuf::from(file_path).parent().unwrap().to_owned();

    prepare_result_sets(root_dir, config, diagnostics).await?;

    Ok(())
}

async fn get_config(file_path: impl AsRef<Path>) -> Result<Config, PrepCmdError> {
    {
        let mut open_options = tokio::fs::OpenOptions::new();
        open_options.read(true);

        let mut file = open_options.open(file_path.as_ref()).await.map_err(|err| {
            PrepCmdError::OpenConfig {
                file_path: file_path.as_ref().display().to_string(),
                err,
            }
        })?;

        let mut buf = String::new();
        file.read_to_string(&mut buf)
            .await
            .map_err(|err| PrepCmdError::ConfigInvalidUtf8 {
                file_path: file_path.as_ref().display().to_string(),
                err,
            })?;

        Ok(ron::from_str::<ConfigSerde>(&buf).map(|config| config.to_config()).map_err(|err| {
            let mut buf_lines = buf.lines();
            let start_line = err.position.line.checked_sub(1).unwrap_or(0);
            for _ in 0..start_line - 1 {
                buf_lines.next();
            }
            let mut lines = String::new();
            for _ in 0..3 {
                if let Some(line) = buf_lines.next() {
                    lines.push_str(line);
                    lines.push('\n');
                }
            }
            PrepCmdError::ConfigParse {
                file_path: file_path.as_ref().display().to_string(),
                start_line,
                lines,
                err,
            }
        })?)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ExtractResultsError {
    #[error("Failed to open '{file_path}' to extract results.\n{err}")]
    FailedOpenFilesSource {
        file_path: String,
        err: std::io::Error,
    },
    #[error("Failed to open file '{file_path}' while extracting results.\n{err}")]
    FailedOpenFile {
        file_path: String,
        err: std::io::Error,
    },
    #[error("Unexpected end of file '{file_path}' at line {line_num} while extracting results.")]
    UnexpectedEndOfFile { file_path: String, line_num: usize },
    #[error("Failed to parse energy of file '{file_path}'.\nAttempted to parse '{}' as energy\nLine '{line}'\n{err}", failed_energy_str.color(colored::Color::BrightRed))]
    ParseEnergy {
        file_path: String,
        line: String,
        line_num: usize,
        failed_energy_str: String,
        err: ParseFloatError,
    },
    #[error("Failed to parse units of file '{file_path}', attempted to parse '{}' as units on line '{line}'", failed_units_str.color(colored::Color::BrightRed))]
    ParseUnits {
        file_path: String,
        line: String,
        line_num: usize,
        failed_units_str: String,
    },
    #[error("Failed to parse a transition of file '{file_path}'.\nAttempted to parse '{}' as transition\nLine '{line}'", failed_transition_str.color(colored::Color::BrightRed))]
    ParseTransition {
        file_path: String,
        line: String,
        line_num: usize,
        failed_transition_str: String,
    },
    #[error("In file '{file_path}', failed to parse line '{line}'\n{context}")]
    InvalidLine {
        file_path: String,
        line: String,
        line_num: usize,
        context: String,
    },
    #[error(transparent)]
    StateInfo(#[from] ExtractStateInfoError),
    #[error("Other io error '{}'.\n{err}\n{}", file_path.as_deref().unwrap_or("N/A"), line.as_ref().map(|line| line.1.as_str()).unwrap_or("N/A"))]
    OtherIoError {
        file_path: Option<String>,
        line: Option<(usize, String)>,
        err: std::io::Error,
    },
}

impl From<std::io::Error> for ExtractResultsError {
    fn from(value: std::io::Error) -> Self {
        Self::OtherIoError {
            file_path: None,
            line: None,
            err: value,
        }
    }
}

impl ExtractResultsError {
    /// Create a [Log] from self.
    pub fn to_log(&self) -> Log {
        match self {
            Self::ParseUnits {
                file_path,
                line,
                line_num,
                failed_units_str,
            } => error!("Failed to parse units `{}`", failed_units_str).with_component(
                LogComponent::Snippet(Snippet::empty(Some(file_path.clone())).with_chunk(
                    SnippetChunk::empty(*line_num).with_line(SnippetLine::new_with_highlight(
                        line.clone(),
                        SnippetLineKind::Normal,
                        LineHighlight::new_auto(
                            &line,
                            &failed_units_str,
                            format!(
                                "Invalid units, expected either `cm^2`, `a0^2` or `m^2` instead"
                            ),
                            LineHighlightTheme::ERROR,
                        ),
                    )),
                )),
            ),
            Self::ParseEnergy {
                file_path,
                line,
                line_num,
                failed_energy_str,
                err,
            } => error!("Failed to parse energy `{}`", failed_energy_str).with_component(
                LogComponent::Snippet(Snippet::empty(Some(file_path.clone())).with_chunk(
                    SnippetChunk::empty(*line_num).with_line(SnippetLine::new_with_highlight(
                        line.clone(),
                        SnippetLineKind::Normal,
                        LineHighlight::new_auto(
                            &line,
                            &failed_energy_str,
                            format!("{}", err),
                            LineHighlightTheme::ERROR,
                        ),
                    )),
                )),
            ),
            other => error!("{}", other),
        }
    }
}

/// Error that occurs while preparing a result folder.
#[derive(Debug, thiserror::Error)]
pub enum PrepareFoldersError {
    #[error("Failed to create result folder '{path}'.\n{io_err}")]
    CreateResultFolder {
        path: PathBuf,
        io_err: std::io::Error,
    },
}

/// Set of transitions.
///
/// Used to define transitions of interest when extracting data.
/// Contains fields [Self::single] for describing single transitions and [Self::ion] for ionisation transitions.
#[derive(Debug, Default, Clone)]
pub struct TransitionsSet {
    pub single: HashSet<SingleTransition>,
    pub ion: HashSet<SingleState>,
}

impl FromIterator<Transition> for TransitionsSet {
    fn from_iter<T: IntoIterator<Item = Transition>>(iter: T) -> Self {
        let mut this = Self::empty();

        iter.into_iter().for_each(|transition| this.insert(transition));

        this
    }
}

impl TransitionsSet {
    pub fn empty() -> Self {
        Self {
            single: HashSet::new(),
            ion: HashSet::new(),
        }
    }

    /// Insert a transition into the set.
    pub fn insert(&mut self, transition: Transition) {
        match transition {
            Transition {
                to: State::Ionized,
                from: State::Single(state),
            } => self.ion.insert(state),
            Transition {
                to: State::Single(to),
                from: State::Single(from),
            } => self.single.insert(SingleTransition { to, from }),
            _ => unimplemented!(),
        };
    }
    pub fn iter(&self) -> impl Iterator<Item = Transition> + '_ {
        self.ion
            .iter()
            .map(|state| Transition {
                to: State::Ionized,
                from: State::Single(state.clone()),
            })
            .chain(
                self.single
                    .iter()
                    .map(|single_transition| single_transition.clone().into()),
            )
    }
}

async fn prepare_result_sets(
    root_dir: PathBuf,
    config: Config,
    diagnostics: &Arc<AsyncDiagnostics>,
) -> Result<(), PrepareFoldersError> {
    let Config {
        result_sets,
        output_folder,
    } = config;

    let mut output_folder = PathBuf::from(output_folder);
    // If it is relative, start from the root directory.
    if output_folder.is_relative() {
        output_folder = root_dir.join(output_folder);
    }

    // Make sure output directory exists
    if let Err(io_err) = ensure_folder_exists(&output_folder).await {
        return Err(PrepareFoldersError::CreateResultFolder {
            path: output_folder,
            io_err: io_err,
        });
    }

    let output_folder = PathBuf::from(output_folder);

    let mut join_handles = Vec::new();

    for result_set in result_sets.into_iter() {
        let diagnostics = Arc::clone(diagnostics);
        let root_dir = root_dir.clone();
        let output_folder = output_folder.clone();
        join_handles.push((
            result_set.name.clone(),
            tokio::spawn(async move {
                prepare_single_result_set(
                    root_dir,
                    output_folder,
                    result_set,
                    &diagnostics,
                )
                .await
            }),
        ));
    }

    for (set_name, join_handle) in join_handles.into_iter() {
        match join_handle.await {
            Ok(Ok(())) => (),
            Ok(Err(err)) => {
                diagnostics.write_log_background(error!(
                    "Error occured extracting results for '{}'\n{}",
                    set_name, err
                ));
            }
            Err(why) => {
                diagnostics.write_log_background(
                    error!("The thread handling set '{}' failed to join.", set_name)
                        .with_sublog(error!("{}", why)),
                );
            }
        }
    }

    Ok(())
}

#[derive(Debug, thiserror::Error)]
pub enum PrepareFolderError {
    #[error("Failed to create result subfolder '{path}'.\n{err}")]
    CreateResultSubFolder { path: PathBuf, err: std::io::Error },
    #[error(transparent)]
    ExtractResults(#[from] ExtractResultsError),
}

/// Note: Output folder is the folder for all results, a subfolder will be made by this function.
async fn prepare_single_result_set(
    root_dir: impl AsRef<Path>,
    output_folder: PathBuf,
    set: ResultSet,
    diagnostics: &Arc<AsyncDiagnostics>,
) -> Result<(), PrepareFolderError> {
    let ResultSet {
        name,
        source,
        cs_kind,
        units,
        overwrite,
        add_threshold_point,
        transitions,
    } = set;

    let subfolder = output_folder.join(&name);

    // Make sure sub directory exists
    if let Err(err) = ensure_folder_exists(&subfolder).await {
        return Err(PrepareFolderError::CreateResultSubFolder {
            path: subfolder,
            err,
        });
    };
    
    let mut results = bench_time!("get_results", {
        get_results(
            root_dir,
            &source,
            &transitions,
            cs_kind.clone(),
            units,
            add_threshold_point,
            diagnostics,
        )
        .await
    })?;
    // Always sort for now.
    results.sort();


    // Log any results sets which had no cross-sections
    for transition in transitions.iter() {
        if !results.contains_key(&transition) {
            diagnostics.write_log_background(info!("No cross-sections found for (result_name, transition): (`{}`, `{}`), an output file was not written for this transition", name, transition));
        }
    }

    let mut open_options = tokio::fs::OpenOptions::new();

    open_options.write(true).create(true);

    if !overwrite {
        open_options.create_new(true);
    } else {
        open_options.truncate(true);
    }

    let results = Arc::new(results);

    bench_time!("write out result files", {
        let join_handles = FuturesUnordered::new();

        for (transition, cs_vec) in results.iter() {
            if cs_vec.is_empty() {
                continue;
            }

            let transition = transition.clone();
            let results = Arc::clone(&results);
            let open_options = open_options.clone();

            let file_name = format!("ics.{}.{}", transition.to, transition.from);
            let file_path = subfolder.join(&file_name);

            let header = match cs_kind {
                CsKind::Integrated { .. } | CsKind::Born { .. } => format!(
                    "# {} for {} <-- {}, in units of {}\n",
                    cs_kind.as_str(),
                    transition.to,
                    transition.from,
                    units.to_str(),
                ),
                CsKind::Partial {
                    partial_wave,
                    column,
                } => format!(
                    "# {} for {} <-- {}, in units of {}, J={}\n",
                    column.as_str(),
                    transition.to,
                    transition.from,
                    units.to_str(),
                    partial_wave,
                ),
            };

            join_handles.push(tokio::spawn(async move {
                let Some(pairs) = results.get(&transition) else {
                    // Should never happen since this is literally from the keys in results.
                    unreachable!()
                };

                let mut data = Vec::new();

                for (energy, cs) in pairs.iter() {
                    writeln!(data, "{:} {:E}", energy, cs).unwrap();
                }

                let mut file = open_options.open(&file_path).await?;

                file.write_all(header.as_bytes()).await?;
                file.write_all(&data).await?;

                Result::<(), std::io::Error>::Ok(())
            }));
        }

        let mut join_handles = join_handles.enumerate();

        while let Some((idx, join_handle)) = join_handles.next().await {
            match join_handle {
                Ok(Ok(())) => (),
                Ok(Err(err)) => {
                    diagnostics.write_log_background(
                        error!("Thread #{} experienced IO errors:", idx)
                            .with_sublog(error!("{}", err)),
                    );
                }
                Err(_) => unreachable!(),
            }
        }
    });

    Ok(())
}

async fn get_results(
    root_dir: impl AsRef<Path>,
    source: &FilesSource,
    transitions_set: &Arc<TransitionsSet>,
    cs_kind: CsKind,
    units: Units,
    add_threshold_point: bool,
    diagnostics: &Arc<AsyncDiagnostics>,
) -> Result<CsResults, ExtractResultsError> {
    let mut results = match cs_kind {
        CsKind::Born { partial } => {
            todo!();
        }
        CsKind::Integrated { extrapolated } => {
            get_integrated_results(
                root_dir.as_ref(),
                &source,
                transitions_set,
                units,
                extrapolated,
                diagnostics,
            )
            .await
        }
        CsKind::Partial {
            partial_wave,
            column,
        } => {
            get_partial_results(
                root_dir.as_ref(),
                &source,
                transitions_set,
                partial_wave,
                column,
                units,
                diagnostics,
            )
            .await
        }
    }?;

    // Add the threshold point
    if add_threshold_point {
        add_threshold_point_to_results(root_dir, source, &mut results, diagnostics).await?;
    }

    Ok(results)
}

async fn add_threshold_point_to_results(
    root_dir: impl AsRef<Path>,
    source: &FilesSource,
    results: &mut CsResults,
    diagnostics: &Arc<AsyncDiagnostics>,
) -> Result<(), ExtractStateInfoError> {
    let states_set = Arc::new(
        results
            .cross_sections
            .keys()
            .cloned()
            .flat_map(|transition| [transition.from.clone(), transition.to.clone()].into_iter())
            .chain(results.to_ion_cross_sections.keys().cloned())
            .collect::<HashSet<_>>(),
    );
    let state_info = get_state_info(root_dir.as_ref(), &source, &states_set, diagnostics).await?;

    for (state, result) in results.cross_sections.iter_mut() {
        let to_info = match state_info.get(&state.to) {
            Some(to_info) => to_info,
            None => {
                diagnostics.write_log_background(warn!("No state information found for state `{}`, this prevents adding the threshold point for transitions with this state.", state.to));
                continue;
            }
        };

        let from_info = match state_info.get(&state.from) {
            Some(from_info) => from_info,
            None => {
                diagnostics.write_log_background(warn!("No state information found for state `{}`, this prevents adding the threshold point for transitions with this state.", state.from));
                continue;
            }
        };

        let threshold = to_info.energy - from_info.energy;
        result.push((threshold, 0.0));
    }

    for (state, result) in results.to_ion_cross_sections.iter_mut() {
        let from_info = match state_info.get(&state) {
            Some(from_info) => from_info,
            None => {
                diagnostics.write_log_background(warn!("No state information found for state `{}`, this prevents adding the threshold point for transitions with this state.", state));
                continue;
            }
        };

        let threshold = -from_info.energy;
        result.push((threshold, 0.0));
    }
    Ok(())
}
