use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use diagnostics::{diagnostics::AsyncDiagnostics, error};

use futures::{
    pin_mut,
    stream::{BoxStream, FuturesUnordered},
    StreamExt,
};
use state_parse::r#async::{compose, AsyncParseSource, AsyncParser, DefaultAsyncParserConfig, SourceInfo};
use tokio_stream::wrappers::ReadDirStream;

use crate::{
    config::{PartialColumn, Units},
    extract::{
        partial::parsers::{ParseCs, ParseEnergy, ParseIonCs, ParseUnits, PartialCsContext},
        CsResults, TransitionsSet,
    }, util::FilesSource,
};

use super::ExtractResultsError;

mod parsers;

/// Get results from a single partial wave.
pub async fn get_partial_results(
    config_dir: impl AsRef<Path>,
    source: &FilesSource,
    transitions_set: &Arc<TransitionsSet>,
    partial_wave: u32,
    column: PartialColumn,
    units: Units,
    diagnostics: &Arc<AsyncDiagnostics>,
) -> Result<CsResults, ExtractResultsError> {
    let totalcs_files = totalcs_j_files(config_dir, source, diagnostics).await?;
    pin_mut!(totalcs_files);

    let mut results = CsResults::empty();
    let mut tasks = FuturesUnordered::new();

    let partialcs_context = Arc::new(PartialCsContext {
        partial_wave,
        column,
        units,
        transitions_set: Arc::clone(transitions_set),
    });

    let parser = {
        let mut parser = AsyncParser::empty(DefaultAsyncParserConfig::new());
        parser.add_section(ParseUnits);
                parser.add_section(ParseEnergy);

        let parse_cs = {
            let mut repeat = compose::RepeatConsume::empty(|_repeat_error| Ok(()));

            if !transitions_set.ion.is_empty() {
                repeat.add_section(ParseIonCs);
            }
            
            if !transitions_set.single.is_empty() {
                repeat.add_section(ParseCs);
            }

            repeat
        };

        parser.add_section(parse_cs);

        Arc::new(parser)
    };

    while let Some(totalcs_path) = totalcs_files.next().await {
        let parser = Arc::clone(&parser);
        let context = (Arc::clone(&partialcs_context), Arc::clone(diagnostics));

        tasks.push(tokio::spawn(async move {
            let file = match tokio::fs::OpenOptions::new()
                .read(true)
                .open(&totalcs_path)
                .await
            {
                Ok(file) => file,
                Err(err) => {
                    return Err(ExtractResultsError::FailedOpenFile {
                        file_path: totalcs_path.display().to_string(),
                        err,
                    });
                }
            };

            let source = AsyncParseSource::new(
                tokio::io::BufReader::new(file),
                SourceInfo {
                    file_path: Some(totalcs_path.display().to_string()),
                },
            );
            pin_mut!(source);

            // bench_time!("parse_file", {
            parser.parse_default(&context, source).await
            // })
        }));

        // // This is probably not needed, only for extremely large results that I somehow hit a memory limit.
        // const MAX_CONCURRENCY: usize = 8;
        // if tasks.len() >= MAX_CONCURRENCY {
        //     match tasks.next().await.unwrap() {
        //         Ok(Ok(single_energy_result)) => {
        //             results.push_single_energy_results(&single_energy_result)
        //         }
        //         Ok(Err(err)) => {
        //             diagnostics.write_log_background(err.to_log());
        //         }
        //         Err(err) => {
        //             diagnostics.write_log_background(
        //                 error!("Failed to join task").with_sublog(error!("{}", err)),
        //             );
        //         }
        //     }
        // }
    }

    while let Some(result) = tasks.next().await {
        match result {
            Ok(Ok(single_energy_result)) => {
                results.push_single_energy_results(&single_energy_result)
            }
            Ok(Err(err)) => {
                diagnostics.write_log_background(err.to_log());
            }
            Err(err) => {
                diagnostics.write_log_background(
                    error!("Failed to join task").with_sublog(error!("{}", err)),
                );
            }
        }
    }
    Ok(results)
}

/// Returns a
async fn totalcs_j_files<'a>(
    config_dir: impl AsRef<Path>,
    source: &'a FilesSource,
    diagnostics: &'a Arc<AsyncDiagnostics>,
) -> tokio::io::Result<BoxStream<'a, PathBuf>> {
    Ok(match source {
        FilesSource::Folder(folder) => {
            // To avoid cloning, use Arc.
            let path = Arc::new(config_dir.as_ref().join(&folder));
            tokio::fs::read_dir(path.as_ref())
                .await
                .map(|read_dir| ReadDirStream::new(read_dir))?
                .filter_map(move |dir_entry| {
                    let path = Arc::clone(&path);
                    async move {
                        match dir_entry {
                            Ok(dir_entry) => {
                                if dir_entry
                                    .file_name()
                                    .into_string()
                                    .is_ok_and(|file_name| is_totalcs_j_file(&file_name))
                                    && dir_entry
                                        .file_type()
                                        .await
                                        .is_ok_and(|file_type| file_type.is_file())
                                {
                                    Some(dir_entry.path())
                                } else {
                                    None
                                }
                            }
                            Err(err) => {
                                diagnostics.write_log_background(
                                    error!("Error reading from directory `{}`", path.display())
                                        .with_sublog(error!("{}", err)),
                                );
                                None
                            }
                        }
                    }
                })
                .boxed()
        }
        FilesSource::Folders(folders) => futures::stream::iter(
            folders
                .iter()
                .map(|folder| Arc::new(config_dir.as_ref().join(&folder)))
                .collect::<Vec<_>>(),
        )
        .filter_map(move |path| async move {
            match tokio::fs::read_dir(path.as_ref()).await {
                Ok(read_dir) => Some((path, ReadDirStream::new(read_dir))),
                Err(err) => {
                    diagnostics.write_log_background(
                        error!("Error reading result directory `{}`", path.display())
                            .with_sublog(error!("{}", err)),
                    );
                    None
                }
            }
        })
        .flat_map_unordered(None, |(path, read_dir)| {
            read_dir.map(move |read_dir| (Arc::clone(&path), read_dir))
        })
        .filter_map(move |(path, dir_entry)| async move {
            match dir_entry {
                Ok(dir_entry) => {
                    let is_totalcs_name = dir_entry
                        .file_name()
                        .into_string()
                        .is_ok_and(|file_name| is_totalcs_j_file(&file_name));
                    if !is_totalcs_name {
                        return None;
                    }
                    let is_file = dir_entry
                        .file_type()
                        .await
                        .is_ok_and(|file_type| file_type.is_file());
                    if !is_file {
                        return None;
                    }

                    Some(dir_entry.path())
                }
                Err(err) => {
                    diagnostics.write_log_background(
                        error!("Error reading from directory `{}`", path.display())
                            .with_sublog(error!("{}", err)),
                    );
                    None
                }
            }
        })
        .boxed(),
        FilesSource::File(file) => {
            let path = config_dir.as_ref().join(&file);
            futures::stream::once(async move { path }).boxed()
        },
        FilesSource::None => {
            futures::stream::empty().boxed()
        }
    })
}

fn is_totalcs_j_file(name: &str) -> bool {
    if name.starts_with("totalcs_") && name.ends_with("_J") {
        true
    } else {
        false
    }
}
