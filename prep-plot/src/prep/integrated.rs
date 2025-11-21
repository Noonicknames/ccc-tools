use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use diagnostics::{
    diagnostics::{AsyncDiagnostics, Diagnostics},
    error,
};
use futures::{
    pin_mut,
    stream::{BoxStream, FuturesUnordered},
    StreamExt,
};
use state_parse::r#async::{
    compose::{self, AsyncSectionParserBuilder},
    AsyncParseSource, AsyncParser, DefaultAsyncParserConfig, SourceInfo,
};

use crate::{
    config::Units,
    prep::{
        TransitionsSet, integrated::parsers::{IntegratedCsContext, IntegratedCsState, ParseEnergyUnits, ParseIonCs, ParseSingleCs}
    },
    results::{CsResults, SingleEnergyResults},
    util::FilesSource,
};

use super::ExtractResultsError;

mod parsers;

// /// Get results from a single partial wave.
// pub async fn get_born_results(
//     config_dir: impl AsRef<Path>,
//     source: &FilesSource,
//     transitions_set: &Arc<TransitionsSet>,
//     units: Units,
//     partial: bool,
//     diagnostics: &Arc<AsyncDiagnostics>,
// ) -> Result<CsResults, ExtractResultsError> {
//     let totalcs_files = totalcs_files(config_dir, &source, diagnostics).await;
//     pin_mut!(totalcs_files);

//     let mut results = CsResults::empty();
//     let mut tasks = FuturesUnordered::new();

//     let integratedcs_context = Arc::new(IntegratedCsContext {
//         extrapolated,
//         units,
//         transitions_set: Arc::clone(transitions_set),
//     });

//     let parser = {
//         let mut parser = AsyncParser::empty(DefaultAsyncParserConfig::new());
//         parser.add_section(ParseEnergyUnits);

//         let parse_cs = {
//             let mut repeat = compose::RepeatConsume::empty(|_repeat_err| Ok(()));

//             if !transitions_set.ion.is_empty() {
//                 repeat.add_section(ParseIonCs);
//             }

//             if !transitions_set.single.is_empty() {
//                 repeat.add_section(ParseSingleCs);
//             }

//             repeat
//         };

//         parser.add_section(parse_cs);

//         Arc::new(parser)
//     };

//     while let Some(totalcs_path) = totalcs_files.next().await {
//         let parser = Arc::clone(&parser);
//         let context = (Arc::clone(&integratedcs_context), Arc::clone(diagnostics));

//         tasks.push(tokio::spawn(async move {
//             let file = match tokio::fs::OpenOptions::new()
//                 .read(true)
//                 .open(&totalcs_path)
//                 .await
//             {
//                 Ok(file) => file,
//                 Err(err) => {
//                     return Err(ExtractResultsError::FailedOpenFile {
//                         file_path: totalcs_path.display().to_string(),
//                         err,
//                     });
//                 }
//             };

//             let source = AsyncParseSource::new(
//                 tokio::io::BufReader::new(file),
//                 SourceInfo {
//                     file_path: Some(totalcs_path.display().to_string()),
//                 },
//             );
//             pin_mut!(source);

//             // bench_time!("parse_file", {
//             parser.parse_default(&context, source).await
//             // })
//         }));

//         // // Backpressure
//         // // This is probably not needed, only for extremely large results that I somehow hit a memory limit.
//         // const MAX_CONCURRENCY: usize = 8;
//         // if tasks.len() >= MAX_CONCURRENCY {
//         //     match tasks.next().await.unwrap() {
//         //         Ok(Ok(single_energy_result)) => {
//         //             results.push_single_energy_results(&single_energy_result)
//         //         }
//         //         Ok(Err(err)) => {
//         //             diagnostics.write_log_background(err.to_log());
//         //         }
//         //         Err(err) => {
//         //             diagnostics.write_log_background(
//         //                 error!("Failed to join task").with_sublog(error!("{}", err)),
//         //             );
//         //         }
//         //     }
//         // }
//     }

//     while let Some(result) = tasks.next().await {
//         match result {
//             Ok(Ok(single_energy_result)) => {
//                 results.push_single_energy_results(&single_energy_result)
//             }
//             Ok(Err(err)) => {
//                 diagnostics.write_log_background(err.to_log());
//             }
//             Err(err) => {
//                 diagnostics.write_log_background(
//                     error!("Failed to join task").with_sublog(error!("{}", err)),
//                 );
//             }
//         }
//     }

//     Ok(results)
// }

/// Get results from a single partial wave.
pub async fn get_integrated_results(
    config_dir: impl AsRef<Path>,
    source: &FilesSource,
    transitions_set: &Arc<TransitionsSet>,
    units: Units,
    extrapolated: bool,
    diagnostics: &Arc<AsyncDiagnostics>,
) -> Result<CsResults, ExtractResultsError> {
    let totalcs_files = totalcs_files(config_dir, &source, diagnostics).await;
    pin_mut!(totalcs_files);

    let mut results = CsResults::empty();
    let mut tasks = FuturesUnordered::new();

    let integratedcs_context = Arc::new(IntegratedCsContext {
        extrapolated,
        units,
        transitions_set: Arc::clone(transitions_set),
    });

    let parser = {
        let mut parser = AsyncParser::empty(DefaultAsyncParserConfig::new());
        parser.add_section(
            ParseEnergyUnits
                .map_context(|_ctx: &(Arc<IntegratedCsContext>, Arc<AsyncDiagnostics>)| &())
                .map_output(|output: &mut SingleEnergyResults| &mut output.energy)
                .map_state(|state: &mut IntegratedCsState| &mut state.units_from),
        );

        let parse_cs = {
            let mut repeat = compose::RepeatConsume::empty(|_repeat_err| Ok(()));

            if !transitions_set.ion.is_empty() {
                repeat.add_section(ParseIonCs);
            }

            if !transitions_set.single.is_empty() {
                repeat.add_section(ParseSingleCs);
            }

            repeat
        };

        parser.add_section(parse_cs);

        Arc::new(parser)
    };

    while let Some(totalcs_path) = totalcs_files.next().await {
        let parser = Arc::clone(&parser);
        let context = (Arc::clone(&integratedcs_context), Arc::clone(diagnostics));

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

        // // Backpressure
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
async fn totalcs_files<'a>(
    config_dir: impl AsRef<Path>,
    source: &'a FilesSource,
    diagnostics: &'a Arc<AsyncDiagnostics>,
) -> BoxStream<'a, PathBuf> {
    source
        .files(config_dir, move |err| async move {
            diagnostics.write_log_background(error!("Failed to open folder: {}", err));
        })
        .filter_map(move |file_path| async move {
            let file_path = match file_path {
                Ok(file_path) => file_path,
                Err(err) => {
                    diagnostics.write_log_background(error!("{}", err));
                    return None;
                }
            };

            if is_totalcs_file(file_path.file_name()?.to_str()?) {
                Some(file_path)
            } else {
                None
            }
        })
        .boxed()
}

/// Checks if the file is a normal totalcs file.
pub fn is_totalcs_file(name: &str) -> bool {
    if name.starts_with("totalcs_") && !name.ends_with("_J") {
        true
    } else {
        false
    }
}
