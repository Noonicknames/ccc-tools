use std::{
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    sync::Arc,
};

use diagnostics::{diagnostics::AsyncDiagnostics, error};
use futures::{pin_mut, StreamExt};
use state_parse::{
    r#async::{AsyncParseSource, AsyncParser, DefaultAsyncParserConfig, SourceInfo},
};
use tokio::io::BufReader;

use crate::{
    config::SingleState,
    extract::state_info::parsers::{ParseStateInfo, StateInfoContext},
    util::FilesSource,
};

mod parsers;

pub struct StateInfo {
    pub energy: f64,
}

#[derive(thiserror::Error, Debug)]
pub enum ExtractStateInfoError {
    #[error("No states.core_parts file found")]
    NoStatesCorePartsFile,
    #[error(transparent)]
    OtherIo(#[from] std::io::Error),
}

pub async fn get_state_info(
    config_dir: impl AsRef<Path>,
    source: &FilesSource,
    states_set: &Arc<HashSet<SingleState>>,
    diagnostics: &Arc<AsyncDiagnostics>,
) -> Result<HashMap<SingleState, StateInfo>, ExtractStateInfoError> {
    let state_core_file_path = get_states_core_parts_file_path(config_dir, source, diagnostics)
        .await
        .ok_or(ExtractStateInfoError::NoStatesCorePartsFile)?;
    let state_core_file = tokio::fs::OpenOptions::new()
        .read(true)
        .open(&state_core_file_path)
        .await?;

    let parse_source = AsyncParseSource::new(
        BufReader::new(state_core_file),
        SourceInfo {
            file_path: Some(state_core_file_path.display().to_string()),
        },
    );
    pin_mut!(parse_source);

    let parser = {
        let mut parser = AsyncParser::empty(DefaultAsyncParserConfig::new());

        parser.add_section(ParseStateInfo);
        Arc::new(parser)
    };

    let context = (
        Arc::new(StateInfoContext {
            states_set: Arc::clone(states_set),
        }),
        Arc::clone(diagnostics),
    );
    let mut output = HashMap::new();

    parser.parse(&context, parse_source, &mut output).await?;

    Ok(output)
}

async fn get_states_core_parts_file_path(
    config_dir: impl AsRef<Path>,
    source: &FilesSource,
    diagnostics: &Arc<AsyncDiagnostics>,
) -> Option<PathBuf> {
    let mut files = source.files(config_dir, |err| async move {
        diagnostics.write_log_background(error!("{}", err));
    });

    while let Some(file) = files.next().await {
        let file = match file {
            Ok(file) => file,
            Err(err) => {
                diagnostics.write_log_background(error!("{}", err));
                continue;
            }
        };

        if file.display().to_string().contains("states.core_parts") {
            return Some(file);
        }
    }
    None
}
