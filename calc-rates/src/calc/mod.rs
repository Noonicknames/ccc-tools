use std::{path::Path, sync::Arc};

use diagnostics::{
    LogComponent,
    components::snippet::{
        Footer, FooterKind, LineHighlight, LineHighlightTheme, Snippet, SnippetChunk,
        SnippetLineKind,
    },
    diagnostics::AsyncDiagnostics,
    error, warn,
};
use tokio::{
    fs::OpenOptions,
    io::{AsyncBufReadExt, AsyncReadExt, BufReader},
};
use unicode_width::UnicodeWidthStr;

use crate::config::{Config, ResultSet};

#[derive(thiserror::Error, Debug)]
pub enum CalcCmdError {
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
}

pub async fn cmd_calc(
    path: impl AsRef<Path>,
    diagnostics: &Arc<AsyncDiagnostics>,
) -> Result<(), CalcCmdError> {
    let config = get_config(path).await?;

    for result_set in config.result_sets {
        let diagnostics_copy = Arc::clone(diagnostics);
        if let Err(err) = process_result_set(result_set, diagnostics_copy).await {
            diagnostics.write_log_background(error!("Error whilst reading result set: {}", err));
        }
    }

    Ok(())
}

async fn process_result_set(
    result_set: ResultSet,
    diagnostics: Arc<AsyncDiagnostics>,
) -> Result<(), std::io::Error> {
    println!("Processing `{}`", result_set.name);
    let file = OpenOptions::new()
        .read(true)
        .open(result_set.source.as_str())
        .await?;
    let mut file = BufReader::new(file);

    let mut energy_vec = Vec::new();
    let mut cs_vec = Vec::new();

    let mut buf = String::new();

    let mut line_count = 0;

    loop {
        buf.clear();
        if file.read_line(&mut buf).await? == 0 {
            break;
        };

        line_count += 1;

        // Skip whitespace only, or starts with #
        if buf
            .split_whitespace()
            .next()
            .map(|first_column| first_column.starts_with("#"))
            .unwrap_or(true)
        {
            continue;
        }

        let mut cols = buf.split_whitespace();

        let energy = cols.next().unwrap(); // Should be guaranteed by the check above.

        let energy = match energy.parse::<f64>() {
            Ok(energy) => energy,
            Err(err) => {
                let snippet = Snippet::empty(Some(result_set.source.clone()))
                    .with_chunk(
                        SnippetChunk::from_str(&buf, SnippetLineKind::Normal, line_count)
                            .with_highlight_auto(
                                line_count,
                                energy,
                                format!("{}", err),
                                LineHighlightTheme::ERROR,
                            ),
                    )
                    .with_footer(Footer::new(
                        "The first two columns are parsed as `<ENERGY> <CROSS-SECTION>`"
                            .to_owned(),
                        FooterKind::Note,
                    ))
                    .with_footer(Footer::new(
                        "Example valid lines `10.0 1.23` or `1E1 1.23E0`"
                            .to_owned(),
                        FooterKind::Note,
                    ));
                diagnostics.write_log_background(
                    error!("Failed to parse {}", energy).with_component(LogComponent::Snippet(snippet)),
                );
                continue;
            }
        };

        let cs = match cols.next() {
            Some(cs) => cs,
            None => {
                let snippet = Snippet::empty(Some(result_set.source.clone()))
                    .with_chunk(
                        SnippetChunk::from_str(&buf, SnippetLineKind::Normal, line_count)
                            .with_highlight(
                                line_count,
                                LineHighlight::new(
                                    buf.width(),
                                    1,
                                    "Expected cross section after energy".to_owned(),
                                    LineHighlightTheme::ERROR,
                                ),
                            ),
                    )
                    .with_footer(Footer::new(
                        "The first two columns are parsed as `<ENERGY> <CROSS-SECTION>`"
                            .to_owned(),
                        FooterKind::Note,
                    ))
                    .with_footer(Footer::new(
                        "Example valid lines `10.0 1.23` or `1E1 1.23E0`"
                            .to_owned(),
                        FooterKind::Note,
                    ));
                diagnostics.write_log_background(
                    error!("Expected cross section")
                        .with_component(LogComponent::Snippet(snippet)),
                );
                continue;
            }
        };

        let cs = match cs.parse::<f64>() {
            Ok(cs) => cs,
            Err(err) => {
                let snippet = Snippet::empty(Some(result_set.source.clone())).with_chunk(
                    SnippetChunk::from_str(&buf, SnippetLineKind::Normal, line_count)
                        .with_highlight_auto(
                            line_count,
                            cs,
                            format!("{}", err),
                            LineHighlightTheme::ERROR,
                        ),
                );
                diagnostics.write_log_background(
                    warn!("Could not parse cross section")
                        .with_component(LogComponent::Snippet(snippet)),
                );
                continue;
            }
        };

        energy_vec.push(energy);
        cs_vec.push(cs);
    }

    Ok(())
}

async fn get_config(file_path: impl AsRef<Path>) -> Result<Config, CalcCmdError> {
    {
        let mut open_options = tokio::fs::OpenOptions::new();
        open_options.read(true);

        let mut file = open_options.open(file_path.as_ref()).await.map_err(|err| {
            CalcCmdError::OpenConfig {
                file_path: file_path.as_ref().display().to_string(),
                err,
            }
        })?;

        let mut buf = String::new();
        file.read_to_string(&mut buf)
            .await
            .map_err(|err| CalcCmdError::ConfigInvalidUtf8 {
                file_path: file_path.as_ref().display().to_string(),
                err,
            })?;

        Ok(ron::from_str::<Config>(&buf).map_err(|err| {
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
            CalcCmdError::ConfigParse {
                file_path: file_path.as_ref().display().to_string(),
                start_line,
                lines,
                err,
            }
        })?)
    }
}
