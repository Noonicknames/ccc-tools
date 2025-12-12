use std::{io::Write, sync::Arc};

use diagnostics::diagnostics::AsyncDiagnostics;
use tokio::io::AsyncWriteExt;

use crate::config::{ConfigSerde, CsKind, PartialColumn};

#[derive(Debug, thiserror::Error)]
pub enum NewCmdError {
    #[error("{}", match file { Some(file) => format!("IO error encountered reading `{}`: {}", file, err), None => format!("{}", err)})]
    IoError {
        err: std::io::Error,
        file: Option<String>,
    },
}

impl From<std::io::Error> for NewCmdError {
    fn from(value: std::io::Error) -> Self {
        Self::IoError {
            err: value,
            file: None,
        }
    }
}

/// Run upon calling the 'new' command.
pub async fn cmd_new(
    path: &str,
    total: bool,
    partial: bool,
    force: bool,
    diagnostics: &Arc<AsyncDiagnostics>,
) -> Result<(), NewCmdError> {
    let mut file = 'file: {
        let mut open_options = tokio::fs::OpenOptions::new();
        open_options.write(true).create_new(true);

        // If not forcing.
        if !force {
            match open_options.open(&path).await {
                Ok(file) => {
                    break 'file file;
                }
                Err(err) => {
                    use std::io::ErrorKind;
                    match err.kind() {
                        ErrorKind::AlreadyExists => (),
                        _ => {
                            return Err(NewCmdError::IoError {
                                err,
                                file: Some(path.to_owned()),
                            })
                        }
                    }
                }
            }

            print!("File '{}' already exists, overwrite? (y/n) : ", path);
            std::io::stdout().flush()?;
            let stdin = std::io::stdin();
            let mut response = String::new();
            stdin.read_line(&mut response)?;
            response.make_ascii_lowercase();

            match response.as_str().trim() {
                "y" | "yes" => (),
                _ => {
                    println!("Cancelled.");
                    return Ok(());
                }
            }
        }

        open_options
            .create_new(false)
            .truncate(true)
            .open(&path)
            .await?
    };

    let mut template = ConfigSerde::template();

    if total {
        template.cs_kind = CsKind::Integrated {
            extrapolated: false,
        };
    }

    if partial {
        template.cs_kind = CsKind::Partial {
            partial_wave: 1,
            column: PartialColumn::T0,
        }
    }

    let template_ser =
        ron::ser::to_string_pretty(&template, ron::ser::PrettyConfig::default()).unwrap();
    file.write_all(template_ser.as_bytes()).await.unwrap();

    Ok(())
}
