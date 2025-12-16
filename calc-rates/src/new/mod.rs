use std::{io::Write, sync::Arc};

use diagnostics::diagnostics::AsyncDiagnostics;
use tokio::io::AsyncWriteExt;

use crate::config::ConfigSerde;

#[derive(thiserror::Error, Debug)]
pub enum NewCmdError {
    #[error("{}", match file { Some(file) => format!("IO error encountered reading `{}`: {}", file, err), None => format!("{}", err)})]
    Io {
        err: std::io::Error,
        file: Option<String>,
    },
}

impl From<std::io::Error> for NewCmdError {
    fn from(value: std::io::Error) -> Self {
        Self::Io {
            err: value,
            file: None,
        }
    }
}

pub async fn cmd_new(
    path: &str,
    force: bool,
    _diagnostics: &Arc<AsyncDiagnostics>,
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
                            return Err(NewCmdError::Io {
                                err,
                                file: Some(path.to_owned()),
                            });
                        }
                    }
                }
            }

            print!("File '{}' already exists, overwrite? (y/n) : ", path);
            std::io::stdout().flush()?;
            let stdin = std::io::stdin();
            let mut response = String::new();

            // This is blocking code in an async function but somehow doesn't panic.
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

    let template = ConfigSerde::template();

    let template_ser =
        ron::ser::to_string_pretty(&template, ron::ser::PrettyConfig::default()).unwrap();
    file.write_all(template_ser.as_bytes()).await.unwrap();
    println!("`calc-rates` config file written to `{}`", path);

    Ok(())
}
