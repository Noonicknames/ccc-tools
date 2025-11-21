#[derive(Debug, thiserror::Error)]
pub enum CliCmdError {
    #[error("{}", match file { Some(file) => format!("IO error encountered reading `{}`: {}", file, err), None => format!("{}", err)})]
    IoError {
        err: std::io::Error,
        file: Option<String>,
    },
}

impl From<std::io::Error> for CliCmdError {
    fn from(value: std::io::Error) -> Self {
        Self::IoError {
            err: value,
            file: None,
        }
    }
}

pub async fn cmd_cli(
    transitions: Vec<String>,
    source: String,
    cs_kind: String,
    output_path: Option<String>,
) -> Result<(), CliCmdError> {
    todo!();
    Ok(())
}
