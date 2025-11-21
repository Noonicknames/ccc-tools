use std::sync::Arc;

use clap::Parser;
use diagnostics::{
    diagnostics::{AsyncDiagnostics, ChannelAllow},
    error, LogFmtOptions,
};
use new::{cmd_new, NewCmdError};
use prep::{cmd_prep, PrepCmdError};

use crate::cli::{cmd_cli, CliCmdError};

pub mod bench;
pub mod config;
// pub mod error;
pub mod results;
pub mod util;

pub mod cli;
pub mod new;
pub mod prep;

/// Program to extract plotting data to prepare for plotting.
#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Clone, clap::Subcommand)]
pub enum Commands {
    /// Prepare results according to a config file.
    Prep {
        /// Path to read in configuration.
        #[arg(short, long, default_value = "prep-plot.ron")]
        path: String,
    },
    /// Writes an example prep-plot file.
    ///
    /// By default writes to './prep-plot.ron'.
    /// The file or directory may be specified with --path.
    New {
        /// Path to write template config file to.
        #[arg(long, default_value = "prep-plot.ron")]
        path: String,
        /// Get an example for extracting total waves.
        #[arg(long)]
        total: bool,
        /// Get an example for extracting partial waves.
        #[arg(long)]
        partial: bool,
        /// Overwrite existing files silently?
        #[arg(short, long)]
        force: bool,
    },
    /// Cli mode e.g. `prep-plot cli s2P<-s2S --source 'Folder("./")'`
    ///
    Cli {
        /// Transitions to extract.
        transitions: Vec<String>,
        #[arg(short, long, default_value = "Folder(\"./\")")]
        source: String,
        /// Kind of cross-section to extract.
        #[arg(short, long, default_value = "Integrated(extrapolated=true)")]
        cs_kind: String,
        /// Directory to output data to, if argument omitted this program will output to stdout.
        #[arg(short, long)]
        output_path: Option<String>,
    },
}

#[derive(Debug, thiserror::Error)]
pub enum AppError {
    #[error(transparent)]
    New(#[from] NewCmdError),
    #[error(transparent)]
    Prep(#[from] PrepCmdError),
    #[error(transparent)]
    Cli(#[from] CliCmdError),
    #[error("Failed to build tokio runtime.\n{err}")]
    FailBuildRuntime { err: std::io::Error },
}

pub fn run() -> Result<(), AppError> {
    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(8) // Kinda don't want to be excessive and use all threads.
        .max_blocking_threads(128)
        .build()
        .map_err(|err| AppError::FailBuildRuntime { err })?;

    rt.block_on(async_run())
}

/// # Warning
/// Only works with tokio runtime.
pub async fn async_run() -> Result<(), AppError> {
    let Cli { command } = Cli::parse();

    let diagnostics = {
        let mut diagnostics = AsyncDiagnostics::new(LogFmtOptions::default());
        diagnostics.add_channel(ChannelAllow::all(), Box::new(tokio::io::stderr()));
        Arc::new(diagnostics)
    };

    let result = match command {
        Commands::Prep { path } => {
            if let Err(err) = cmd_prep(&path, &diagnostics).await {
                diagnostics.write_log_background(err.to_log());
                Err(err.into())
            } else {
                Ok(())
            }
        }
        Commands::New {
            path,
            total,
            partial,
            force,
        } => {
            if let Err(err) = cmd_new(&path, total, partial, force, &diagnostics).await {
                diagnostics.write_log_background(error!("{}", err));
                Err(err.into())
            } else {
                Ok(())
            }
        }
        Commands::Cli {
            transitions,
            source,
            cs_kind,
            output_path,
        } => {
            if let Err(err) = cmd_cli(transitions, source, cs_kind, output_path).await {
                diagnostics.write_log_background(error!("{}", err));
                Err(err.into())
            } else {
                Ok(())
            }
        }
    };

    diagnostics.finish().await;

    result
}
