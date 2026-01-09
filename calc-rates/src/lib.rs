use std::sync::Arc;

use clap::Parser;
use diagnostics::{
    LogFmtOptions,
    diagnostics::{AsyncDiagnostics, ChannelAllow},
    error,
};

use crate::{
    calc::{CalcCmdError, cmd_calc},
    new::{NewCmdError, cmd_new},
};

pub mod config;

pub mod calc;
pub mod new;

pub mod bench;

/// Calculates rates vs temperature from two-column (projectile energy, cross-section) data files.
#[derive(clap::Parser)]
#[command(version, about, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Clone, clap::Subcommand)]
pub enum Commands {
    /// Create a config file.
    New {
        /// Path to write template config file to.
        #[arg(long, default_value = "calc-rates.ron")]
        path: String,
        /// Overwrite existing files silently?
        #[arg(short, long)]
        force: bool,
    },
    /// Calculate rates from a config file.
    Calc {
        #[arg(long, default_value = "calc-rates.ron")]
        path: String,
    },
}

#[derive(thiserror::Error, Debug)]
pub enum AppError {
    #[error(transparent)]
    New(#[from] NewCmdError),
    #[error(transparent)]
    Calc(#[from] CalcCmdError),
    #[error(transparent)]
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
    // test();
    let Cli { command } = Cli::parse();

    let log_level = std::env::vars()
        .find_map(|(var, val)| if var == "RUST_LOG" { Some(val) } else { None })
        .map(|mut level| {
            level.make_ascii_lowercase();
            match level.as_str() {
                "info" => ChannelAllow::all(),
                "warn" => ChannelAllow::WARN | ChannelAllow::ERROR,
                "error" => ChannelAllow::ERROR,
                "none" => ChannelAllow::empty(),
                _ => ChannelAllow::all(),
            }
        })
        .unwrap_or(ChannelAllow::all());

    let diagnostics = {
        let mut diagnostics = AsyncDiagnostics::new(LogFmtOptions::default());
        diagnostics.add_channel(log_level, Box::new(tokio::io::stderr()));
        Arc::new(diagnostics)
    };

    let result = match command {
        Commands::New { path, force } => {
            if let Err(err) = cmd_new(&path, force, &diagnostics).await {
                diagnostics.write_log_background(error!("{}", err));
                Err(err.into())
            } else {
                Ok(())
            }
        }
        Commands::Calc { path } => {
            if let Err(err) = cmd_calc(&path, &diagnostics).await {
                diagnostics.write_log_background(err.to_log());
                Err(err.into())
            } else {
                Ok(())
            }
        }
    };

    diagnostics.finish().await;

    result
}