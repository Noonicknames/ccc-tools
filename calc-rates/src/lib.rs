use std::sync::Arc;

use clap::Parser;
use diagnostics::{
    LogFmtOptions,
    diagnostics::{AsyncDiagnostics, ChannelAllow},
    error,
};
use integrate::math::moment_fitted;
// use la::{BlasLib, LapackeLib};
use nalgebra::dvector;

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

pub fn test() {
    // let blas_lib = BlasLib::new().unwrap();
    // let lapacke_lib = LapackeLib::new(&blas_lib).unwrap();
    // let lapacke = lapacke_lib.functions();

    let x_points = dvector![0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 1.0, 2.0, 3.0, 4.0, 6.0, 8.0];
    let int_domain = (0.0, 8.0);
    let int_domain_width = int_domain.1 - int_domain.0;
    let scaled_x_points = x_points.add_scalar(-int_domain.0) / int_domain_width;

    let moment_grid =
        integrate::math::moment_fitted(scaled_x_points.as_slice(), |i| 1.0 / (i + 1) as f64);

    let amp = 0.084;
    let offset = 0.3;
    let width = 0.02;

    let result = moment_grid.eval_fn(|x| {
        let x = x[0] * int_domain_width + int_domain.0;

        let result = x * (-x).exp() + amp * (-((x - offset) / width).powi(2)).exp();

        result
    }) * int_domain_width;

    println!("Integration result: {:.5}", result);

    // // Check removal of integration point., seems to be a possible way to check which points are resonant.
    // for i in 0..x_points.len() {
    //     let x_points = x_points.clone().remove_row(i);
    //     let scaled_x_points = x_points.add_scalar(-int_domain.0) / int_domain_width;
    //     let moment_grid = grid::moment_fitted(scaled_x_points.as_slice(), |i| 1.0 / (i + 1) as f64);

    //     let worse_result = moment_grid.eval_fn(|x| {
    //         let x = x[0] * int_domain_width + int_domain.0;

    //         let result = x * (-x).exp() + amp * (-((x - offset) / width).powi(2)).exp();

    //         result
    //     }) * int_domain_width;

    //     println!("Remove {}th point, got {:.5}, error: {:.5}%", i, worse_result, (worse_result - result) / result);
    // }

    let mut prev_result = result;

    // Check integration craziness.
    for i in 0..x_points.len() - 1 {
        let x_points = x_points.clone().remove_rows(0, i);
        let int_domain = (x_points[0], 8.0);
        let int_domain_width = int_domain.1 - int_domain.0;
        let scaled_x_points = x_points.add_scalar(-int_domain.0) / int_domain_width;
        let moment_grid = moment_fitted(scaled_x_points.as_slice(), |i| 1.0 / (i + 1) as f64);

        let worse_result = moment_grid.eval_fn(|x| {
            let x = x[0] * int_domain_width + int_domain.0;

            let result = x * (-x).exp() + amp * (-((x - offset) / width).powi(2)).exp();

            result
        }) * int_domain_width;

        println!(
            "After {}th points, got {:.5}, error: {:.5}%",
            i,
            worse_result,
            (worse_result - prev_result) / worse_result
        );
        prev_result = worse_result;
    }

    // let result_sparse = moment_grid_sparse.eval_fn(|x| {
    //     let x = x[0] * int_domain_width + int_domain.0;

    //     let result = x * (-x).exp() + amp * (-((x - offset) / width).powi(2)).exp();

    //     println!("({}, {})", x, result);

    //     result
    // }) * int_domain_width;

    // println!("integration result: {:.5} vs {:.5} (sparse)", result, result_sparse);
}
