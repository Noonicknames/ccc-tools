use std::{
    collections::BTreeMap,
    io::Write,
    path::{Path, PathBuf},
    sync::Arc,
};

use diagnostics::{
    Log, LogComponent,
    components::snippet::{
        Footer, FooterKind, LineHighlight, LineHighlightTheme, MultiLineHighlight, Snippet,
        SnippetChunk, SnippetLineKind,
    },
    diagnostics::AsyncDiagnostics,
    error, info, warn,
};
use futures::stream::FuturesUnordered;
use la::{BlasLib, LapackeLib};
use ordered_float::OrderedFloat;
use tokio::{
    fs::OpenOptions,
    io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader},
};
use tokio_stream::{StreamExt, wrappers::LinesStream};
use unicode_width::UnicodeWidthStr;

use crate::{
    config::{
        CollisionRateUnits, Config, ConfigSerde, CsUnits, CsUnitsOrAuto, EnergyUnits,
        EnergyUnitsOrAuto, ResultSet, TemperatureUnits, integration_grid_to_points,
    },
    integrate::{
        GaussIntegrator, IntegrationKind, Integrator, NaturalCubicIntegrator, SubIntegrators,
    },
    util::ensure_folder_exists,
};

#[derive(thiserror::Error, Debug)]
pub enum CalcCmdError {
    #[error("Failed to open config file '{file_path}': {err}")]
    OpenConfig {
        file_path: String,
        err: std::io::Error,
    },
    #[error("Failed to create result folder '{path}'.\n{io_err}")]
    CreateResultFolder {
        path: PathBuf,
        io_err: std::io::Error,
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

impl CalcCmdError {
    pub fn to_log(&self) -> Log {
        match self {
            Self::ConfigParse {
                file_path,
                start_line,
                lines,
                err,
            } => {
                let mut snippet = Snippet::empty(Some(file_path.clone()));
                let mut chunk =
                    SnippetChunk::from_str(&lines, SnippetLineKind::Normal, *start_line);

                // Same line error
                if err.span.end.line == err.span.start.line {
                    _ = chunk.set_highlight(
                        err.span.start.line,
                        Some(LineHighlight::new(
                            err.span.start.col - 1,
                            err.span.end.col - err.span.start.col + 1,
                            err.to_string(),
                            LineHighlightTheme::ERROR,
                        )),
                    );
                } else {
                    chunk.set_multiline_highlight(Some(MultiLineHighlight {
                        start_message: err.to_string(),
                        start_line: err.span.start.line,
                        start_col: err.span.start.col,
                        end_message: "".to_owned(),
                        end_line: err.span.end.line,
                        end_col: err.span.end.col,
                        theme: LineHighlightTheme::ERROR,
                    }));
                }

                snippet.add_chunk(chunk);

                error!("Failed to parse config file").with_component(LogComponent::Snippet(snippet))
            }
            _ => error!("{}", self),
        }
    }
}

pub async fn cmd_calc(
    path: impl AsRef<Path>,
    diagnostics: &Arc<AsyncDiagnostics>,
) -> Result<(), CalcCmdError> {
    let Config {
        result_sets,
        temperatures,
        temperature_units,
        energy_units,
        cs_units,
        output_folder,
        collision_rate_units,
    } = get_config(path).await?;

    let temperatures = Arc::new(temperatures);
    let output_folder = Arc::new(PathBuf::from(output_folder));

    if let Err(err) = ensure_folder_exists(output_folder.as_ref()).await {
        return Err(CalcCmdError::CreateResultFolder {
            path: output_folder.as_ref().clone(),
            io_err: err,
        });
    }

    let mut tasks = FuturesUnordered::new();

    for result_set in result_sets {
        let diagnostics_copy = Arc::clone(diagnostics);
        let temperatures = Arc::clone(&temperatures);
        let output_folder = Arc::clone(&output_folder);

        tasks.push(process_result_set(
            result_set,
            temperatures,
            temperature_units,
            collision_rate_units,
            output_folder,
            diagnostics_copy,
        ));
    }

    while let Some(result) = tasks.next().await {
        if let Err(err) = result {
            diagnostics.write_log_background(error!("Error whilst reading result set: {}", err));
        }
    }

    Ok(())
}

async fn process_result_set(
    result_set: ResultSet,
    temperatures: Arc<Vec<f64>>,
    temperature_units: TemperatureUnits,
    rate_units: CollisionRateUnits,
    output_folder: Arc<PathBuf>,
    diagnostics: Arc<AsyncDiagnostics>,
) -> Result<(), std::io::Error> {
    let ResultSet {
        name,
        source,
        grid,
        energy_units,
        cs_units,
        output_integrands,
    } = result_set;
    let name = Arc::new(name);

    let (energy_vec, cs_vec) = get_energy_cs(&source, energy_units, cs_units, &diagnostics).await?;
    let energy_vec = Arc::new(energy_vec);

    let blas_lib = BlasLib::new().unwrap();
    let lapacke_lib = LapackeLib::new(&blas_lib).unwrap();

    let output_integrands_data = if output_integrands {
        let (send, mut recv) = tokio::sync::mpsc::unbounded_channel::<(f64, Vec<f64>)>();
        let integrands_folder = output_folder.join(format!("{}.integrands", name));
        let temperature_units = temperature_units.clone();
        let diagnostics = Arc::clone(&diagnostics);
        let energy_vec = Arc::clone(&energy_vec);
        let name = Arc::clone(&name);
        let lapacke_lib = lapacke_lib.clone();
        let task = tokio::task::spawn(async move {
            ensure_folder_exists(&integrands_folder).await?;

            let mut open_options = OpenOptions::new();
            open_options.create(true).write(true).truncate(true);

            let mut tasks = FuturesUnordered::new();

            let mut integrator =
                NaturalCubicIntegrator::new(&energy_vec, lapacke_lib.functions_static());
            let energy_range =
                energy_vec.first().cloned().unwrap()..energy_vec.last().cloned().unwrap();
            let dense_x_grid = (0..=3000)
                .map(|i| i as f64 / 3000.0 * (energy_range.end - energy_range.start) + energy_range.start)
                .collect::<Vec<_>>();

            while let Some((temperature, integrand)) = recv.recv().await {
                let mut x_grid = dense_x_grid.clone();
                let interpolation = integrator.interpolation(&integrand);

                interpolation.eval_many(&mut x_grid);
                let integrand_interpolated = x_grid;

                let mut buf = Vec::new();

                writeln!(
                    &mut buf,
                    "# Integrand for `{}`, in units of Ha, a0^2Ha, with T={}{}",
                    name,
                    temperature,
                    temperature_units.as_str()
                )?;

                for (energy, integrand) in dense_x_grid.iter().zip(integrand_interpolated.iter()) {
                    writeln!(&mut buf, "{} {:E}", energy, integrand)?;
                }

                let open_options = open_options.clone();
                let file_name = integrands_folder.join(format!(
                    "T={}{}",
                    temperature,
                    temperature_units.as_str()
                ));
                tasks.push(tokio::spawn(async move {
                    let file = open_options.open(&file_name).await;
                    let mut file = match file {
                        Ok(file) => file,
                        Err(err) => {
                            return Err(err);
                        }
                    };
                    file.write_all(&buf).await?;
                    Ok::<(), std::io::Error>(())
                }));
            }

            while let Some(result) = tasks.next().await {
                match result {
                    Ok(Ok(())) => (),
                    Err(err) => {
                        diagnostics.write_log_background(
                            warn!("Error joining tokio task.").with_sublog(error!("{}", err)),
                        );
                    }
                    Ok(Err(err)) => {
                        diagnostics.write_log_background(
                            warn!("Error occured while outputting integrands.")
                                .with_sublog(error!("{}", err)),
                        );
                    }
                }
            }
            Ok::<(), std::io::Error>(())
        });
        Some((task, send))
    } else {
        None
    };

    if energy_vec.len() < 2 {
        diagnostics.write_log_background(warn!(
            "At least 2 cross sections are required, only {} found in `{}`",
            energy_vec.len(),
            source.display(),
        ));
        return Ok(());
    }

    let mut grid = integration_grid_to_points(&grid, energy_vec.len())
        .into_iter()
        .map(|(kind, range)| match kind {
            IntegrationKind::AutoGauss => todo!(),
            IntegrationKind::NaturalCubic => (
                range.clone(),
                Box::new(NaturalCubicIntegrator::new(
                    &energy_vec[range],
                    lapacke_lib.functions_static(),
                )) as Box<dyn Integrator>,
            ),
            IntegrationKind::Gauss => (
                range.clone(),
                Box::new(GaussIntegrator::new(&energy_vec[range])) as Box<dyn Integrator>,
            ),
        })
        .fold(
            SubIntegrators::new(),
            |mut acc, (range, partial_integrator)| {
                acc.push_boxed(range, partial_integrator);
                acc
            },
        );

    // let (grid_idx, grid_cache) =
    //     resolve_grid(&energy_vec, &cs_vec, &grid, name.as_ref(), &diagnostics);

    let mut rate_vec = Vec::with_capacity(temperatures.len());

    let temp_conversion =
        TemperatureUnits::conversion_factor(temperature_units, TemperatureUnits::Hartree);

    let rate_conversion =
        CollisionRateUnits::conversion_factor(CollisionRateUnits::Atomic, rate_units);

    for &temperature in temperatures.iter() {
        // Integrand according to Joel
        // Atomic units kB = 1, T is converted to Hartree, energy in hartree
        let integrand = energy_vec
            .iter()
            .zip(cs_vec.iter())
            .map(|(&energy, &cs)| cs * energy * (-energy / (temperature * temp_conversion)).exp())
            .collect::<Vec<_>>();

        let mut result = grid.integrate(&integrand);

        // Factor in front
        result = result
            * std::f64::consts::PI.sqrt().recip()
            * (2.0 / (temperature * temp_conversion)).powf(3.0 / 2.0);

        // Convert to desired units
        result = result * rate_conversion;

        rate_vec.push(result);

        if let Some((_, send)) = &output_integrands_data {
            if let Err(err) = send.send((temperature, integrand)) {
                diagnostics.write_log_background(
                    warn!("Failed to send integrand across channel.")
                        .with_sublog(error!("{}", err)),
                );
            }
        }
    }

    // Output results
    let mut data = Vec::new();
    writeln!(
        &mut data,
        "# Rates for `{}`, in units of {}, {}",
        name,
        temperature_units.as_str(),
        rate_units.as_str(),
    )?;

    for (temperature, rate) in temperatures.iter().zip(rate_vec.iter()) {
        writeln!(&mut data, "{} {:E}", temperature, rate)?;
    }

    let mut file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(output_folder.join(name.as_ref()))
        .await?;

    file.write_all(&data).await?;

    if let Some((task, send)) = output_integrands_data {
        drop(send);
        if let Err(err) = task.await {
            diagnostics.write_log_background(
                warn!("Failed to join tokio task").with_sublog(error!("{}", err)),
            );
        }
    }

    Ok(())
}

async fn get_energy_cs(
    source: impl AsRef<Path>,
    energy_units: EnergyUnitsOrAuto,
    cs_units: CsUnitsOrAuto,
    diagnostics: &Arc<AsyncDiagnostics>,
) -> Result<(Vec<f64>, Vec<f64>), std::io::Error> {
    let file = OpenOptions::new().read(true).open(source.as_ref()).await?;
    let mut file = LinesStream::new(BufReader::new(file).lines()).peekable();

    let energy_units = match energy_units {
        EnergyUnitsOrAuto::Auto => {
            let line = match file.peek().await {
                Some(Ok(val)) => Some(val),
                Some(Err(_)) | None => None,
            };

            let units = line.and_then(|line| {
                line.split_whitespace()
                    .find_map(|word| EnergyUnits::from_str(word.trim_matches(',')))
            });

            match units {
                Some(units) => units,
                None => {
                    diagnostics.write_log_background(warn!(
                        "No energy units found for file `{}`, assuming eV",
                        source.as_ref().display()
                    ).with_sublog(info!("Units are deduced from instances of 'eV' or 'Ha' in the first line of the file")));

                    EnergyUnits::ElectronVolt
                }
            }
        }
        _ => energy_units.non_auto(),
    };

    let cs_units = match cs_units {
        CsUnitsOrAuto::Auto => {
            let line = match file.peek().await {
                Some(Ok(val)) => Some(val),
                Some(Err(_)) | None => None,
            };

            let units = line.and_then(|line| {
                line.split_whitespace()
                    .find_map(|word| CsUnits::from_str(word.trim_matches(',')))
            });

            match units {
                Some(units) => units,
                None => {
                    diagnostics.write_log_background(warn!(
                        "No cross section units found for file `{}`, assuming au",
                        source.as_ref().display()
                    ).with_sublog(info!("Units are deduced from instances of 'a0^2', 'au', 'cm^2' or 'm^2' in the first line of the file")));

                    CsUnits::Atomic
                }
            }
        }
        _ => cs_units.non_auto(),
    };

    let mut energy_cs = BTreeMap::new();

    let mut line_count = 0;

    loop {
        let line = match file.next().await {
            Some(val) => val?,
            None => break,
        };

        line_count += 1;

        // Skip whitespace only, or starts with #
        if line
            .split_whitespace()
            .next()
            .map(|first_column| first_column.starts_with("#"))
            .unwrap_or(true)
        {
            continue;
        }

        let mut cols = line.split_whitespace();

        let energy = cols.next().unwrap(); // Should be guaranteed by the check above.

        let energy = match energy.parse::<f64>() {
            Ok(energy) => energy,
            Err(err) => {
                let snippet = Snippet::empty(Some(source.as_ref().display().to_string()))
                    .with_chunk(
                        SnippetChunk::from_str(&line, SnippetLineKind::Normal, line_count)
                            .with_highlight_auto(
                                line_count,
                                energy,
                                format!("{}", err),
                                LineHighlightTheme::ERROR,
                            ),
                    )
                    .with_footer(Footer::new(
                        "The first two columns are parsed as `<ENERGY> <CROSS-SECTION>`".to_owned(),
                        FooterKind::Note,
                    ))
                    .with_footer(Footer::new(
                        "Example valid lines `10.0 1.23` or `1E1 1.23E0`".to_owned(),
                        FooterKind::Note,
                    ));
                diagnostics.write_log_background(
                    error!("Failed to parse `{}` as energy", energy)
                        .with_component(LogComponent::Snippet(snippet)),
                );
                continue;
            }
        };

        let cs = match cols.next() {
            Some(cs) => cs,
            None => {
                let snippet = Snippet::empty(Some(source.as_ref().display().to_string()))
                    .with_chunk(
                        SnippetChunk::from_str(&line, SnippetLineKind::Normal, line_count)
                            .with_highlight(
                                line_count,
                                LineHighlight::new(
                                    line.width(),
                                    1,
                                    "Expected cross section after energy".to_owned(),
                                    LineHighlightTheme::ERROR,
                                ),
                            ),
                    )
                    .with_footer(Footer::new(
                        "The first two columns are parsed as `<ENERGY> <CROSS-SECTION>`".to_owned(),
                        FooterKind::Note,
                    ))
                    .with_footer(Footer::new(
                        "Example valid lines `10.0 1.23` or `1E1 1.23E0`".to_owned(),
                        FooterKind::Note,
                    ));
                diagnostics.write_log_background(
                    error!("Expected cross section").with_component(LogComponent::Snippet(snippet)),
                );
                continue;
            }
        };

        let cs = match cs.parse::<f64>() {
            Ok(cs) => cs,
            Err(err) => {
                let snippet = Snippet::empty(Some(source.as_ref().display().to_string()))
                    .with_chunk(
                        SnippetChunk::from_str(&line, SnippetLineKind::Normal, line_count)
                            .with_highlight_auto(
                                line_count,
                                cs,
                                format!("{}", err),
                                LineHighlightTheme::ERROR,
                            ),
                    );
                diagnostics.write_log_background(
                    warn!("Failed to parse `{}` as cross section", cs)
                        .with_component(LogComponent::Snippet(snippet)),
                );
                continue;
            }
        };

        // Unit conversion
        let cs_au = cs * CsUnits::conversion_factor(cs_units, CsUnits::Atomic);
        let energy_ha = energy * EnergyUnits::conversion_factor(energy_units, EnergyUnits::Hartree);

        // Insert sorted
        if let Some(_) = energy_cs.insert(OrderedFloat(energy_ha), cs_au) {
            diagnostics.write_log_background(
                warn!(
                    "Duplicate cross section found in `{}` for energy `{}`",
                    source.as_ref().display(),
                    energy
                )
                .with_sublog(info!("Line: `{}`", line)),
            );
        }
    }

    let mut energy_vec = Vec::with_capacity(energy_cs.len());
    let mut cs_vec = Vec::with_capacity(energy_cs.len());

    energy_cs.into_iter().for_each(|(OrderedFloat(k), v)| {
        energy_vec.push(k);
        cs_vec.push(v);
    });

    Ok((energy_vec, cs_vec))
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

        Ok(ron::from_str::<ConfigSerde>(&buf)
            .map(ConfigSerde::to_config)
            .map_err(|err| {
                let mut buf_lines = buf.lines();
                let start_line = err.span.start.line.checked_sub(3).unwrap_or(0) + 1;
                for _ in 0..start_line - 1 {
                    buf_lines.next();
                }
                let mut lines = String::new();
                for _ in start_line..=err.span.end.line + 2 {
                    if let Some(line) = buf_lines.next() {
                        lines.push_str(line);
                        lines.push('\n');
                    }
                }
                CalcCmdError::ConfigParse {
                    file_path: file_path.as_ref().display().to_string(),
                    start_line: start_line,
                    lines,
                    err,
                }
            })?)
    }
}
