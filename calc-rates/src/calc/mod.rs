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
use nalgebra::DVector;
use ordered_float::OrderedFloat;
use tokio::{
    fs::OpenOptions,
    io::{AsyncBufReadExt, AsyncReadExt, AsyncWriteExt, BufReader, BufWriter},
};
use tokio_stream::{StreamExt, wrappers::LinesStream};
use unicode_width::UnicodeWidthStr;

use crate::{
    config::{
        CollisionRateUnits, Config, ConfigSerde, CsUnits, CsUnitsOrAuto, EnergyUnits, EnergyUnitsOrAuto, IntGridConfig, IntGridPoints, ResultSet, TemperatureUnits
    },
    grid::{Grid, moment_fitted},
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
        collision_rate_units
    } = get_config(path).await?;

    let temperatures = Arc::new(temperatures);
    let output_folder = Arc::new(PathBuf::from(output_folder));

    if let Err(err) = ensure_folder_exists(output_folder.as_ref()).await {
        return Err(CalcCmdError::CreateResultFolder {
            path: output_folder.as_ref().clone(),
            io_err: err,
        });
    }

    for result_set in result_sets {
        let diagnostics_copy = Arc::clone(diagnostics);
        let temperatures = Arc::clone(&temperatures);
        let output_folder = Arc::clone(&output_folder);
        if let Err(err) = process_result_set(
            result_set,
            temperatures,
            temperature_units,
            collision_rate_units,
            output_folder,
            diagnostics_copy,
        )
        .await
        {
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
        mut grid,
        energy_units,
        cs_units,
    } = result_set;

    grid.flatten(); // Flatten for ease

    let (energy_vec, cs_vec) = get_energy_cs(&source, energy_units, cs_units, &diagnostics).await?;

    if energy_vec.len() < 2 {
        diagnostics.write_log_background(warn!(
            "At least 2 cross sections are required, only {} found in `{}`",
            energy_vec.len(),
            source
        ));
        return Ok(());
    }

    let (grid_idx, grid_cache) = resolve_grid(&energy_vec, &cs_vec, &grid, &name, &diagnostics);

    let mut rate_vec = Vec::with_capacity(temperatures.len());

    let temp_conversion =
        TemperatureUnits::conversion_factor(temperature_units, TemperatureUnits::Hartree);

    let rate_conversion = CollisionRateUnits::conversion_factor(CollisionRateUnits::Atomic, rate_units);

    for &temperature in temperatures.iter() {
        // Integrand according to Joel
        // Atomic units kB = 1, T is converted to Hartree, energy in hartree
        let integrand = energy_vec
            .iter()
            .zip(cs_vec.iter())
            .map(|(&energy, &cs)| cs * energy * (-energy / (temperature * temp_conversion)).exp())
            .collect::<Vec<_>>();

        let mut result = 0.0;

        for (window, grid) in grid_idx.windows(2).zip(grid_cache.iter()) {
            let &[istart, iend] = window else {
                unreachable!()
            };

            result += grid.eval(&integrand[istart..=iend]);
        }

        // Factor in front
        result = result
            * std::f64::consts::PI.sqrt().recip()
            * (2.0 / (temperature * temp_conversion)).powf(3.0 / 2.0);

        // Convert to desired units
        result = result * rate_conversion;

        rate_vec.push(result);
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
    write!(&mut data, "# Grid(Ha): [",)?;

    for i in 0..grid_idx.len() - 1 {
        write!(
            &mut data,
            "{:.3}-{:.3}({}pt),",
            energy_vec[grid_idx[i]],
            energy_vec[grid_idx[i + 1]],
            grid_idx[i + 1] - grid_idx[i] + 1
        )?;
    }
    write!(&mut data, "]\n")?;

    for (temperature, rate) in temperatures.iter().zip(rate_vec.iter()) {
        writeln!(&mut data, "{} {:E}", temperature, rate)?;
    }

    let mut file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open(output_folder.join(&name))
        .await?;

    file.write_all(&data).await?;

    Ok(())
}

fn auto_grid(
    x_points: &[f64],
    integrand: &[f64],
    mut error_threshold: impl FnMut(usize) -> f64,
    max_degree: usize,
) -> (Vec<usize>, Vec<Grid<f64>>) {
    let mut idx_result = Vec::new();
    idx_result.push(0);
    let mut grid_result = Vec::new();

    let epsilon = 1e-8;
    // let perturbed_x_points = x_points
    //     .iter()
    //     .map(|&x| x * (1.0 + epsilon))
    //     .collect::<Vec<_>>();

    let get_result = |istart: usize, iend: usize, x_points: &[f64]| -> (Grid<f64>, f64) {
        let x_points_temp = DVector::from_vec(x_points[istart..=iend].to_vec());
        let start = x_points[istart];
        let x_range = x_points[iend] - x_points[istart];

        let scaled_x_points = &x_points_temp.add_scalar(-start) / x_range;
        let mut grid = moment_fitted(scaled_x_points.as_slice(), |i| 1.0 / (i + 1) as f64);
        grid.scale_x(x_range);
        grid.translate(&DVector::from_vec(vec![start])).unwrap();
        let val = grid.eval(&integrand[istart..=iend]);
        (grid, val)
    };

    'outer: loop {
        let istart = *idx_result.last().unwrap();
        if istart == x_points.len() - 1 {
            break;
        }
        let max_degree = max_degree.min(x_points.len() - istart - 1);
        let mut prev_result = get_result(istart, istart + 1, x_points);

        for degree in 2..=max_degree {
            // Perturbing x positions, only checks if the rule is well behaved.
            // Ineffective against detecting issues with resonances.
            let result = get_result(istart, istart + degree, x_points);
            // let perturbed_result = get_result(istart, istart + degree, &perturbed_x_points);

            // let error = result.1 - perturbed_result.1;

            // println!(
            //     "{}: {:.5} vs {:.5} (perturbed), error: {} = {}%",
            //     degree,
            //     result.1,
            //     perturbed_result.1,
            //     error,
            //     error / result.1 * 100.0
            // );

            // // Predicted difference, for a safer integration against resonances.
            let predicted_diff = get_result(istart + degree - 1, istart + degree, x_points).1;
            let actual_diff = result.1 - prev_result.1;
            let error_percent = actual_diff / predicted_diff * 100.0 - 100.0;

            println!(
                "{}: {:.5} vs {:.5} (prev), predicted_diff {:.5}, actual_diff {:.5}, error: {}%",
                degree,
                result.1,
                prev_result.1,
                predicted_diff,
                actual_diff,
                actual_diff / predicted_diff * 100.0 - 100.0
            );

            if error_percent.abs() > error_threshold(degree) || prev_result.1 < 0.0 {
                println!("Degree {} unsatisfactory", degree);
                idx_result.push(istart + degree - 1);
                grid_result.push(prev_result.0);
                continue 'outer;
            }

            prev_result = result;
        }
        println!("All satisfactory, pushing maximum");
        idx_result.push(istart + max_degree);
        grid_result.push(prev_result.0);
    }
    (idx_result, grid_result)
}

fn resolve_grid(
    energy_vec: &[f64],
    cs_vec: &[f64],
    grid: &IntGridConfig,
    name: impl AsRef<str>,
    diagnostics: &Arc<AsyncDiagnostics>,
) -> (Vec<usize>, Vec<Grid<f64>>) {
    match grid {
        IntGridConfig::Auto => {
            // Basically the normal integrand but no Maxwellian decay factor, should be the most unstable.
            let integrand = energy_vec
                .iter()
                .zip(cs_vec.iter())
                .map(|(&energy, &cs)| energy * cs)
                .collect::<Vec<_>>();

            let error_threshold = |deg| 10.0 / (deg as f64 + 1.0).ln();

            let max_degree = 4;

            auto_grid(energy_vec, &integrand, error_threshold, max_degree)
        }
        IntGridConfig::Manual(grid) => {
            let mut result = Vec::with_capacity(grid.len());
            let mut ptr = 0;

            'outer: for (i, grid) in grid.into_iter().enumerate() {
                if ptr >= energy_vec.len() - 1 {
                    break 'outer;
                }
                match grid {
                    IntGridPoints::Repeat(..) => {
                        panic!("Please only feed in flattened grids without repeats.")
                    }
                    IntGridPoints::EnergyRange(range) => {
                        'inner: loop {
                            let Some(energy) = energy_vec.get(ptr) else {
                                break 'outer;
                            };
                            if energy >= range.start() {
                                break 'inner;
                            }
                            ptr += 1;
                        }
                        let start = ptr;

                        'inner: loop {
                            let Some(energy) = energy_vec.get(ptr) else {
                                ptr -= 1; // the start must exist so ptr >= 1
                                break 'inner;
                            };
                            if energy >= range.end() {
                                break 'inner;
                            }
                            ptr += 1;
                        }
                        if ptr > start {
                            result.push(start);
                            result.push(ptr);
                        } else {
                            diagnostics.write_log_background(warn!(
                                "Grid entry {} (zero-indexed) is empty for result set `{}`",
                                i,
                                name.as_ref(),
                            ));
                        }
                    }
                    IntGridPoints::PointsCount(0 | 1) => {
                        diagnostics.write_log_background(warn!(
                            "Grid entry {} (zero-indexed) for result set `{}` must have at least 2 or more points",
                            i, name.as_ref(),
                        ));
                    }
                    IntGridPoints::PointsCount(n) => {
                        if result.is_empty() {
                            result.push(0);
                        }
                        result.push(
                            (*result.last().unwrap() + *n as usize - 1).min(energy_vec.len() - 1),
                        );
                        ptr = *result.last().unwrap();
                    }

                    IntGridPoints::ToEnd => {
                        if result.is_empty() {
                            result.push(0);
                        }
                        result.push(energy_vec.len() - 1);
                        ptr = *result.last().unwrap();
                        ptr = *result.last().unwrap();
                    }
                }
            }

            let grid_result = result
                .windows(2)
                .map(|window| {
                    let &[istart, iend] = window else {
                        unreachable!()
                    };

                    let x_points = DVector::from_vec(energy_vec[istart..=iend].to_vec());
                    let start = energy_vec[istart];
                    let x_range = energy_vec[iend] - energy_vec[istart];

                    let scaled_x_points = &x_points.add_scalar(-start) / x_range;

                    // Integrating from 0.0 to 1.0
                    let mut grid =
                        moment_fitted(scaled_x_points.as_slice(), |i| 1.0 / (i + 1) as f64);
                    grid.scale_x(x_range);
                    grid.translate(&DVector::from_vec(vec![start])).unwrap();
                    grid
                })
                .collect::<Vec<_>>();
            (result, grid_result)
        }
    }
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
