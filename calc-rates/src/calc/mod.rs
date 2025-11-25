use std::{collections::BTreeMap, path::Path, sync::Arc};

use diagnostics::{
    LogComponent,
    components::snippet::{
        Footer, FooterKind, LineHighlight, LineHighlightTheme, Snippet, SnippetChunk,
        SnippetLineKind,
    },
    diagnostics::AsyncDiagnostics,
    error, info, warn,
};
use nalgebra::DVector;
use ordered_float::OrderedFloat;
use tokio::{
    fs::OpenOptions,
    io::{AsyncBufReadExt, AsyncReadExt, BufReader},
};
use tokio_stream::{StreamExt, wrappers::LinesStream};
use unicode_width::UnicodeWidthStr;

use crate::{
    config::{
        Config, ConfigSerde, CsUnits, CsUnitsOrAuto, EnergyUnits, EnergyUnitsOrAuto, Grid,
        GridConfig, ResultSet,
    },
    grid::moment_fitted,
};

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
    let Config {
        result_sets,
        temperatures,
        energy_units,
        cs_units,
    } = get_config(path).await?;

    let temperatures = Arc::new(temperatures);

    for result_set in result_sets {
        let diagnostics_copy = Arc::clone(diagnostics);
        let temperatures = Arc::clone(&temperatures);
        if let Err(err) = process_result_set(result_set, temperatures, diagnostics_copy).await {
            diagnostics.write_log_background(error!("Error whilst reading result set: {}", err));
        }
    }

    Ok(())
}

async fn process_result_set(
    result_set: ResultSet,
    temperatures: Arc<Vec<f64>>,
    diagnostics: Arc<AsyncDiagnostics>,
) -> Result<(), std::io::Error> {
    let ResultSet {
        name,
        source,
        grid,
        energy_units,
        cs_units,
    } = result_set;

    let (energy_vec, cs_vec) = get_energy_cs(&source, energy_units, cs_units, &diagnostics).await?;

    if energy_vec.len() < 2 {
        diagnostics.write_log_background(warn!(
            "At least 2 cross sections are required, only {} found in `{}`",
            energy_vec.len(),
            source
        ));
        return Ok(());
    }

    let grid_idx = resolve_grid(&energy_vec, &grid, name, &diagnostics);
    let grid_cache = grid_idx
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
            let mut grid = moment_fitted(scaled_x_points.as_slice(), |i| 1.0 / (i + 1) as f64);
            grid.scale_x(x_range);
            grid.translate(&DVector::from_vec(vec![start])).unwrap();
            grid
        })
        .collect::<Vec<_>>();

    let mut collision_strength_vec = Vec::with_capacity(temperatures.len());

    for &temperature in temperatures.iter() {
        // Boltzmann constant in Hartree/K
        const BOLTZMANN: f64 = 3.1668e-6;
        // Integrand according to Joel
        // Atomic units kB = 1, T is in Kelvin, energy in hartree
        let integrand = energy_vec
            .iter()
            .zip(cs_vec.iter())
            .map(|(&energy, &cs)| cs * energy * (-energy / (temperature * BOLTZMANN)).exp())
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
            * (2.0 / (temperature * BOLTZMANN)).powf(3.0 / 2.0);

        collision_strength_vec.push(result);
    }

    println!("Grid: {:?}", grid_idx);
    println!("Collision rates: {:?}", collision_strength_vec);

    Ok(())
}

fn resolve_grid(
    energy_vec: &[f64],
    grid: &GridConfig,
    name: impl AsRef<str>,
    diagnostics: &Arc<AsyncDiagnostics>,
) -> Vec<usize> {
    match grid {
        GridConfig::Auto => todo!(),
        GridConfig::Manual(grid) => {
            let mut result = Vec::with_capacity(grid.len());
            let mut ptr = 0;

            'outer: for (i, grid) in grid.into_iter().enumerate() {
                match grid {
                    Grid::EnergyRange(range) => {
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
                                "grid entry {} (zero-indexed) is empty for result set `{}`",
                                i,
                                name.as_ref(),
                            ));
                        }
                    }
                    Grid::PointsCount(0 | 1) => {
                        diagnostics.write_log_background(warn!(
                            "grid entry {} (zero-indexed) for result set `{}` must have at least 2 or more points",
                            i, name.as_ref(),
                        ));
                    }
                    Grid::PointsCount(n) => {
                        if result.is_empty() {
                            result.push(0);
                        }
                        result.push(
                            (*result.last().unwrap() + *n as usize - 1).min(energy_vec.len() - 1),
                        )
                    }

                    Grid::ToEnd => {
                        if result.is_empty() {
                            result.push(0);
                        }
                        result.push(energy_vec.len() - 1)
                    }
                }
            }
            result
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
                    ).with_sublog(info!("Units are deduced from instances of 'eV' or 'ha' in the first line of the file")));

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
