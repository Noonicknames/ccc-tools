use std::{
    ops::{Bound, Range, RangeBounds},
    path::PathBuf,
};

use serde::{Deserialize, Serialize};

use crate::{
    config::{
        CollisionRateUnits, CsUnitsOrAuto, EnergyUnitsOrAuto, RangeOrCountSerde, RangeSerde,
        TemperatureUnits,
    },
    integrate::IntegrationKind,
};

/// User facing struct which is directly serialized and deserialized.
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct ConfigSerde {
    /// Result sets for which to calculate rates for.
    pub result_sets: Vec<ResultSetSerde>,
    /// Temperatures to calculate collision rates for.
    pub temperatures: Vec<TemperatureGridPoints>,
    /// Units for temperatures in `temperatures` field.
    pub temperature_units: TemperatureUnits,
    /// Default behaviour for outputting integrands if unspecified in a result set.
    #[serde(default)]
    pub output_integrands: bool,
    /// Default energy unit to assume a file is in.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub energy_units: Option<EnergyUnitsOrAuto>,
    /// Default cross section unit to assume a file is in.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cs_units: Option<CsUnitsOrAuto>,
    /// Folder to write results to.
    #[serde(default = "default_output_folder")]
    pub output_folder: String,
    /// Units for collision rate to output.
    pub collision_rate_units: CollisionRateUnits,
}

fn default_output_folder() -> String {
    "./calc-rates-out".to_string()
}

/// Provides configuration data for calculating one set of (temperature, rate) results.
///
/// This is the user facing variant of this struct, see [ResultSet].
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct ResultSetSerde {
    pub name: String,
    pub source: String,
    pub grid: Vec<IntegrationGridPoints>,
    pub energy_units: EnergyUnitsOrAuto,
    pub cs_units: CsUnitsOrAuto,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub output_integrands: Option<bool>,
}

impl ResultSetSerde {
    pub fn to_result_set(self, output_integrands: bool) -> ResultSet {
        ResultSet {
            name: self.name,
            source: PathBuf::from(self.source),
            grid: self.grid,
            energy_units: self.energy_units,
            cs_units: self.cs_units,
            output_integrands: self.output_integrands.unwrap_or(output_integrands),
        }
    }
}

impl ConfigSerde {
    pub fn template() -> Self {
        Self {
            result_sets: vec![
                ResultSetSerde {
                    name: "s2P<-s2S".to_owned(),
                    source: "ics.s2P.s2S".to_owned(),
                    grid: vec![IntegrationGridPoints::NaturalCubic(
                        RangeSerde::RangeFull.into(),
                    )],
                    cs_units: CsUnitsOrAuto::Auto,
                    energy_units: EnergyUnitsOrAuto::Auto,
                    output_integrands: None,
                },
                ResultSetSerde {
                    name: "s2P<-s2S(manual)".to_owned(),
                    source: "ics.s2P.s2S".to_owned(),
                    grid: vec![IntegrationGridPoints::NaturalCubic(
                        RangeSerde::RangeFull.into(),
                    )],
                    cs_units: CsUnitsOrAuto::Atomic,
                    energy_units: EnergyUnitsOrAuto::ElectronVolt,
                    output_integrands: None,
                },
            ],
            output_integrands: false,
            temperatures: vec![TemperatureGridPoints::Direct(vec![1.0, 2.0, 3.0, 4.0, 5.0])],
            temperature_units: TemperatureUnits::ElectronVolt,
            energy_units: None,
            cs_units: None,
            output_folder: default_output_folder(),
            collision_rate_units: CollisionRateUnits::Atomic,
        }
    }

    pub fn to_config(self) -> Config {
        Config {
            result_sets: self
                .result_sets
                .into_iter()
                .map(|result_set| result_set.to_result_set(self.output_integrands))
                .collect(),
            temperatures: TemperatureGridPoints::to_points_slice(&self.temperatures),
            temperature_units: self.temperature_units,
            energy_units: self.energy_units.unwrap_or(EnergyUnitsOrAuto::Auto),
            cs_units: self.cs_units.unwrap_or(CsUnitsOrAuto::Auto),
            output_folder: PathBuf::from(self.output_folder),
            collision_rate_units: self.collision_rate_units,
        }
    }
}

/// Integration grid points.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum IntegrationGridPoints {
    NaturalCubic(RangeOrCountSerde<usize>),
    AutoGauss(RangeOrCountSerde<usize>),
    Gauss(RangeOrCountSerde<usize>),

    Repeat(Box<IntegrationGridPoints>, usize),
}

impl IntegrationGridPoints {
    pub fn kind(&self) -> IntegrationKind {
        match self {
            Self::AutoGauss(..) => IntegrationKind::AutoGauss,
            Self::Gauss(..) => IntegrationKind::AutoGauss,
            Self::NaturalCubic(..) => IntegrationKind::NaturalCubic,
            Self::Repeat(inner, ..) => inner.kind(),
        }
    }

    pub fn repeats(&self) -> usize {
        match self {
            Self::Repeat(inner, n) => *n as usize * inner.repeats(),
            _ => 1,
        }
    }

    pub fn repeats_inner(&self) -> (usize, &Self) {
        match self {
            Self::Repeat(inner, n) => {
                let (n_inner, inner) = inner.repeats_inner();
                (*n as usize * n_inner, inner)
            }
            _ => (1, self),
        }
    }

    /// Gets the range of indexes of points.
    pub fn try_range(&self, constraint: Range<usize>) -> Option<Range<usize>> {
        if let IntegrationGridPoints::AutoGauss(range)
        | IntegrationGridPoints::NaturalCubic(range)
        | IntegrationGridPoints::Gauss(range) = self
        {
            return Some(match range {
                &RangeOrCountSerde::Count(n) => {
                    constraint.start..(constraint.start + n).min(constraint.end)
                }
                RangeOrCountSerde::Range(range) => {
                    let start = match range.start_bound() {
                        Bound::Excluded(&start) => (start + 1).max(constraint.start),
                        Bound::Included(&start) => start.max(constraint.start),
                        Bound::Unbounded => constraint.start,
                    };
                    let end = match range.end_bound() {
                        Bound::Excluded(&end) => end.min(constraint.end),
                        Bound::Included(&end) => (end + 1).min(constraint.end),
                        Bound::Unbounded => constraint.end,
                    };
                    start..end
                }
            });
        }

        None
    }

    pub fn flat_iter(slice: &[Self]) -> impl Iterator<Item = &Self> {
        slice.iter().flat_map(|grid_pts| {
            let (n, grid_pts) = grid_pts.repeats_inner();
            (0..n).map(move |_| grid_pts)
        })
    }
}

/// [ConfigSerde] but with some user conveniences removed.
#[derive(Debug, Default)]
pub struct Config {
    pub result_sets: Vec<ResultSet>,
    pub temperatures: Vec<f64>,
    pub temperature_units: TemperatureUnits,
    pub energy_units: EnergyUnitsOrAuto,
    pub cs_units: CsUnitsOrAuto,
    pub output_folder: PathBuf,
    pub collision_rate_units: CollisionRateUnits,
}

/// Provides configuration data for calculating one set of (temperature, rate) results.
#[derive(Debug, Default)]
pub struct ResultSet {
    pub name: String,
    pub source: PathBuf,
    pub grid: Vec<IntegrationGridPoints>,
    pub energy_units: EnergyUnitsOrAuto,
    pub cs_units: CsUnitsOrAuto,
    pub output_integrands: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum TemperatureGridPoints {
    Direct(Vec<f64>),
    Sequence { start: f64, end: f64, n: usize },
    StepSequence { start: f64, step: f64, end: f64 },
}

impl TemperatureGridPoints {
    pub fn to_points_slice(slice: &[Self]) -> Vec<f64> {
        let mut points = Vec::new();

        for grid_points in slice.iter() {
            grid_points.write_points(&mut points);
        }

        points
    }
    /// Write points
    pub fn write_points(&self, writer: &mut Vec<f64>) {
        match self {
            Self::Direct(points) => {
                writer.extend_from_slice(&points);
            }
            &Self::Sequence { start, end, n } => {
                writer.reserve(n as usize);
                if n == 1 {
                    writer.push((start + end) / 2.0)
                } else {
                    for i in 0..n {
                        let val = i as f64 / (n - 1) as f64 * (end - start) + start;
                        writer.push(val);
                    }
                }
            }
            &Self::StepSequence { start, step, end } => {
                let mut val = start;
                while val < end {
                    writer.push(val);
                    val += step;
                }
            }
        }
    }
}

pub fn integration_grid_to_points(
    grid: &[IntegrationGridPoints],
    points_count: usize,
) -> Vec<(IntegrationKind, Range<usize>)> {
    let mut ptr = 0;

    let mut result = Vec::new();

    for grid_pts in IntegrationGridPoints::flat_iter(grid) {
        let kind = grid_pts.kind();
        let range = grid_pts.try_range(ptr..points_count).unwrap();
        ptr = range.end;

        result.push((kind, range));
        if ptr == points_count {
            break;
        }
    }

    result
}

pub fn flatten_integration_grid(
    grid: &mut Vec<IntegrationGridPoints>,
) -> Vec<IntegrationGridPoints> {
    let mut result = Vec::new();

    for grid_pts in grid.iter() {
        match grid_pts {
            IntegrationGridPoints::Repeat(grid_pts, n) => {
                let mut grid_pts = grid_pts.as_ref().clone();
                let mut n = *n;
                while let IntegrationGridPoints::Repeat(grid_pts_inner, n_inner) = &grid_pts {
                    n = *n_inner;
                    grid_pts = grid_pts_inner.as_ref().clone();
                }

                for _ in 0..n {
                    result.push(grid_pts.clone());
                }
            }
            grid => result.push(grid.clone()),
        }
    }

    result
}
