use std::path::PathBuf;

use serde::{Deserialize, Serialize};

use crate::config::{CollisionRateOrStrengthUnits, CollisionRateUnits, Config, CsUnitsOrAuto, EnergyUnits, EnergyUnitsOrAuto, IntegrationGridPoints, RangeSerde, ResultSet, TemperatureGridPoints, TemperatureUnits};


/// User facing struct which is directly serialized and deserialized.
///
/// This is the user facing variant of [Config].
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct ConfigSerde {
    /// Result sets for which to calculate rates for.
    pub result_sets: Vec<ResultSetSerde>,
    /// Temperatures to calculate collision rates for.
    pub temperatures: Vec<TemperatureGridPoints>,
    /// Units for temperatures in the `temperatures` field.
    pub temperature_units: TemperatureUnits,
    /// Default behaviour for outputting integrands if unspecified in a result set.
    #[serde(default)]
    pub output_integrands: bool,
    // /// Default energy unit to assume a file is in.
    // #[serde(default, skip_serializing_if = "Option::is_none")]
    // pub energy_units: Option<EnergyUnitsOrAuto>,
    // /// Default cross section unit to assume a file is in.
    // #[serde(default, skip_serializing_if = "Option::is_none")]
    // pub cs_units: Option<CsUnitsOrAuto>,
    /// Folder to write results to.
    #[serde(default = "default_output_folder")]
    pub output_folder: String,
    /// Units for collision rate to output.
    pub collision_rate_units: CollisionRateOrStrengthUnits,
}

fn default_output_folder() -> String {
    "./calc-rates-out".to_string()
}

/// Provides configuration data for calculating one set of (temperature, rate) results.
///
/// This is the user facing variant of [ResultSet].
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct ResultSetSerde {
    /// Name of result set, results will be outputted to `<output_folder>/<name>`
    pub name: String,
    /// Path to the data file which holds two column
    pub source: String,
    pub grid: Vec<IntegrationGridPoints>,
    pub energy_units: EnergyUnitsOrAuto,
    pub cs_units: CsUnitsOrAuto,
    pub calc_strength_ctx: Option<CalcStrengthContext>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub output_integrands: Option<bool>,
}

/// Additional data needed to calculate collision strength.
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct CalcStrengthContext {
    pub degeneracy: u32,
    pub threshold_energy: f64,
    pub threshold_energy_units: EnergyUnits,
}

impl ResultSetSerde {
    pub fn to_result_set(self, output_integrands: bool) -> ResultSet {
        ResultSet {
            name: self.name,
            source: PathBuf::from(self.source),
            grid: self.grid,
            energy_units: self.energy_units,
            cs_units: self.cs_units,
            calc_strength_ctx: self.calc_strength_ctx,
            output_integrands: self.output_integrands.unwrap_or(output_integrands),
        }
    }
}

impl ConfigSerde {
    /// Template which is created when calling the `new` command.
    pub fn template() -> Self {
        Self {
            result_sets: vec![
                ResultSetSerde {
                    name: "s2P<-s2S".to_owned(),
                    source: "ics.s2P.s2S".to_owned(),
                    grid: vec![IntegrationGridPoints::MonotoneCubic(
                        RangeSerde::RangeFull.into(),
                    )],
                    cs_units: CsUnitsOrAuto::Auto,
                    energy_units: EnergyUnitsOrAuto::Auto,
                    calc_strength_ctx: Some(CalcStrengthContext {
                        degeneracy: 1,
                        threshold_energy: 25.15,
                        threshold_energy_units: EnergyUnits::ElectronVolt,
                    }),
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
                    calc_strength_ctx: None,
                    output_integrands: None,
                },
            ],
            output_integrands: false,
            temperatures: vec![
                TemperatureGridPoints::Direct(vec![1.0, 2.0, 3.0, 4.0, 5.0]),
                TemperatureGridPoints::Sequence {
                    start: 6.0,
                    end: 9.0,
                    n: 4,
                },
                TemperatureGridPoints::LogSequence {
                    start: 10.0,
                    end: 1e+6,
                    n: 10,
                },
            ],
            temperature_units: TemperatureUnits::ElectronVolt,
            // energy_units: None,
            // cs_units: None,
            output_folder: default_output_folder(),
            collision_rate_units: CollisionRateUnits::Atomic.into(),
        }
    }

    /// Convert to [Config] which is more convenient for processing.
    ///
    /// Note that currently there is no backwards conversion.
    pub fn to_config(self) -> Config {
        Config {
            result_sets: self
                .result_sets
                .into_iter()
                .map(|result_set| result_set.to_result_set(self.output_integrands))
                .collect(),
            temperatures: TemperatureGridPoints::to_points_slice(&self.temperatures),
            temperature_units: self.temperature_units,
            // energy_units: self.energy_units.unwrap_or(EnergyUnitsOrAuto::Auto),
            // cs_units: self.cs_units.unwrap_or(CsUnitsOrAuto::Auto),
            output_folder: PathBuf::from(self.output_folder),
            collision_rate_units: self.collision_rate_units,
        }
    }
}