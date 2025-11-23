use std::ops::RangeInclusive;

use serde::{Deserialize, Serialize};

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct ConfigSerde {
    pub result_sets: Vec<ResultSet>,
    pub temperatures: Vec<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub energy_units: Option<EnergyUnitsOrAuto>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cs_units: Option<CsUnitsOrAuto>,
}

impl ConfigSerde {
    pub fn template() -> Self {
        Self {
            result_sets: vec![ResultSet {
                name: "s2P<-s2S".to_owned(),
                source: "ics.s2P.s2S".to_owned(),
                grid: GridConfig::Auto,
                cs_units: CsUnitsOrAuto::Auto,
                energy_units: EnergyUnitsOrAuto::Auto,
            }],
            temperatures: vec![1.0, 2.0, 3.0, 4.0, 5.0],
            energy_units: None,
            cs_units: None,
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Config {
    pub result_sets: Vec<ResultSet>,
    pub temperatures: Vec<f64>,
    pub energy_units: EnergyUnitsOrAuto,
    pub cs_units: CsUnitsOrAuto,
}

#[derive(Clone, Copy, Debug, Default, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum CsUnitsOrAuto {
    // Automatically figure out the units from the header of a result file
    #[default]
    Auto,
    MetreSquared,
    CentimetreSquared,
    Atomic,
}

#[derive(Debug, Default, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum CsUnits {
    MetreSquared,
    CentimetreSquared,
    #[default]
    Atomic,
}

#[derive(Debug, Default, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum EnergyUnitsOrAuto {
    // Automatically figure out the units from the header of a result file
    #[default]
    Auto,
    /// Hartrees
    Atomic,
    ElectronVolt,
}

#[derive(Debug, Default, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum EnergyUnits {
    /// Hartrees
    Atomic,
    #[default]
    ElectronVolt,
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct ResultSet {
    pub name: String,
    pub source: String,
    pub grid: GridConfig,
    pub energy_units: EnergyUnitsOrAuto,
    pub cs_units: CsUnitsOrAuto,
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub enum GridConfig {
    #[default]
    Auto,
    Manual(Vec<Grid>),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Grid {
    EnergyRange(RangeInclusive<f64>),
    PointsCount(u32),
}
