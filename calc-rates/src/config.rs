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
            result_sets: vec![
                ResultSet {
                    name: "s2P<-s2S".to_owned(),
                    source: "ics.s2P.s2S".to_owned(),
                    grid: GridConfig::Auto,
                    cs_units: CsUnitsOrAuto::Auto,
                    energy_units: EnergyUnitsOrAuto::Auto,
                },
                ResultSet {
                    name: "s2P<-s2S(manual)".to_owned(),
                    source: "ics.s2P.s2S".to_owned(),
                    grid: GridConfig::Manual(vec![]),
                    cs_units: CsUnitsOrAuto::Atomic,
                    energy_units: EnergyUnitsOrAuto::ElectronVolt,
                },
            ],
            temperatures: vec![1.0, 2.0, 3.0, 4.0, 5.0],
            energy_units: None,
            cs_units: None,
        }
    }

    pub fn to_config(self) -> Config {
        Config {
            result_sets: self.result_sets,
            temperatures: self.temperatures,
            energy_units: self.energy_units.unwrap_or(EnergyUnitsOrAuto::Auto),
            cs_units: self.cs_units.unwrap_or(CsUnitsOrAuto::Auto),
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

impl CsUnitsOrAuto {
    /// Return non auto version of self.
    ///
    /// # Panics
    /// This will panic if it is [EnergyUnitsOrAuto::Auto].
    pub fn non_auto(self) -> CsUnits {
        match self {
            Self::Atomic => CsUnits::Atomic,
            Self::CentimetreSquared => CsUnits::CentimetreSquared,
            Self::MetreSquared => CsUnits::MetreSquared,
            Self::Auto => panic!(),
        }
    }
}

#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum CsUnits {
    MetreSquared,
    CentimetreSquared,
    #[default]
    Atomic,
}

impl CsUnits {
    pub fn from_str(str: &str) -> Option<Self> {
        match str.to_lowercase().as_str() {
            "atomic" | "a0^2" | "au" => Some(Self::Atomic),
            "cm^2" | "centimetre^2" | "centimetresquared" | "centimetre squared" => {
                Some(Self::CentimetreSquared)
            }
            "m^2" | "metre^2" | "metresquared" | "metre squared" => Some(Self::MetreSquared),
            _ => None,
        }
    }

    /// Conversion factor F to convert from this metres to this unit.
    pub fn to_unit_factor(&self) -> f64 {
        self.to_metre_factor().recip()
    }

    /// Conversion factor F to convert from metres to this factor.
    pub fn to_metre_factor(&self) -> f64 {
        match self {
            CsUnits::Atomic => 2.8002852016E-21,
            CsUnits::CentimetreSquared => 1E-4,
            CsUnits::MetreSquared => 1.0,
        }
    }

    pub fn conversion_factor(from: Self, to: Self) -> f64 {
        from.to_metre_factor() * to.to_unit_factor()
    }
}

#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum EnergyUnitsOrAuto {
    // Automatically figure out the units from the header of a result file
    #[default]
    Auto,
    /// Hartrees
    Atomic,
    ElectronVolt,
}

impl EnergyUnitsOrAuto {
    /// Return non auto version of self.
    ///
    /// # Panics
    /// This will panic if it is [EnergyUnitsOrAuto::Auto].
    pub fn non_auto(self) -> EnergyUnits {
        match self {
            Self::Atomic => EnergyUnits::Hartree,
            Self::ElectronVolt => EnergyUnits::ElectronVolt,
            Self::Auto => panic!(),
        }
    }
}

#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum EnergyUnits {
    Hartree,
    #[default]
    ElectronVolt,
}

impl EnergyUnits {
    pub fn from_str(str: &str) -> Option<Self> {
        match str.to_lowercase().as_str() {
            "ha" | "hartree" => Some(Self::Hartree),
            "ev" | "electron volt" | "electronvolt" => Some(Self::ElectronVolt),
            _ => None,
        }
    }

    /// Conversion factor F to convert from this atomic to this unit.
    pub fn to_unit_factor(&self) -> f64 {
        self.to_hartree_factor().recip()
    }

    /// Conversion factor F to convert from hartree to this unit.
    pub fn to_hartree_factor(&self) -> f64 {
        match self {
            Self::Hartree => 1.0,
            Self::ElectronVolt => 1.0/27.211396641308,
        }
    }

    pub fn conversion_factor(from: Self, to: Self) -> f64 {
        from.to_hartree_factor() * to.to_unit_factor()
    }
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
    ToEnd,
}
