use std::ops::RangeInclusive;

use serde::{Deserialize, Serialize};

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct ConfigSerde {
    pub result_sets: Vec<ResultSet>,
    pub temperatures: Vec<f64>,
    pub temperature_units: TemperatureUnits,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub energy_units: Option<EnergyUnitsOrAuto>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cs_units: Option<CsUnitsOrAuto>,
    #[serde(default = "default_output_folder")]
    pub output_folder: String,
}

fn default_output_folder() -> String {
    "./calc-rates-out".to_string()
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
            temperature_units: TemperatureUnits::ElectronVolt,
            energy_units: None,
            cs_units: None,
            output_folder: default_output_folder(),
        }
    }

    pub fn to_config(self) -> Config {
        Config {
            result_sets: self.result_sets,
            temperatures: self.temperatures,
            temperature_units: self.temperature_units,
            energy_units: self.energy_units.unwrap_or(EnergyUnitsOrAuto::Auto),
            cs_units: self.cs_units.unwrap_or(CsUnitsOrAuto::Auto),
            output_folder: self.output_folder,
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Config {
    pub result_sets: Vec<ResultSet>,
    pub temperatures: Vec<f64>,
    pub temperature_units: TemperatureUnits,
    pub energy_units: EnergyUnitsOrAuto,
    pub cs_units: CsUnitsOrAuto,
    pub output_folder: String,
}

/// Units of temperature
///
/// Other than the Kelvin, the temperatures with an energy unit refer to T=E/k_B.
#[derive(Clone, Copy, Debug, Default, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum TemperatureUnits {
    Kelvin,
    #[default]
    ElectronVolt,
    Hartree,
}

impl TemperatureUnits {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Kelvin => "K",
            Self::ElectronVolt => "eV",
            Self::Hartree => "Ha",
        }
    }
    /// Conversion factor F to convert from this kelvin to this unit.
    pub fn to_unit_factor(&self) -> f64 {
        self.to_kelvin_factor().recip()
    }

    /// Conversion factor F to convert to kelvin from this factor.
    ///
    /// This is the boltzmann factor for energy based units.
    pub fn to_kelvin_factor(&self) -> f64 {
        match self {
            Self::Kelvin => 1.0,
            Self::ElectronVolt => 1.0 / 8.6173e-5,
            Self::Hartree => 1.0 / 3.1668e-6,
        }
    }

    pub fn conversion_factor(from: Self, to: Self) -> f64 {
        from.to_kelvin_factor() * to.to_unit_factor()
    }
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
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::MetreSquared => "m^2",
            Self::Atomic => "a0^2",
            Self::CentimetreSquared => "cm^2",
        }
    }

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

    /// Conversion factor F to convert to metres from this unit.
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
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::ElectronVolt => "eV",
            Self::Hartree => "Ha",
        }
    }
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

    /// Conversion factor F to convert to hartree from this unit.
    pub fn to_hartree_factor(&self) -> f64 {
        match self {
            Self::Hartree => 1.0,
            Self::ElectronVolt => 1.0 / 27.211396641308,
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
    Manual(Vec<GridPoints>),
}

impl GridConfig {
    pub fn flatten(&mut self) {
        if let Self::Manual(current_grid) = self {
            let mut temp_grid = Vec::new();

            for grid_pts in current_grid.iter() {
                match grid_pts {
                    GridPoints::Repeat(grid_pts, n) => {
                        let mut grid_pts = grid_pts.as_ref().clone();
                        let mut n = *n;
                        while let GridPoints::Repeat(grid_pts_inner, n_inner) = &grid_pts {
                            n = *n_inner;
                            grid_pts = grid_pts_inner.as_ref().clone();
                        } 

                        for _ in 0..n {
                            temp_grid.push(grid_pts.clone());
                        }
                    }
                    grid => temp_grid.push(grid.clone()),
                }
            }

            *current_grid = temp_grid;
        }
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum GridPoints {
    Repeat(Box<GridPoints>, u32),
    EnergyRange(RangeInclusive<f64>),
    PointsCount(u32),
    ToEnd,
}
