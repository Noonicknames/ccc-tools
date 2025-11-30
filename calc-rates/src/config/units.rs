use serde::{Deserialize, Serialize};


/// Units of collision rate
///
/// Other than the Kelvin, the temperatures with an energy unit refer to T=E/k_B.
#[derive(Clone, Copy, Debug, Default, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum CollisionRateUnits {
    #[default]
    Atomic,
    CentimetreCubedPerSecond,
    MetreCubedPerSecond,
}

impl CollisionRateUnits {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Atomic => "a0^3/s",
            Self::CentimetreCubedPerSecond => "cm^3/s",
            Self::MetreCubedPerSecond => "m^3/s",
        }
    }

    /// Conversion factor F to convert from this kelvin to this unit.
    pub fn to_unit_factor(&self) -> f64 {
        self.to_atomic_factor().recip()
    }

    /// Conversion factor F to convert to kelvin from this factor.
    ///
    /// This is the boltzmann factor for energy based units.
    pub fn to_atomic_factor(&self) -> f64 {
        match self {
            Self::Atomic => 1.0,
            Self::CentimetreCubedPerSecond => {
                (2.418884e-17) / (5.29177210903e-11f64 * 100.0).powi(3)
            }
            Self::MetreCubedPerSecond => (2.418884e-17) / (5.29177210903e-11f64).powi(3),
        }
    }

    pub fn conversion_factor(from: Self, to: Self) -> f64 {
        from.to_atomic_factor() * to.to_unit_factor()
    }
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
