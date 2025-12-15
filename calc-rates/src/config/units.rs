use serde::{Deserialize, Serialize};

use crate::config::CalcStrengthContext;

/// Units of collision rate or strength.
///
/// Other than the Kelvin, the temperatures with an energy unit refer to T=E/k_B.
#[derive(Clone, Copy, Debug, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum CollisionRateOrStrengthUnits {
    Strength,
    #[serde(untagged)]
    Rate(CollisionRateUnits),
}

impl CollisionRateOrStrengthUnits {
    /// As an appropriate string literal e.g. `a0^3/s` for atomic units, will be `strength` if this is collision strength which is unitless.
    ///
    /// See [CollisionRateOrStrengthUnits::as_str] for details about when this is a collision rate unit.
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Strength => "strength",
            Self::Rate(rate_units) => rate_units.as_str(),
        }
    }

    pub fn is_strength(&self) -> bool {
        match self {
            Self::Strength => true,
            Self::Rate(..) => false,
        }
    }

    /// Return a closure that, when given the temperature will return the conversion factor from atomic collision rate to this unit.
    ///
    /// Temperature is expected to be given in Hartree/k_B and threshold energy is expected to be given in Hartrees.
    pub fn converter(&self, ctx: Option<CalcStrengthContext>) -> Option<impl Fn(f64) -> f64> {
        enum ConverterData {
            Strength {
                threshold_energy: f64,
                degeneracy: f64,
            },
            Rate {
                conversion: f64,
            },
        }

        let data = match self {
            Self::Rate(rate_units) => ConverterData::Rate {
                conversion: rate_units.to_unit_factor(),
            },
            Self::Strength => {
                let CalcStrengthContext {
                    degeneracy,
                    threshold_energy,
                    threshold_energy_units,
                } = ctx?;

                ConverterData::Strength {
                    threshold_energy: threshold_energy * threshold_energy_units.to_hartree_factor(),
                    degeneracy: degeneracy as f64,
                }
            }
        };

        Some(move |temperature: f64| -> f64 {
            match data {
                ConverterData::Rate { conversion } => conversion,
                ConverterData::Strength {
                    threshold_energy,
                    degeneracy,
                } => {
                    // Conversion factor from some desmos deriving https://www.desmos.com/calculator/ttaagvnwjb
                    temperature.sqrt()
                        * degeneracy as f64
                        * 0.398942280401
                        * (threshold_energy / temperature).exp()
                }
            }
        })
    }
}

impl From<CollisionRateUnits> for CollisionRateOrStrengthUnits {
    fn from(value: CollisionRateUnits) -> Self {
        Self::Rate(value)
    }
}

impl Default for CollisionRateOrStrengthUnits {
    fn default() -> Self {
        Self::Rate(CollisionRateUnits::default())
    }
}

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

impl TryFrom<CollisionRateOrStrengthUnits> for CollisionRateUnits {
    type Error = ();
    fn try_from(value: CollisionRateOrStrengthUnits) -> Result<Self, Self::Error> {
        match value {
            CollisionRateOrStrengthUnits::Rate(rate) => Ok(rate),
            CollisionRateOrStrengthUnits::Strength => Err(()),
        }
    }
}

impl CollisionRateUnits {
    /// As an appropriate string literal e.g. `a0^3/s` for atomic units.
    ///
    /// For all units,
    /// - [CollisionRateUnits::Atomic] => `a0^3/s`
    /// - [CollisionRateUnits::CentimetreCubedPerSecond] => `cm^3/s`
    /// - [CollisionRateUnits::MetreCubedPerSecond] => `m^3/s`
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Atomic => "a0^3/s",
            Self::CentimetreCubedPerSecond => "cm^3/s",
            Self::MetreCubedPerSecond => "m^3/s",
        }
    }

    /// Conversion factor to convert from atomic to this unit.
    ///
    /// # Example
    /// ```
    /// use calc_rates::config::CollisionRateUnits;
    ///
    /// let rate_atomic = 10.0;
    /// let rate_cm = rate_atomic * CollisionRateUnits::CentimetreCubedPerSecond.to_unit_factor();
    ///
    /// // 10 a0^3/s is roughly 6.126e-8 cm^3/s
    /// assert!((rate_cm - 6.126e-8).abs() < 1e-11);
    ///
    /// ```
    pub fn to_unit_factor(&self) -> f64 {
        self.to_atomic_factor().recip()
    }

    /// Conversion factor to convert to atomic units from this factor.
    ///
    /// This is the boltzmann factor for energy based units.
    ///
    /// # Example
    /// ```
    /// use calc_rates::config::CollisionRateUnits;
    ///
    /// let rate_cm = 6.126e-8;
    /// let rate_atomic = rate_cm * CollisionRateUnits::CentimetreCubedPerSecond.to_atomic_factor();
    ///
    /// // 6.126e-8 cm^3/s is roughly 10 a0^3/s
    /// assert!((rate_atomic - 10.0).abs() < 1e-3)
    /// ```
    pub fn to_atomic_factor(&self) -> f64 {
        match self {
            Self::Atomic => 1.0,
            Self::CentimetreCubedPerSecond => {
                (2.418884e-17) / (5.29177210903e-11f64 * 100.0).powi(3)
            }
            Self::MetreCubedPerSecond => (2.418884e-17) / (5.29177210903e-11f64).powi(3),
        }
    }

    /// Conversion factor to convert to atomic units from this factor.
    ///
    /// This is the boltzmann factor for energy based units.
    ///
    /// # Example
    /// ```
    /// use calc_rates::config::CollisionRateUnits;
    ///
    /// let convert_m_to_cm = CollisionRateUnits::conversion_factor(
    ///     CollisionRateUnits::MetreCubedPerSecond,
    ///     CollisionRateUnits::CentimetreCubedPerSecond,
    /// );
    /// let rate_m = 1.0;
    /// let rate_cm = rate_m * convert_m_to_cm;
    ///
    /// // There are 1e6 cm^3 in 1m^3
    /// assert!((rate_cm - 1e6).abs() < 1e-8)
    /// ```
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
    /// An appropriate string literal to serve as temperature units such as `K` in `325K`.
    ///
    /// For all units,
    /// - [TemperatureUnits::Hartree] => `Ha`
    /// - [TemperatureUnits::ElectronVolt] => `eV`
    /// - [TemperatureUnits::Kelvin] => `K`
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Kelvin => "K",
            Self::ElectronVolt => "eV",
            Self::Hartree => "Ha",
        }
    }
    /// Conversion factor to convert from kelvin to this unit.
    ///
    /// # Example
    /// ```
    /// use calc_rates::config::TemperatureUnits;
    ///
    /// let temperature_kelvin = 11604.525;
    /// let temperature_ev = temperature_kelvin * TemperatureUnits::ElectronVolt.to_unit_factor();
    ///
    /// // 11604.525K is about 1eV/kB
    /// assert!((temperature_ev - 1.0).abs() < 1e-5);
    ///
    /// ```
    pub fn to_unit_factor(&self) -> f64 {
        self.to_kelvin_factor().recip()
    }

    /// Conversion factor to convert to kelvin from this unit.
    ///
    /// This is the boltzmann constant for energy based units.
    ///
    /// # Example
    /// ```
    /// use calc_rates::config::TemperatureUnits;
    ///
    /// let temperature_ev = 1.0;
    /// let temperature_kelvin = temperature_ev * TemperatureUnits::ElectronVolt.to_kelvin_factor();
    ///
    /// // 1eV/kB is about 11604.525K
    /// assert!((temperature_kelvin - 11604.525).abs() < 1e-1);
    /// ```
    pub fn to_kelvin_factor(&self) -> f64 {
        match self {
            Self::Kelvin => 1.0,
            Self::ElectronVolt => 1.0 / 8.6173e-5,
            Self::Hartree => 1.0 / 3.1668e-6,
        }
    }

    /// Conversion factor to convert between temperature units.
    ///
    /// # Example
    /// ```
    /// use calc_rates::config::TemperatureUnits;
    ///
    /// let temperature_ha = 1.0;
    /// let temperature_ev = temperature_ha * TemperatureUnits::conversion_factor(
    ///     TemperatureUnits::Hartree,
    ///     TemperatureUnits::ElectronVolt,
    /// );
    ///
    /// // 1Ha/kB is about 27.2114eV/kB
    /// assert!((temperature_ev - 27.2114).abs() < 1e-4);
    /// ```
    pub fn conversion_factor(from: Self, to: Self) -> f64 {
        from.to_kelvin_factor() * to.to_unit_factor()
    }
}

/// Cross section units to assume a data file is in or automatically determine with [CsUnitsOrAuto::Auto].
///
/// # Auto
/// This may also be set to [CsUnitsOrAuto::Auto] which automatically figures out the units from the first line of the file.
/// For example, units are deduced as atomic units if instances of 'au' are found.
#[derive(Clone, Copy, Debug, Default, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum CsUnitsOrAuto {
    /// Automatically figure out the units from the first line of a result file
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

/// Cross section units.
#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum CsUnits {
    MetreSquared,
    CentimetreSquared,
    #[default]
    Atomic,
}

impl CsUnits {
    /// This cross section unit as a string literal that is appropriate to use as units such as `a0^2` in `1.23 a0^2`.
    ///
    /// For all units this is,
    /// - [CsUnits::MetreSquared] => `m^2`
    /// - [CsUnits::Atomic] => `a0^2`
    /// - [CsUnits::CentimetreSquared] => `cm^2`
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::MetreSquared => "m^2",
            Self::Atomic => "a0^2",
            Self::CentimetreSquared => "cm^2",
        }
    }

    /// Attempt to convert a string into an instance of [CsUnits].
    ///
    /// # Example
    /// ```
    /// use calc_rates::config::CsUnits;
    ///
    /// assert_eq!(CsUnits::from_str("cm^2"), Some(CsUnits::CentimetreSquared));
    /// assert_eq!(CsUnits::from_str("atomic"), Some(CsUnits::Atomic));
    /// assert_eq!(CsUnits::from_str("Steve Harvey"), None);
    /// assert_eq!(CsUnits::from_str("26m^2"), None);
    /// ```
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

    /// Conversion factor to convert from metres to this unit.
    ///
    /// # Example
    /// ```
    /// use calc_rates::config::CsUnits;
    ///
    /// let cs_metres = 1.0;
    /// let cs_centimetres = cs_metres * CsUnits::CentimetreSquared.to_unit_factor();
    ///
    /// // 1m^2 is 1e4cm^2
    /// assert!((cs_centimetres - 1e4).abs() < 1e-8);
    /// ```
    pub fn to_unit_factor(&self) -> f64 {
        self.to_metre_factor().recip()
    }

    /// Conversion factor to convert to metres from this unit.
    ///
    /// # Example
    /// ```
    /// use calc_rates::config::CsUnits;
    ///
    /// let cs_centimetres = 1e4;
    /// let cs_metres = cs_centimetres * CsUnits::CentimetreSquared.to_metre_factor();
    ///
    /// // 1m^2 is 1e4cm^2
    /// assert!((cs_metres - 1.0).abs() < 1e-8);
    /// ```
    pub fn to_metre_factor(&self) -> f64 {
        match self {
            CsUnits::Atomic => 2.8002852016e-21,
            CsUnits::CentimetreSquared => 1e-4,
            CsUnits::MetreSquared => 1.0,
        }
    }

    /// Conversion factor between two cross section units.
    ///
    /// # Example
    /// ```
    /// use calc_rates::config::CsUnits;
    ///
    /// let cs_centimetres = 2.8002852016e-17;
    /// let cs_atomic = cs_centimetres * CsUnits::conversion_factor(
    ///     CsUnits::CentimetreSquared,
    ///     CsUnits::Atomic,
    /// );
    ///
    /// // 2.8002852016e-17 cm^2 happens to be 1 a0^2
    /// assert!((cs_atomic - 1.0).abs() < 1e-6, "{}", cs_atomic);
    /// ```
    pub fn conversion_factor(from: Self, to: Self) -> f64 {
        from.to_metre_factor() * to.to_unit_factor()
    }
}

/// Energy units to assume a data file is in, or automatically deduce from the first line with [EnergyUnitsOrAuto::Auto].
///
///
#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum EnergyUnitsOrAuto {
    // Automatically figure out the units from the header of a result file
    #[default]
    Auto,
    /// Also known as Hartrees.
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

/// Units of energy.
#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum EnergyUnits {
    Hartree,
    #[default]
    ElectronVolt,
}

impl EnergyUnits {
    /// This energy unit as a string literal which is appropriate to be used as a unti such as `eV` in `12eV`.
    ///
    /// For all units this is,
    /// - [EnergyUnits::Hartree] => `Ha`
    /// - [EnergyUnits::ElectronVolt] => `eV`
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::ElectronVolt => "eV",
            Self::Hartree => "Ha",
        }
    }

    /// Attempt to parse an energy unit from a string.
    ///
    /// Note that this is insensitive to case.
    /// # Example
    /// ```
    /// use calc_rates::config::EnergyUnits;
    ///
    /// assert_eq!(EnergyUnits::from_str("Hartree"), Some(EnergyUnits::Hartree));
    /// assert_eq!(EnergyUnits::from_str("Ha"), Some(EnergyUnits::Hartree));
    /// assert_eq!(EnergyUnits::from_str("eV"), Some(EnergyUnits::ElectronVolt));
    /// assert_eq!(EnergyUnits::from_str("Steve Harvey"), None);
    /// assert_eq!(EnergyUnits::from_str("12eV"), None);
    /// ```
    pub fn from_str(str: &str) -> Option<Self> {
        match str.to_lowercase().as_str() {
            "ha" | "hartree" => Some(Self::Hartree),
            "ev" | "electron volt" | "electronvolt" => Some(Self::ElectronVolt),
            _ => None,
        }
    }

    /// Conversion factor to convert from Hartrees to this unit.
    ///
    /// # Example
    /// ```
    /// use calc_rates::config::EnergyUnits;
    ///
    /// let energy_ha = 1.0;
    /// let energy_ev = energy_ha * EnergyUnits::ElectronVolt.to_unit_factor();
    ///
    /// // 1 Ha happens to be 27.2114 eV
    /// assert!((energy_ev - 27.2114).abs() < 1e-4);
    /// ```
    pub fn to_unit_factor(&self) -> f64 {
        self.to_hartree_factor().recip()
    }

    /// Conversion factor to convert to Hartrees from this unit.
    ///
    /// # Example
    /// ```
    /// use calc_rates::config::EnergyUnits;
    ///
    /// let energy_ev = 27.2114;
    /// let energy_ha = energy_ev * EnergyUnits::ElectronVolt.to_hartree_factor();
    ///
    /// // 1 Ha happens to be 27.2114 eV
    /// assert!((energy_ha - 1.0).abs() < 1e-5);
    /// ```
    pub fn to_hartree_factor(&self) -> f64 {
        match self {
            Self::Hartree => 1.0,
            Self::ElectronVolt => 1.0 / 27.211396641308,
        }
    }

    /// Conversion factor to convert between energy units.
    ///
    /// Conversion factor to convert to Hartrees from this unit.
    ///
    /// # Example
    /// ```
    /// use calc_rates::config::EnergyUnits;
    ///
    /// let energy_ev = 27.2114;
    /// let energy_ha = energy_ev * EnergyUnits::conversion_factor(
    ///     EnergyUnits::ElectronVolt,
    ///     EnergyUnits::Hartree,
    /// );
    ///
    /// // 1 Ha happens to be 27.2114 eV
    /// assert!((energy_ha - 1.0).abs() < 1e-5);
    ///
    /// // Of course, converting to the same units yields 1.0;
    /// let conversion = EnergyUnits::conversion_factor(
    ///     EnergyUnits::ElectronVolt,
    ///     EnergyUnits::ElectronVolt,
    /// );
    /// assert_eq!(conversion, 1.0);
    /// ```
    pub fn conversion_factor(from: Self, to: Self) -> f64 {
        from.to_hartree_factor() * to.to_unit_factor()
    }
}
