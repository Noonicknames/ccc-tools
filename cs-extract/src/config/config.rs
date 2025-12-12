use serde::{Deserialize, Serialize};
use std::{fmt::Debug, sync::Arc};

use crate::{config::Transition, extract::TransitionsSet, util::FilesSource};

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct ConfigSerde {
    pub result_sets: Vec<ResultSetSerde>,

    #[serde(default)]
    pub cs_kind: CsKind,
    #[serde(default)]
    pub transitions: Vec<Transition>,
    #[serde(default)]
    pub overwrite: bool,
    #[serde(default)]
    pub add_threshold_point: bool,
    #[serde(default)]
    pub units: Units,

    #[serde(default = "default_output_folder")]
    pub output_folder: String,
}

impl ConfigSerde {
    pub fn to_config(self) -> Config {
        let transitions = Arc::new(self.transitions.into_iter().collect::<TransitionsSet>());

        let result_sets = self
            .result_sets
            .into_iter()
            .map(|result_set| {
                result_set.to_result_set(
                    self.cs_kind,
                    self.units,
                    self.overwrite,
                    self.add_threshold_point,
                    &transitions,
                )
            })
            .collect();
        let output_folder = self.output_folder;
        Config {
            result_sets,
            output_folder,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Config {
    pub result_sets: Vec<ResultSet>,
    pub output_folder: String,
}

fn default_output_folder() -> String {
    "./extract-cs-out".to_string()
}

#[derive(Clone, Debug, Serialize, Deserialize, Default)]
pub struct ResultSetSerde {
    pub name: String,
    pub source: FilesSource,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub cs_kind: Option<CsKind>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub units: Option<Units>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub overwrite: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub add_threshold_point: Option<bool>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub transitions: Option<Vec<Transition>>,
}

impl ResultSetSerde {
    pub fn to_result_set(
        self,
        cs_kind: CsKind,
        units: Units,
        overwrite: bool,
        add_threshold_point: bool,
        transitions: &Arc<TransitionsSet>,
    ) -> ResultSet {
        ResultSet {
            name: self.name,
            source: self.source,
            cs_kind: self.cs_kind.unwrap_or(cs_kind),
            units: self.units.unwrap_or(units),
            overwrite: self.overwrite.unwrap_or(overwrite),
            add_threshold_point: self.add_threshold_point.unwrap_or(add_threshold_point),
            transitions: self
                .transitions
                .map(|transitions| Arc::new(transitions.into_iter().collect::<TransitionsSet>()))
                .unwrap_or_else(|| Arc::clone(transitions)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ResultSet {
    pub name: String,
    pub source: FilesSource,
    pub cs_kind: CsKind,
    pub units: Units,
    pub overwrite: bool,
    pub add_threshold_point: bool,
    pub transitions: Arc<TransitionsSet>,
}

/// Describes a kind of cross-section which may be extracted from a totalcs_*/totalcs_*_J file
#[derive(Clone, Copy, Debug, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum CsKind {
    Born {
        /// There are two columns for Born results, PCS and ICS.
        /// PCS is a cumulative sum over the partial waves currently run for.
        partial: bool,
    },
    Integrated {
        extrapolated: bool,
    },
    Partial {
        partial_wave: u32,
        column: PartialColumn,
    },
}
impl Default for CsKind {
    fn default() -> Self {
        Self::Integrated { extrapolated: true }
    }
}

impl CsKind {
    /// Gives a short string to name the kind of cross-section.
    pub const fn as_str(&self) -> &'static str {
        match self {
            Self::Born { partial: false } => "BornICS",
            Self::Born { partial: true } => "BornPCS",
            Self::Integrated {
                extrapolated: false,
            } => "ICS",
            Self::Integrated { extrapolated: true } => "ICS+",
            Self::Partial { column, .. } => column.as_str(),
        }
    }
}

/// Column of cross-sections in a totalcs_*_J file.
#[derive(Clone, Copy, Debug, Serialize, Deserialize, Default, Hash, PartialEq, Eq)]
pub enum PartialColumn {
    CrossSection,
    #[default]
    Extrapolated,
    V,
    T0,
    T1,
}

impl PartialColumn {
    /// Gives a good name string.
    pub const fn as_str(&self) -> &'static str {
        match self {
            PartialColumn::CrossSection => "PCS",
            PartialColumn::Extrapolated => "PCS (Extrap)",
            PartialColumn::V => "PCS(V)",
            PartialColumn::T0 => "PCS(T) S=0",
            PartialColumn::T1 => "PCS(T) S=1",
        }
    }

    /// Gets the column number out by whitespace.
    pub const fn col_num(&self) -> usize {
        match self {
            Self::CrossSection => 3,
            Self::Extrapolated => 4,
            Self::V => 5,
            Self::T0 => 6,
            Self::T1 => 7,
        }
    }
}

#[derive(Clone, Copy, Debug, Hash, Default, PartialEq, Eq, Serialize, Deserialize)]
pub enum Units {
    MetreSquared,
    CentimetreSquared,
    #[default]
    Atomic,
}

impl Units {
    pub fn from_str(val: &str) -> Option<Self> {
        match val {
            "cm^2" => Some(Self::CentimetreSquared),
            "a0^2" => Some(Self::Atomic),
            "m^2" => Some(Self::MetreSquared),
            _ => None,
        }
    }

    pub fn to_str(&self) -> &'static str {
        match self {
            Self::Atomic => "a0^2",
            Self::CentimetreSquared => "cm^2",
            Self::MetreSquared => "m^2",
        }
    }
    /// Conversion factor F to convert from this metres to this unit.
    pub fn to_unit_factor(&self) -> f64 {
        self.to_metre_factor().recip()
    }

    /// Conversion factor F to convert from metres to this factor.
    pub fn to_metre_factor(&self) -> f64 {
        match self {
            Units::Atomic => 2.8002852016E-21,
            Units::CentimetreSquared => 1E-4,
            Units::MetreSquared => 1.0,
        }
    }

    pub fn conversion_factor(from: Self, to: Self) -> f64 {
        from.to_metre_factor() * to.to_unit_factor()
    }
}

impl ConfigSerde {
    pub fn template() -> Self {
        Self {
            result_sets: vec![
                ResultSetSerde {
                    name: "single_folder_set".into(),
                    source: FilesSource::Folder("./folder_with_totalcs_files".into()),
                    ..Default::default()
                },
                ResultSetSerde {
                    name: "multi_folder_set".into(),
                    source: FilesSource::Folders(vec!["./folder_1".into(), "./folder_2".into()]),
                    ..Default::default()
                },
                ResultSetSerde {
                    name: "single_file_set".into(),
                    source: FilesSource::File("./totalcs_1.2300E+01_J".into()),
                    ..Default::default()
                },
            ],
            cs_kind: CsKind::Partial {
                partial_wave: 1,
                column: PartialColumn::T0,
            },
            output_folder: "./default-prep-plot".into(),
            transitions: vec![
                Transition::new("s2P", "s2S").unwrap(),
                Transition::new("s3S", "s2S").unwrap(),
            ],
            units: Units::Atomic,
            add_threshold_point: true,
            overwrite: false,
        }
    }
}
