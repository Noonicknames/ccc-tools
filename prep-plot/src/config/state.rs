use std::fmt::Display;

use arrayvec::ArrayString;

use crate::config::{AngularMomentumNum, PrincipleNum, SpinNum};

/// Describes a state or group of states
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum State {
    Single(SingleState),
    Ionized,
}

impl State {
    pub fn from_str(str: &str) -> Result<Self, String> {
        match str {
            "ion" => Ok(Self::Ionized),
            other => Ok(Self::Single(SingleState::from_str(other)?)),
        }
    }
    /// State string without the order suffix
    pub fn unordered_str(&self) -> ArrayString<3> {
        match self {
            Self::Ionized => ArrayString::from("ion").unwrap(),
            Self::Single(bound) => bound.unordered_str(),
        }
    }
}

impl From<SingleState> for State {
    fn from(value: SingleState) -> Self {
        Self::Single(value)
    }
}

impl Default for State {
    fn default() -> Self {
        Self::Single(Default::default())
    }
}

impl Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Ionized => write!(f, "ion"),
            Self::Single(state) => Display::fmt(state, f),
        }
    }
}

impl<'a> TryFrom<&'a str> for State {
    type Error = String;
    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        Self::from_str(value)
    }
}

#[derive(Clone, Hash, Default, PartialEq, Eq, Debug)]
pub struct SingleState {
    pub n: PrincipleNum,
    pub s: SpinNum,
    pub l: AngularMomentumNum,
    pub is_natural_parity: bool,
}

impl TryFrom<State> for SingleState {
    type Error = ();
    fn try_from(value: State) -> Result<Self, Self::Error> {
        match value {
            State::Single(state) => Ok(state),
            State::Ionized => Err(()),
        }
    }
}

impl SingleState {
    pub fn from_str(str: &str) -> Result<Self, String> {
        if !str.is_ascii() {
            return Err(String::from(
                "Strings which may be parsed into a State must be ASCII.",
            ));
        }

        let [s, n, l, ..] = str.as_bytes() else {
            return Err(String::from("State strings must atleast be of length 3."));
        };

        let Some((s, is_natural_parity)) = SpinNum::from_byte_char(*s) else {
            return Err(format!(
                "Invalid character '{}' for spin number.",
                *s as char
            ));
        };

        let Some(l) = AngularMomentumNum::from_byte_char(*l) else {
            return Err(format!(
                "Invalid character '{}' for angular momentum number.",
                *l as char
            ));
        };

        let order;

        if let Some('#') = str.chars().nth(3) {
            order = str[4..].parse().map_err(|why| format!("{}", why))?;
        } else {
            order = 0u16;
        };

        let Some(n) = PrincipleNum::from_byte_char(*n, order) else {
            return Err(format!(
                "Invalid character '{}' for principle number.",
                *n as char
            ));
        };

        Ok(Self {
            n,
            s,
            l,
            is_natural_parity,
        })
    }

    /// State string without the order suffix
    pub fn unordered_str(&self) -> ArrayString<3> {
        ArrayString::from_byte_string(&[
            self.s.as_byte_char(self.is_natural_parity),
            self.n.as_byte_char(),
            self.l.as_byte_char(),
        ])
        .unwrap()
    }
    /// Checks if a string is a bound state string.
    pub fn is_valid_state_str(val: &str) -> bool {
        if val.len() != 3 {
            return false;
        }
        let bytes = val.as_bytes();

        if bytes.len() != 3 {
            return false;
        }

        let s = bytes[0];
        let l = bytes[2];

        match s {
            b's' | b'S' | b't' | b'T' => (),
            _ => return false,
        }

        match l {
            b'S' | b'P' | b'D' | b'F' | b'G' | b'H' | b'I' | b'J' => (),
            _ => return false,
        }

        true
    }
}

impl Display for SingleState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.n.order() {
            0 => write!(f, "{}", self.unordered_str()),
            order => write!(f, "{}#{}", self.unordered_str(), order),
        }
    }
}

impl<'a> TryFrom<&'a str> for SingleState {
    type Error = String;
    fn try_from(value: &'a str) -> Result<Self, Self::Error> {
        Self::from_str(value)
    }
}

impl serde::Serialize for SingleState {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // There is an allocation here
        serializer.serialize_str(&self.to_string())
    }
}

impl<'a> serde::Deserialize<'a> for SingleState {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'a>,
    {
        struct SingleStateStrVisitor;

        impl<'a> serde::de::Visitor<'a> for SingleStateStrVisitor {
            type Value = SingleState;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(
                    formatter,
                    "3 letter state string e.g. 's2P' or with order suffix 's2P#0'"
                )
            }
            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                SingleState::from_str(
                    std::str::from_utf8(v).map_err(|_| E::custom("Not a valid utf8 string"))?,
                )
                .map_err(|_| E::custom("Not a valid state"))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                SingleState::from_str(v).map_err(|_| E::custom("Not a valid state"))
            }
        }
        deserializer.deserialize_str(SingleStateStrVisitor)
    }
}
