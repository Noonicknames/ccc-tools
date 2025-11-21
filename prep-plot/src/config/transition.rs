use std::fmt::Display;

use serde::{Deserialize, Serialize};

use crate::config::{SingleState, State};

#[derive(Clone, Default, Debug, Hash, PartialEq, Eq)]
pub struct Transition {
    pub to: State,
    pub from: State,
}

impl Transition {
    pub fn new(to: impl TryInto<State>, from: impl TryInto<State>) -> Result<Self, ()> {
        Ok(Self {
            to: to.try_into().map_err(|_| ())?,
            from: from.try_into().map_err(|_| ())?,
        })
    }

    pub fn from_str(str: &str) -> Result<Self, String> {
        if !str.is_ascii() {
            return Err(String::from("String provided must be ascii."));
        }

        let start_state;
        let final_state;

        'a: {
            if let Some(separator) = str.find("->") {
                start_state = State::from_str(str[..separator].trim()).map_err(|why| {
                    format!(
                        "Start state '{}' was invalid: {}",
                        str[..separator].trim(),
                        why
                    )
                })?;
                final_state = State::from_str(str[separator + 2..].trim()).map_err(|why| {
                    format!(
                        "Final state '{}' was invalid: {}",
                        str[separator + 2..].trim(),
                        why
                    )
                })?;
                break 'a;
            }
            if let Some(separator) = str.find("<-") {
                final_state = State::from_str(str[..separator].trim()).map_err(|why| {
                    format!(
                        "Final state '{}' was invalid: {}",
                        str[..separator].trim(),
                        why
                    )
                })?;
                start_state = State::from_str(str[separator + 2..].trim()).map_err(|why| {
                    format!(
                        "Start state '{}' was invalid: {}",
                        str[separator + 2..].trim(),
                        why
                    )
                })?;
                break 'a;
            };
            return Err(String::from("No separator '<-' or '->' was found."));
        }

        Ok(Self {
            to: final_state,
            from: start_state,
        })
    }
}

impl Display for Transition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} <- {}", self.to, self.from)
    }
}

impl Serialize for Transition {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&format!("{}<-{}", self.to, self.from))
    }
}

impl<'a> Deserialize<'a> for Transition {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'a>,
    {
        struct TransitionStrVisitor;

        impl<'a> serde::de::Visitor<'a> for TransitionStrVisitor {
            type Value = Transition;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("3 letter state string e.g. s2P")
            }
            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Transition::from_str(
                    std::str::from_utf8(v).map_err(|_| E::custom("Not a valid utf8 string"))?,
                )
                .map_err(|why| E::custom(why))
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Transition::from_str(v).map_err(|why| E::custom(why))
            }
        }
        deserializer.deserialize_str(TransitionStrVisitor)
    }
}

impl From<SingleTransition> for Transition {
    fn from(value: SingleTransition) -> Self {
        Self {
            to: value.to.into(),
            from: value.from.into(),
        }
    }
}

#[derive(Default, Debug, Clone, Hash, PartialEq, Eq)]
pub struct SingleTransition {
    pub to: SingleState,
    pub from: SingleState,
}

impl SingleTransition {
    pub fn new(to: impl TryInto<SingleState>, from: impl TryInto<SingleState>) -> Result<Self, ()> {
        Ok(Self {
            to: to.try_into().map_err(|_| ())?,
            from: from.try_into().map_err(|_| ())?,
        })
    }

    pub fn is_valid_transition_str(transition: &str) -> bool {
        // Try convention final<-initial with any whitespace.
        let mut states = transition.split("<-");
        if let Some((final_state, initial_state)) = states.next().zip(states.next()) {
            let final_state = final_state.trim();
            let initial_state = initial_state.trim();

            if SingleState::is_valid_state_str(final_state) && SingleState::is_valid_state_str(initial_state) {
                return true;
            }
        }

        // Try convention initial->final with any whitespace
        let mut states = transition.split("->");
        if let Some((initial_state, final_state)) = states.next().zip(states.next()) {
            let final_state = final_state.trim();
            let initial_state = initial_state.trim();

            if SingleState::is_valid_state_str(final_state) && SingleState::is_valid_state_str(initial_state) {
                return true;
            }
        }

        false
    }

    pub fn from_str(str: &str) -> Result<Self, String> {
        if !str.is_ascii() {
            return Err(String::from("String provided must be ascii."));
        }

        let start_state;
        let final_state;

        'a: {
            if let Some(separator) = str.find("->") {
                start_state = SingleState::from_str(str[..separator].trim()).map_err(|why| {
                    format!(
                        "Start state '{}' was invalid: {}",
                        str[..separator].trim(),
                        why
                    )
                })?;
                final_state =
                    SingleState::from_str(str[separator + 2..].trim()).map_err(|why| {
                        format!(
                            "Final state '{}' was invalid: {}",
                            str[separator + 2..].trim(),
                            why
                        )
                    })?;
                break 'a;
            }
            if let Some(separator) = str.find("<-") {
                final_state = SingleState::from_str(str[..separator].trim()).map_err(|why| {
                    format!(
                        "Final state '{}' was invalid: {}",
                        str[..separator].trim(),
                        why
                    )
                })?;
                start_state =
                    SingleState::from_str(str[separator + 2..].trim()).map_err(|why| {
                        format!(
                            "Start state '{}' was invalid: {}",
                            str[separator + 2..].trim(),
                            why
                        )
                    })?;
                break 'a;
            };
            return Err(String::from("No separator '<-' or '->' was found."));
        }

        Ok(Self {
            to: final_state,
            from: start_state,
        })
    }
}

impl TryFrom<Transition> for SingleTransition {
    type Error = ();
    fn try_from(value: Transition) -> Result<Self, Self::Error> {
        Ok(Self {
            to: value.to.try_into()?,
            from: value.from.try_into()?,
        })
    }
}
