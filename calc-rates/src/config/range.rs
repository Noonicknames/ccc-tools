use std::{
    fmt::{Display, Write},
    marker::PhantomData,
    ops::{Bound, RangeBounds},
    str::FromStr,
};

use serde::{Deserialize, Serialize, de::Visitor};

#[derive(Clone, Debug, Serialize)]
pub enum RangeOrCountSerde<T> {
    #[serde(untagged)]
    Count(T),
    #[serde(untagged, bound(serialize = "T: Display",))]
    Range(RangeSerde<T>),
}

impl<T> TryInto<RangeSerde<T>> for RangeOrCountSerde<T> {
    type Error = Self;
    fn try_into(self) -> Result<RangeSerde<T>, Self::Error> {
        match self {
            Self::Count(_) => Err(self),
            Self::Range(range) => Ok(range),
        }
    }
}

impl<T> Into<RangeOrCountSerde<T>> for RangeSerde<T> {
    fn into(self) -> RangeOrCountSerde<T> {
        RangeOrCountSerde::Range(self)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ParseRangeOrCountError<E> {
    #[error("Invalid range, missing `..` or `..=`")]
    NotARangeOrCount,
    #[error(transparent)]
    ParseType(#[from] E),
}

impl<T> FromStr for RangeOrCountSerde<T>
where
    T: FromStr,
{
    type Err = ParseRangeOrCountError<<T as FromStr>::Err>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(s) = s.parse::<RangeSerde<T>>() {
            return Ok(Self::Range(s));
        }

        Ok(Self::Count(s.parse::<T>()?))
    }
}

impl<'de, T> Deserialize<'de> for RangeOrCountSerde<T>
where
    T: FromStr,
    <T as FromStr>::Err: Display,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct RangeOrCountVisitor<T>(PhantomData<T>);

        impl<'de, T> Visitor<'de> for RangeOrCountVisitor<T>
        where
            T: FromStr,
            <T as FromStr>::Err: Display,
        {
            type Value = RangeOrCountSerde<T>;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "an integer range or integer")
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                v.parse::<RangeOrCountSerde<T>>()
                    .map_err(|err| E::custom(err))
            }
        }
        deserializer.deserialize_str(RangeOrCountVisitor(PhantomData))
    }
}

impl<T> std::fmt::Display for RangeOrCountSerde<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Count(count) => write!(f, "{}", count),
            Self::Range(range) => range.fmt(f),
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum RangeSerde<T> {
    RangeInclusive { start: T, end: T },
    Range { start: T, end: T },
    RangeFrom { start: T },
    RangeTo { end: T },
    RangeToInclusive { end: T },
    RangeFull,
}

impl<T> RangeBounds<T> for RangeSerde<T> {
    fn end_bound(&self) -> std::ops::Bound<&T> {
        match self {
            Self::Range { end, .. } | Self::RangeTo { end } => Bound::Excluded(end),
            Self::RangeFrom { .. } | Self::RangeFull => Bound::Unbounded,
            Self::RangeInclusive { end, .. } | Self::RangeToInclusive { end } => {
                Bound::Included(end)
            }
        }
    }

    fn start_bound(&self) -> std::ops::Bound<&T> {
        match self {
            Self::Range { start, .. }
            | Self::RangeInclusive { start, .. }
            | Self::RangeFrom { start } => Bound::Included(start),
            Self::RangeTo { .. } | Self::RangeToInclusive { .. } | Self::RangeFull => {
                Bound::Unbounded
            }
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ParseRangeError<E> {
    #[error("Invalid range, missing `..` or `..=`")]
    NotARange,
    #[error(transparent)]
    ParseType(#[from] E),
}
impl<T> FromStr for RangeSerde<T>
where
    T: FromStr,
{
    type Err = ParseRangeError<<T as FromStr>::Err>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        if let Some((start, end)) = s.split_once("..=") {
            match start {
                "" => {
                    return Ok(Self::RangeToInclusive {
                        end: end.parse::<T>()?,
                    });
                }
                start => {
                    return Ok(Self::RangeInclusive {
                        start: start.parse::<T>()?,
                        end: end.parse::<T>()?,
                    });
                }
            }
        }

        match s.split_once("..") {
            Some(("", "")) => {
                return Ok(Self::RangeFull);
            }
            Some((start, "")) => {
                return Ok(Self::RangeFrom {
                    start: start.parse::<T>()?,
                });
            }
            Some(("", end)) => {
                return Ok(Self::RangeTo {
                    end: end.parse::<T>()?,
                });
            }
            Some((start, end)) => {
                return Ok(Self::Range {
                    start: start.parse::<T>()?,
                    end: end.parse::<T>()?,
                });
            }
            None => (),
        }

        Err(ParseRangeError::NotARange)
    }
}

impl<T> std::fmt::Display for RangeSerde<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::RangeInclusive { start, end } => write!(f, "{}..={}", start, end),
            Self::Range { start, end } => write!(f, "{}..{}", start, end),
            Self::RangeFrom { start } => write!(f, "{}..", start),
            Self::RangeToInclusive { end } => write!(f, "..={}", end),
            Self::RangeTo { end } => write!(f, "..{}", end),
            Self::RangeFull => write!(f, ".."),
        }
    }
}

impl<T> Serialize for RangeSerde<T>
where
    T: Display,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut buf = String::new();
        unsafe { write!(&mut buf, "{}", self).unwrap_unchecked() };
        serializer.serialize_str(&buf)
    }
}

impl<'de, T> Deserialize<'de> for RangeSerde<T>
where
    T: FromStr,
    <T as FromStr>::Err: Display,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct RangeVisitor<T>(PhantomData<T>);

        impl<'de, T> Visitor<'de> for RangeVisitor<T>
        where
            T: FromStr,
            <T as FromStr>::Err: Display,
        {
            type Value = RangeSerde<T>;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "a range")
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                v.parse::<RangeSerde<T>>().map_err(|err| E::custom(err))
            }
        }

        deserializer.deserialize_str(RangeVisitor(PhantomData))
    }
}
