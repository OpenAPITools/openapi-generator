use std::{convert::TryFrom, fmt, ops::Deref};

use chrono::{DateTime, Utc};
use http::HeaderValue;

/// A struct to allow homogeneous conversion into a HeaderValue. We can't
/// implement the From/Into trait on HeaderValue because we don't own
/// either of the types.
#[derive(Debug, Clone)]
pub(crate) struct IntoHeaderValue<T>(pub T);

// Generic implementations

impl<T> Deref for IntoHeaderValue<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.0
    }
}

// Derive for each TryFrom<T> in http::HeaderValue

macro_rules! ihv_generate {
    ($t:ident) => {
        impl TryFrom<HeaderValue> for IntoHeaderValue<$t> {
            type Error = String;

            fn try_from(hdr_value: HeaderValue) -> Result<Self, Self::Error> {
                match hdr_value.to_str() {
                    Ok(hdr_value) => match hdr_value.parse::<$t>() {
                        Ok(hdr_value) => Ok(IntoHeaderValue(hdr_value)),
                        Err(e) => Err(format!(
                            "Unable to parse {} as a string: {}",
                            stringify!($t),
                            e
                        )),
                    },
                    Err(e) => Err(format!(
                        "Unable to parse header {:?} as a string - {}",
                        hdr_value, e
                    )),
                }
            }
        }

        impl TryFrom<IntoHeaderValue<$t>> for HeaderValue {
            type Error = String;

            fn try_from(hdr_value: IntoHeaderValue<$t>) -> Result<Self, Self::Error> {
                Ok(hdr_value.0.into())
            }
        }
    };
}

ihv_generate!(u64);
ihv_generate!(i64);
ihv_generate!(i16);
ihv_generate!(u16);
ihv_generate!(u32);
ihv_generate!(usize);
ihv_generate!(isize);
ihv_generate!(i32);

// Custom derivations

// Vec<String>

impl TryFrom<HeaderValue> for IntoHeaderValue<Vec<String>> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> Result<Self, Self::Error> {
        match hdr_value.to_str() {
            Ok(hdr_value) => Ok(IntoHeaderValue(
                hdr_value
                    .split(',')
                    .filter_map(|x| match x.trim() {
                        "" => None,
                        y => Some(y.to_string()),
                    })
                    .collect(),
            )),
            Err(e) => Err(format!(
                "Unable to parse header: {:?} as a string - {}",
                hdr_value, e
            )),
        }
    }
}

impl TryFrom<IntoHeaderValue<Vec<String>>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: IntoHeaderValue<Vec<String>>) -> Result<Self, Self::Error> {
        match HeaderValue::from_str(&hdr_value.0.join(", ")) {
            Ok(hdr_value) => Ok(hdr_value),
            Err(e) => Err(format!(
                "Unable to convert {:?} into a header - {}",
                hdr_value, e
            )),
        }
    }
}

// String

impl TryFrom<HeaderValue> for IntoHeaderValue<String> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> Result<Self, Self::Error> {
        match hdr_value.to_str() {
            Ok(hdr_value) => Ok(IntoHeaderValue(hdr_value.to_string())),
            Err(e) => Err(format!("Unable to convert header {:?} to {}", hdr_value, e)),
        }
    }
}

impl TryFrom<IntoHeaderValue<String>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: IntoHeaderValue<String>) -> Result<Self, Self::Error> {
        match HeaderValue::from_str(&hdr_value.0) {
            Ok(hdr_value) => Ok(hdr_value),
            Err(e) => Err(format!(
                "Unable to convert {:?} from a header {}",
                hdr_value, e
            )),
        }
    }
}

// Bool

impl TryFrom<HeaderValue> for IntoHeaderValue<bool> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> Result<Self, Self::Error> {
        match hdr_value.to_str() {
            Ok(hdr_value) => match hdr_value.parse() {
                Ok(hdr_value) => Ok(IntoHeaderValue(hdr_value)),
                Err(e) => Err(format!("Unable to parse bool from {} - {}", hdr_value, e)),
            },
            Err(e) => Err(format!(
                "Unable to convert {:?} from a header {}",
                hdr_value, e
            )),
        }
    }
}

impl TryFrom<IntoHeaderValue<bool>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: IntoHeaderValue<bool>) -> Result<Self, Self::Error> {
        match HeaderValue::from_str(&hdr_value.0.to_string()) {
            Ok(hdr_value) => Ok(hdr_value),
            Err(e) => Err(format!(
                "Unable to convert: {:?} into a header: {}",
                hdr_value, e
            )),
        }
    }
}

// DateTime

impl TryFrom<HeaderValue> for IntoHeaderValue<DateTime<Utc>> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> Result<Self, Self::Error> {
        match hdr_value.to_str() {
            Ok(hdr_value) => match DateTime::parse_from_rfc3339(hdr_value) {
                Ok(date) => Ok(IntoHeaderValue(date.with_timezone(&Utc))),
                Err(e) => Err(format!("Unable to parse: {} as date - {}", hdr_value, e)),
            },
            Err(e) => Err(format!(
                "Unable to convert header {:?} to string {}",
                hdr_value, e
            )),
        }
    }
}

impl TryFrom<IntoHeaderValue<DateTime<Utc>>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: IntoHeaderValue<DateTime<Utc>>) -> Result<Self, Self::Error> {
        match HeaderValue::from_str(hdr_value.0.to_rfc3339().as_str()) {
            Ok(hdr_value) => Ok(hdr_value),
            Err(e) => Err(format!(
                "Unable to convert {:?} to a header: {}",
                hdr_value, e
            )),
        }
    }
}
