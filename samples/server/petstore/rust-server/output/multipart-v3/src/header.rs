use chrono::{DateTime, Utc};
use hyper::header::HeaderValue;
use std::fmt;
use std::ops::Deref;

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

// Derive for each From<T> in hyper::header::HeaderValue

macro_rules! ihv_generate {
    ($t:ident) => {
        impl From<HeaderValue> for IntoHeaderValue<$t> {
            fn from(hdr_value: HeaderValue) -> Self {
                IntoHeaderValue(hdr_value.to_str().unwrap().parse::<$t>().unwrap())
            }
        }

        impl From<IntoHeaderValue<$t>> for HeaderValue {
            fn from(hdr_value: IntoHeaderValue<$t>) -> Self {
                hdr_value.0.into()
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

impl From<HeaderValue> for IntoHeaderValue<Vec<String>> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(
            hdr_value
                .to_str()
                .unwrap()
                .split(',')
                .filter_map(|x| match x.trim() {
                    "" => None,
                    y => Some(y.to_string()),
                })
                .collect(),
        )
    }
}

impl From<IntoHeaderValue<Vec<String>>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<Vec<String>>) -> Self {
        HeaderValue::from_str(&hdr_value.0.join(", ")).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<String> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(hdr_value.to_str().unwrap().to_string())
    }
}

impl From<IntoHeaderValue<String>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<String>) -> Self {
        HeaderValue::from_str(&hdr_value.0).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<DateTime<Utc>> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(
            DateTime::parse_from_rfc3339(hdr_value.to_str().unwrap())
                .unwrap()
                .with_timezone(&Utc),
        )
    }
}

impl From<IntoHeaderValue<DateTime<Utc>>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<DateTime<Utc>>) -> Self {
        HeaderValue::from_str(hdr_value.0.to_rfc3339().as_str()).unwrap()
    }
}
