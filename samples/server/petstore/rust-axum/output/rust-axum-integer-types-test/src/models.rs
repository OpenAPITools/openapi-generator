#![allow(unused_qualifications)]

use http::HeaderValue;
use validator::Validate;

#[cfg(feature = "server")]
use crate::header;
use crate::{models, types::*};

#[allow(dead_code)]
fn from_validation_error(e: validator::ValidationError) -> validator::ValidationErrors {
    let mut errs = validator::ValidationErrors::new();
    errs.add("na", e);
    errs
}

#[allow(dead_code)]
pub fn check_xss_string(v: &str) -> std::result::Result<(), validator::ValidationError> {
    if ammonia::is_html(v) {
        std::result::Result::Err(validator::ValidationError::new("xss detected"))
    } else {
        std::result::Result::Ok(())
    }
}

#[allow(dead_code)]
pub fn check_xss_vec_string(v: &[String]) -> std::result::Result<(), validator::ValidationError> {
    if v.iter().any(|i| ammonia::is_html(i)) {
        std::result::Result::Err(validator::ValidationError::new("xss detected"))
    } else {
        std::result::Result::Ok(())
    }
}

#[allow(dead_code)]
pub fn check_xss_map_string(
    v: &std::collections::HashMap<String, String>,
) -> std::result::Result<(), validator::ValidationError> {
    if v.keys().any(|k| ammonia::is_html(k)) || v.values().any(|v| ammonia::is_html(v)) {
        std::result::Result::Err(validator::ValidationError::new("xss detected"))
    } else {
        std::result::Result::Ok(())
    }
}

#[allow(dead_code)]
pub fn check_xss_map_nested<T>(
    v: &std::collections::HashMap<String, T>,
) -> std::result::Result<(), validator::ValidationError>
where
    T: validator::Validate,
{
    if v.keys().any(|k| ammonia::is_html(k)) || v.values().any(|v| v.validate().is_err()) {
        std::result::Result::Err(validator::ValidationError::new("xss detected"))
    } else {
        std::result::Result::Ok(())
    }
}

#[allow(dead_code)]
pub fn check_xss_map<T>(
    v: &std::collections::HashMap<String, T>,
) -> std::result::Result<(), validator::ValidationError> {
    if v.keys().any(|k| ammonia::is_html(k)) {
        std::result::Result::Err(validator::ValidationError::new("xss detected"))
    } else {
        std::result::Result::Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct GetIntegersQueryParams {
    #[serde(rename = "legacy_uint32")]
    pub legacy_uint32: u32,
    #[serde(rename = "legacy_uint64")]
    pub legacy_uint64: u64,
    #[serde(rename = "positive_int32")]
    #[validate(range(min = 0u32))]
    pub positive_int32: u32,
    #[serde(rename = "positive_int64")]
    #[validate(range(min = 0u64))]
    pub positive_int64: u64,
    #[serde(rename = "small_positive")]
    #[validate(range(min = 0u8, max = 255u8))]
    pub small_positive: u8,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct IntegerTypes {
    #[serde(rename = "legacy_uint32")]
    pub legacy_uint32: u32,

    #[serde(rename = "legacy_uint64")]
    pub legacy_uint64: u64,

    #[serde(rename = "positive_int32")]
    #[validate(range(min = 0u32))]
    pub positive_int32: u32,

    #[serde(rename = "positive_int64")]
    #[validate(range(min = 0u64))]
    pub positive_int64: u64,

    #[serde(rename = "small_positive")]
    #[validate(range(min = 0u8, max = 255u8))]
    pub small_positive: u8,
}

impl IntegerTypes {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(
        legacy_uint32: u32,
        legacy_uint64: u64,
        positive_int32: u32,
        positive_int64: u64,
        small_positive: u8,
    ) -> IntegerTypes {
        IntegerTypes {
            legacy_uint32,
            legacy_uint64,
            positive_int32,
            positive_int64,
            small_positive,
        }
    }
}

/// Converts the IntegerTypes value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for IntegerTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            Some("legacy_uint32".to_string()),
            Some(self.legacy_uint32.to_string()),
            Some("legacy_uint64".to_string()),
            Some(self.legacy_uint64.to_string()),
            Some("positive_int32".to_string()),
            Some(self.positive_int32.to_string()),
            Some("positive_int64".to_string()),
            Some(self.positive_int64.to_string()),
            Some("small_positive".to_string()),
            Some(self.small_positive.to_string()),
        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a IntegerTypes value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for IntegerTypes {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub legacy_uint32: Vec<u32>,
            pub legacy_uint64: Vec<u64>,
            pub positive_int32: Vec<u32>,
            pub positive_int64: Vec<u64>,
            pub small_positive: Vec<u8>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing IntegerTypes".to_string(),
                    );
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "legacy_uint32" => intermediate_rep.legacy_uint32.push(
                        <u32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "legacy_uint64" => intermediate_rep.legacy_uint64.push(
                        <u64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "positive_int32" => intermediate_rep.positive_int32.push(
                        <u32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "positive_int64" => intermediate_rep.positive_int64.push(
                        <u64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "small_positive" => intermediate_rep
                        .small_positive
                        .push(<u8 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing IntegerTypes".to_string(),
                        );
                    }
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(IntegerTypes {
            legacy_uint32: intermediate_rep
                .legacy_uint32
                .into_iter()
                .next()
                .ok_or_else(|| "legacy_uint32 missing in IntegerTypes".to_string())?,
            legacy_uint64: intermediate_rep
                .legacy_uint64
                .into_iter()
                .next()
                .ok_or_else(|| "legacy_uint64 missing in IntegerTypes".to_string())?,
            positive_int32: intermediate_rep
                .positive_int32
                .into_iter()
                .next()
                .ok_or_else(|| "positive_int32 missing in IntegerTypes".to_string())?,
            positive_int64: intermediate_rep
                .positive_int64
                .into_iter()
                .next()
                .ok_or_else(|| "positive_int64 missing in IntegerTypes".to_string())?,
            small_positive: intermediate_rep
                .small_positive
                .into_iter()
                .next()
                .ok_or_else(|| "small_positive missing in IntegerTypes".to_string())?,
        })
    }
}

// Methods for converting between header::IntoHeaderValue<IntegerTypes> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<IntegerTypes>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<IntegerTypes>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Invalid header value for IntegerTypes - value: {hdr_value} is invalid {e}"#
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<IntegerTypes> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <IntegerTypes as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        r#"Unable to convert header value '{value}' into IntegerTypes - {err}"#
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                r#"Unable to convert header: {hdr_value:?} to string: {e}"#
            )),
        }
    }
}
