#![allow(unused_qualifications)]

use http::HeaderValue;
use validator::Validate;

#[cfg(feature = "server")]
use crate::header;
use crate::{models, types::*};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct EndpointGetQueryParams {
    /// Some numbers.
    #[serde(rename = "numbers")]
    #[serde(default)]
    pub numbers: Vec<f64>,
    /// Multipler for sum.
    #[serde(rename = "multiplier")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub multiplier: Option<f64>,
}
