#![allow(unused_imports, unused_qualifications)]

use serde::ser::Serializer;

use std::collections::HashMap;
use models;
use swagger;
use hyper::header::HeaderValue;
use std::string::ParseError;
use std::str::FromStr;
use header::IntoHeaderValue;



// Methods for converting between IntoHeaderValue<MultipartRelatedRequest> and HeaderValue

impl From<IntoHeaderValue<MultipartRelatedRequest>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<MultipartRelatedRequest>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<MultipartRelatedRequest> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(MultipartRelatedRequest::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct MultipartRelatedRequest {
    #[serde(rename = "object_field")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub object_field: Option<models::MultipartRequestObjectField>,

    #[serde(rename = "optional_binary_field")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub optional_binary_field: Option<swagger::ByteArray>,

    #[serde(rename = "required_binary_field")]
    pub required_binary_field: swagger::ByteArray,

}

impl MultipartRelatedRequest {
    pub fn new(required_binary_field: swagger::ByteArray, ) -> MultipartRelatedRequest {
        MultipartRelatedRequest {
            object_field: None,
            optional_binary_field: None,
            required_binary_field: required_binary_field,
        }
    }
}



// Methods for converting between IntoHeaderValue<MultipartRequest> and HeaderValue

impl From<IntoHeaderValue<MultipartRequest>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<MultipartRequest>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<MultipartRequest> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(MultipartRequest::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct MultipartRequest {
    #[serde(rename = "string_field")]
    pub string_field: String,

    #[serde(rename = "optional_string_field")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub optional_string_field: Option<String>,

    #[serde(rename = "object_field")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub object_field: Option<models::MultipartRequestObjectField>,

    #[serde(rename = "binary_field")]
    pub binary_field: swagger::ByteArray,

}

impl MultipartRequest {
    pub fn new(string_field: String, binary_field: swagger::ByteArray, ) -> MultipartRequest {
        MultipartRequest {
            string_field: string_field,
            optional_string_field: None,
            object_field: None,
            binary_field: binary_field,
        }
    }
}



// Methods for converting between IntoHeaderValue<MultipartRequestObjectField> and HeaderValue

impl From<IntoHeaderValue<MultipartRequestObjectField>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<MultipartRequestObjectField>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<MultipartRequestObjectField> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(MultipartRequestObjectField::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct MultipartRequestObjectField {
    #[serde(rename = "field_a")]
    pub field_a: String,

    #[serde(rename = "field_b")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub field_b: Option<Vec<String>>,

}

impl MultipartRequestObjectField {
    pub fn new(field_a: String, ) -> MultipartRequestObjectField {
        MultipartRequestObjectField {
            field_a: field_a,
            field_b: None,
        }
    }
}

