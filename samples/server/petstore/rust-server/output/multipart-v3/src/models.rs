#![allow(unused_imports, unused_qualifications, unused_extern_crates)]
extern crate chrono;

use serde::ser::Serializer;

use std::collections::HashMap;
use models;
use swagger;
use std::string::ParseError;


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

