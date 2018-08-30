#![allow(unused_imports, unused_qualifications, unused_extern_crates)]
extern crate chrono;
extern crate uuid;


use serde::ser::Serializer;

use std::collections::HashMap;
use models;
use swagger;


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ANullableContainer {
    #[serde(rename = "NullableThing")]
    #[serde(deserialize_with = "swagger::nullable_format::deserialize_optional_nullable")]
    #[serde(default = "swagger::nullable_format::default_optional_nullable")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub nullable_thing: Option<swagger::Nullable<String>>,

}

impl ANullableContainer {
    pub fn new() -> ANullableContainer {
        ANullableContainer {
            nullable_thing: None,
        }
    }
}

/// An additionalPropertiesObject
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AdditionalPropertiesObject {
}

impl AdditionalPropertiesObject {
    pub fn new() -> AdditionalPropertiesObject {
        AdditionalPropertiesObject {
        }
    }
}
