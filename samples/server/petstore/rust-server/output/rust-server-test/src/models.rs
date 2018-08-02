#![allow(unused_imports, unused_qualifications, unused_extern_crates)]
extern crate chrono;
extern crate uuid;


use serde::ser::Serializer;

use std::collections::HashMap;
use models;
use swagger;


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
