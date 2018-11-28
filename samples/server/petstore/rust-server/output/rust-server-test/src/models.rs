#![allow(unused_imports, unused_qualifications, unused_extern_crates)]
extern crate chrono;
extern crate uuid;

use serde_xml_rs;
use serde::ser::Serializer;

use std::collections::{HashMap, BTreeMap};
use models;
use swagger;


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ANullableContainer {
    #[serde(rename = "NullableThing")]
    #[serde(deserialize_with = "swagger::nullable_format::deserialize_optional_nullable")]
    #[serde(default = "swagger::nullable_format::default_optional_nullable")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub nullable_thing: Option<swagger::Nullable<String>>,

    #[serde(rename = "RequiredNullableThing")]
    pub required_nullable_thing: swagger::Nullable<String>,

}

impl ANullableContainer {
    pub fn new(required_nullable_thing: swagger::Nullable<String>, ) -> ANullableContainer {
        ANullableContainer {
            nullable_thing: None,
            required_nullable_thing: required_nullable_thing,
        }
    }
}


impl ANullableContainer {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct InlineObject {
    #[serde(rename = "id")]
    pub id: String,

    #[serde(rename = "password")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub password: Option<String>,

}

impl InlineObject {
    pub fn new(id: String, ) -> InlineObject {
        InlineObject {
            id: id,
            password: None,
        }
    }
}


impl InlineObject {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


/// An object of objects
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectOfObjects {
    #[serde(rename = "inner")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub inner: Option<models::ObjectOfObjectsInner>,

}

impl ObjectOfObjects {
    pub fn new() -> ObjectOfObjects {
        ObjectOfObjects {
            inner: None,
        }
    }
}


impl ObjectOfObjects {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ObjectOfObjectsInner {
    #[serde(rename = "optional_thing")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub optional_thing: Option<isize>,

    #[serde(rename = "required_thing")]
    pub required_thing: String,

}

impl ObjectOfObjectsInner {
    pub fn new(required_thing: String, ) -> ObjectOfObjectsInner {
        ObjectOfObjectsInner {
            optional_thing: None,
            required_thing: required_thing,
        }
    }
}


impl ObjectOfObjectsInner {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


/// An XML object
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename = "an_xml_object")]
pub struct XmlObject {
    #[serde(rename = "inner")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub inner: Option<String>,

}

impl XmlObject {
    pub fn new() -> XmlObject {
        XmlObject {
            inner: None,
        }
    }
}


impl XmlObject {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        let mut namespaces = BTreeMap::new();
        // An empty string is used to indicate a global namespace in xmltree.
        namespaces.insert("".to_string(), models::namespaces::XMLOBJECT.clone());
        serde_xml_rs::to_string_with_namespaces(&self, namespaces).expect("impossible to fail to serialize")
    }
}


//XML namespaces
pub mod namespaces {
    lazy_static!{
        pub static ref XMLOBJECT: String = "foo.bar".to_string();
    }
}
