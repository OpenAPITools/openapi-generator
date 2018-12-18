#![allow(unused_imports, unused_qualifications, unused_extern_crates)]
extern crate chrono;
extern crate uuid;

use serde_xml_rs;
use serde::ser::Serializer;

use std::collections::{HashMap, BTreeMap};
use models;
use swagger;


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename = "another")]
pub struct XmlInner(String);

impl ::std::convert::From<String> for XmlInner {
    fn from(x: String) -> Self {
        XmlInner(x)
    }
}

impl ::std::convert::From<XmlInner> for String {
    fn from(x: XmlInner) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for XmlInner {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl ::std::ops::DerefMut for XmlInner {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}


impl XmlInner {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code, non_snake_case)]
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
    #[allow(dead_code, non_snake_case)]
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
