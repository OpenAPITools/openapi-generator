#![allow(unused_imports, unused_qualifications, unused_extern_crates)]
extern crate chrono;
extern crate uuid;

use serde_xml_rs;
use serde::ser::Serializer;

use std::collections::{HashMap, BTreeMap};
use models;
use swagger;


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename = "snake_another_xml_inner")]
pub struct AnotherXmlInner(String);

impl ::std::convert::From<String> for AnotherXmlInner {
    fn from(x: String) -> Self {
        AnotherXmlInner(x)
    }
}

impl ::std::convert::From<AnotherXmlInner> for String {
    fn from(x: AnotherXmlInner) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for AnotherXmlInner {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl ::std::ops::DerefMut for AnotherXmlInner {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}


impl AnotherXmlInner {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// An XML object
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename = "snake_another_xml_object")]
pub struct AnotherXmlObject {
    #[serde(rename = "inner_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub inner_string: Option<String>,

}

impl AnotherXmlObject {
    pub fn new() -> AnotherXmlObject {
        AnotherXmlObject {
            inner_string: None,
        }
    }
}

impl AnotherXmlObject {
    /// Associated constant for this model's XML namespace.
    #[allow(dead_code)]
    pub const NAMESPACE: &'static str = "http://foo.bar";
}

impl AnotherXmlObject {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        let mut namespaces = BTreeMap::new();
        // An empty string is used to indicate a global namespace in xmltree.
        namespaces.insert("".to_string(), Self::NAMESPACE.to_string());
        serde_xml_rs::to_string_with_namespaces(&self, namespaces).expect("impossible to fail to serialize")
    }
}

/// An XML object
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename = "camelDuplicateXmlObject")]
pub struct DuplicateXmlObject {
    #[serde(rename = "inner_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub inner_string: Option<String>,

    #[serde(rename = "inner_array")]
    pub inner_array: Vec<models::XmlInner>,

}

impl DuplicateXmlObject {
    pub fn new(inner_array: Vec<models::XmlInner>, ) -> DuplicateXmlObject {
        DuplicateXmlObject {
            inner_string: None,
            inner_array: inner_array,
        }
    }
}

impl DuplicateXmlObject {
    /// Associated constant for this model's XML namespace.
    #[allow(dead_code)]
    pub const NAMESPACE: &'static str = "http://different.bar";
}

impl DuplicateXmlObject {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        let mut namespaces = BTreeMap::new();
        // An empty string is used to indicate a global namespace in xmltree.
        namespaces.insert("".to_string(), Self::NAMESPACE.to_string());
        serde_xml_rs::to_string_with_namespaces(&self, namespaces).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename = "camelXmlInner")]
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
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// An XML object
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename = "camelXmlObject")]
pub struct XmlObject {
    #[serde(rename = "innerString")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub inner_string: Option<String>,

    #[serde(rename = "other_inner_rename")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub other_inner_rename: Option<isize>,

}

impl XmlObject {
    pub fn new() -> XmlObject {
        XmlObject {
            inner_string: None,
            other_inner_rename: None,
        }
    }
}

impl XmlObject {
    /// Associated constant for this model's XML namespace.
    #[allow(dead_code)]
    pub const NAMESPACE: &'static str = "http://foo.bar";
}

impl XmlObject {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        let mut namespaces = BTreeMap::new();
        // An empty string is used to indicate a global namespace in xmltree.
        namespaces.insert("".to_string(), Self::NAMESPACE.to_string());
        serde_xml_rs::to_string_with_namespaces(&self, namespaces).expect("impossible to fail to serialize")
    }
}
