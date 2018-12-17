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
    #[allow(dead_code, non_snake_case)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
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

impl AdditionalPropertiesObject {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code, non_snake_case)]
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
    #[allow(dead_code, non_snake_case)]
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
    #[allow(dead_code, non_snake_case)]
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
    #[allow(dead_code, non_snake_case)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// An XML list with items directly defined
// Utility function for wrapping list elements when serializing xml
fn wrap_in_xml_list_inner<S>(item: &Vec<String>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serde_xml_rs::wrap_primitives(item, serializer, "xml_list_inner")
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct XmlList(#[serde(serialize_with = "wrap_in_xml_list_inner")]Vec<String>);

impl ::std::convert::From<Vec<String>> for XmlList {
    fn from(x: Vec<String>) -> Self {
        XmlList(x)
    }
}

impl ::std::convert::From<XmlList> for Vec<String> {
    fn from(x: XmlList) -> Self {
        x.0
    }
}

impl ::std::iter::FromIterator<String> for XmlList {
    fn from_iter<U: IntoIterator<Item=String>>(u: U) -> Self {
        XmlList(Vec::<String>::from_iter(u))
    }
}

impl ::std::iter::IntoIterator for XmlList {
    type Item = String;
    type IntoIter = ::std::vec::IntoIter<String>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> ::std::iter::IntoIterator for &'a XmlList {
    type Item = &'a String;
    type IntoIter = ::std::slice::Iter<'a, String>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<'a> ::std::iter::IntoIterator for &'a mut XmlList {
    type Item = &'a mut String;
    type IntoIter = ::std::slice::IterMut<'a, String>;

    fn into_iter(self) -> Self::IntoIter {
        (&mut self.0).into_iter()
    }
}

impl ::std::ops::Deref for XmlList {
    type Target = Vec<String>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ::std::ops::DerefMut for XmlList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}


impl XmlList {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code, non_snake_case)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// An XML list with items referenced. The wrapping doesn't currently work - it's stripped during the conversion to openapi v3. See https://github.com/OpenAPITools/openapi-generator/issues/1581.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct XmlListRef(Vec<String>);

impl ::std::convert::From<Vec<String>> for XmlListRef {
    fn from(x: Vec<String>) -> Self {
        XmlListRef(x)
    }
}

impl ::std::convert::From<XmlListRef> for Vec<String> {
    fn from(x: XmlListRef) -> Self {
        x.0
    }
}

impl ::std::iter::FromIterator<String> for XmlListRef {
    fn from_iter<U: IntoIterator<Item=String>>(u: U) -> Self {
        XmlListRef(Vec::<String>::from_iter(u))
    }
}

impl ::std::iter::IntoIterator for XmlListRef {
    type Item = String;
    type IntoIter = ::std::vec::IntoIter<String>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> ::std::iter::IntoIterator for &'a XmlListRef {
    type Item = &'a String;
    type IntoIter = ::std::slice::Iter<'a, String>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<'a> ::std::iter::IntoIterator for &'a mut XmlListRef {
    type Item = &'a mut String;
    type IntoIter = ::std::slice::IterMut<'a, String>;

    fn into_iter(self) -> Self::IntoIter {
        (&mut self.0).into_iter()
    }
}

impl ::std::ops::Deref for XmlListRef {
    type Target = Vec<String>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ::std::ops::DerefMut for XmlListRef {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}


impl XmlListRef {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code, non_snake_case)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename = "xml_list_ref_inner")]
pub struct XmlListRefInner(String);

impl ::std::convert::From<String> for XmlListRefInner {
    fn from(x: String) -> Self {
        XmlListRefInner(x)
    }
}

impl ::std::convert::From<XmlListRefInner> for String {
    fn from(x: XmlListRefInner) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for XmlListRefInner {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl ::std::ops::DerefMut for XmlListRefInner {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}


impl XmlListRefInner {
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
