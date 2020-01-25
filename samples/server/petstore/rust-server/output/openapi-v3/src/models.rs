#![allow(unused_imports, unused_qualifications)]

use serde_xml_rs;
use serde::ser::Serializer;

use std::collections::{HashMap, BTreeMap};
use models;
use swagger;
use std::string::ParseError;
use uuid;


// Utility function for wrapping list elements when serializing xml
#[allow(non_snake_case)]
fn wrap_in_snake_another_xml_inner<S>(item: &Vec<String>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serde_xml_rs::wrap_primitives(item, serializer, "snake_another_xml_inner")
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct AnotherXmlArray(#[serde(serialize_with = "wrap_in_snake_another_xml_inner")]Vec<String>);

impl ::std::convert::From<Vec<String>> for AnotherXmlArray {
    fn from(x: Vec<String>) -> Self {
        AnotherXmlArray(x)
    }
}

impl ::std::convert::From<AnotherXmlArray> for Vec<String> {
    fn from(x: AnotherXmlArray) -> Self {
        x.0
    }
}

impl ::std::iter::FromIterator<String> for AnotherXmlArray {
    fn from_iter<U: IntoIterator<Item=String>>(u: U) -> Self {
        AnotherXmlArray(Vec::<String>::from_iter(u))
    }
}

impl ::std::iter::IntoIterator for AnotherXmlArray {
    type Item = String;
    type IntoIter = ::std::vec::IntoIter<String>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> ::std::iter::IntoIterator for &'a AnotherXmlArray {
    type Item = &'a String;
    type IntoIter = ::std::slice::Iter<'a, String>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<'a> ::std::iter::IntoIterator for &'a mut AnotherXmlArray {
    type Item = &'a mut String;
    type IntoIter = ::std::slice::IterMut<'a, String>;

    fn into_iter(self) -> Self::IntoIter {
        (&mut self.0).into_iter()
    }
}

impl ::std::ops::Deref for AnotherXmlArray {
    type Target = Vec<String>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ::std::ops::DerefMut for AnotherXmlArray {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}


impl AnotherXmlArray {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
#[serde(rename = "snake_another_xml_inner")]
pub struct AnotherXmlInner(String);

impl ::std::convert::From<String> for AnotherXmlInner {
    fn from(x: String) -> Self {
        AnotherXmlInner(x)
    }
}

impl std::str::FromStr for AnotherXmlInner {
    type Err = ParseError;
    fn from_str(x: &str) -> Result<Self, Self::Err> {
        Ok(AnotherXmlInner(x.to_string()))
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
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
#[serde(rename = "camelDuplicateXmlObject")]
pub struct DuplicateXmlObject {
    #[serde(rename = "inner_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub inner_string: Option<String>,

    #[serde(rename = "inner_array")]
    #[serde(serialize_with = "wrap_in_camelXmlInner")]
    pub inner_array: models::XmlArray,

}

impl DuplicateXmlObject {
    pub fn new(inner_array: models::XmlArray, ) -> DuplicateXmlObject {
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

/// Test a model containing a special character in the enum
/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGenericEnum))]
pub enum EnumWithStarObject { 
    #[serde(rename = "FOO")]
    FOO,
    #[serde(rename = "BAR")]
    BAR,
    #[serde(rename = "*")]
    STAR,
}

impl ::std::fmt::Display for EnumWithStarObject {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self { 
            EnumWithStarObject::FOO => write!(f, "{}", "FOO"),
            EnumWithStarObject::BAR => write!(f, "{}", "BAR"),
            EnumWithStarObject::STAR => write!(f, "{}", "*"),
        }
    }
}

impl ::std::str::FromStr for EnumWithStarObject {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "FOO" => Ok(EnumWithStarObject::FOO),
            "BAR" => Ok(EnumWithStarObject::BAR),
            "*" => Ok(EnumWithStarObject::STAR),
            _ => Err(()),
        }
    }
}

impl EnumWithStarObject {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct InlineResponse201 {
    #[serde(rename = "foo")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub foo: Option<String>,

}

impl InlineResponse201 {
    pub fn new() -> InlineResponse201 {
        InlineResponse201 {
            foo: None,
        }
    }
}

impl InlineResponse201 {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct MyId(i32);

impl ::std::convert::From<i32> for MyId {
    fn from(x: i32) -> Self {
        MyId(x)
    }
}


impl ::std::convert::From<MyId> for i32 {
    fn from(x: MyId) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for MyId {
    type Target = i32;
    fn deref(&self) -> &i32 {
        &self.0
    }
}

impl ::std::ops::DerefMut for MyId {
    fn deref_mut(&mut self) -> &mut i32 {
        &mut self.0
    }
}


impl MyId {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct MyIdList(Vec<i32>);

impl ::std::convert::From<Vec<i32>> for MyIdList {
    fn from(x: Vec<i32>) -> Self {
        MyIdList(x)
    }
}

impl ::std::convert::From<MyIdList> for Vec<i32> {
    fn from(x: MyIdList) -> Self {
        x.0
    }
}

impl ::std::iter::FromIterator<i32> for MyIdList {
    fn from_iter<U: IntoIterator<Item=i32>>(u: U) -> Self {
        MyIdList(Vec::<i32>::from_iter(u))
    }
}

impl ::std::iter::IntoIterator for MyIdList {
    type Item = i32;
    type IntoIter = ::std::vec::IntoIter<i32>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> ::std::iter::IntoIterator for &'a MyIdList {
    type Item = &'a i32;
    type IntoIter = ::std::slice::Iter<'a, i32>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<'a> ::std::iter::IntoIterator for &'a mut MyIdList {
    type Item = &'a mut i32;
    type IntoIter = ::std::slice::IterMut<'a, i32>;

    fn into_iter(self) -> Self::IntoIter {
        (&mut self.0).into_iter()
    }
}

impl ::std::ops::Deref for MyIdList {
    type Target = Vec<i32>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ::std::ops::DerefMut for MyIdList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}


impl MyIdList {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Test a model containing a UUID
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct UuidObject(uuid::Uuid);

impl ::std::convert::From<uuid::Uuid> for UuidObject {
    fn from(x: uuid::Uuid) -> Self {
        UuidObject(x)
    }
}


impl ::std::convert::From<UuidObject> for uuid::Uuid {
    fn from(x: UuidObject) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for UuidObject {
    type Target = uuid::Uuid;
    fn deref(&self) -> &uuid::Uuid {
        &self.0
    }
}

impl ::std::ops::DerefMut for UuidObject {
    fn deref_mut(&mut self) -> &mut uuid::Uuid {
        &mut self.0
    }
}


impl UuidObject {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

// Utility function for wrapping list elements when serializing xml
#[allow(non_snake_case)]
fn wrap_in_camelXmlInner<S>(item: &Vec<String>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serde_xml_rs::wrap_primitives(item, serializer, "camelXmlInner")
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct XmlArray(#[serde(serialize_with = "wrap_in_camelXmlInner")]Vec<String>);

impl ::std::convert::From<Vec<String>> for XmlArray {
    fn from(x: Vec<String>) -> Self {
        XmlArray(x)
    }
}

impl ::std::convert::From<XmlArray> for Vec<String> {
    fn from(x: XmlArray) -> Self {
        x.0
    }
}

impl ::std::iter::FromIterator<String> for XmlArray {
    fn from_iter<U: IntoIterator<Item=String>>(u: U) -> Self {
        XmlArray(Vec::<String>::from_iter(u))
    }
}

impl ::std::iter::IntoIterator for XmlArray {
    type Item = String;
    type IntoIter = ::std::vec::IntoIter<String>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> ::std::iter::IntoIterator for &'a XmlArray {
    type Item = &'a String;
    type IntoIter = ::std::slice::Iter<'a, String>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<'a> ::std::iter::IntoIterator for &'a mut XmlArray {
    type Item = &'a mut String;
    type IntoIter = ::std::slice::IterMut<'a, String>;

    fn into_iter(self) -> Self::IntoIter {
        (&mut self.0).into_iter()
    }
}

impl ::std::ops::Deref for XmlArray {
    type Target = Vec<String>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ::std::ops::DerefMut for XmlArray {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}


impl XmlArray {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
#[serde(rename = "camelXmlInner")]
pub struct XmlInner(String);

impl ::std::convert::From<String> for XmlInner {
    fn from(x: String) -> Self {
        XmlInner(x)
    }
}

impl std::str::FromStr for XmlInner {
    type Err = ParseError;
    fn from_str(x: &str) -> Result<Self, Self::Err> {
        Ok(XmlInner(x.to_string()))
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
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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
