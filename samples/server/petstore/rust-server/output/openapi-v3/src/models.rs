#![allow(unused_imports, unused_qualifications)]

use serde_xml_rs;
use serde::ser::Serializer;

use std::collections::{HashMap, BTreeMap};
use models;
use swagger;
use hyper::header::HeaderValue;
use std::string::ParseError;
use uuid;
use std::str::FromStr;
use header::IntoHeaderValue;


// Methods for converting between IntoHeaderValue<AnotherXmlArray> and HeaderValue

impl From<IntoHeaderValue<AnotherXmlArray>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<AnotherXmlArray>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<AnotherXmlArray> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(AnotherXmlArray::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}

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

/// Converts the AnotherXmlArray value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for AnotherXmlArray {
    fn to_string(&self) -> String {
        self.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnotherXmlArray value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for AnotherXmlArray {
    type Err = <String as ::std::str::FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut items = vec![];
        for item in s.split(',')
        {
            items.push(item.parse()?);
        }
        Ok(AnotherXmlArray(items))
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
// Methods for converting between IntoHeaderValue<AnotherXmlObject> and HeaderValue

impl From<IntoHeaderValue<AnotherXmlObject>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<AnotherXmlObject>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<AnotherXmlObject> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(AnotherXmlObject::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}


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

/// Converts the AnotherXmlObject value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for AnotherXmlObject {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref inner_string) = self.inner_string {
            params.push("inner_string".to_string());
            params.push(inner_string.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnotherXmlObject value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for AnotherXmlObject {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub inner_string: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return Err("Missing value while parsing AnotherXmlObject".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "inner_string" => intermediate_rep.inner_string.push(String::from_str(val).map_err(|x| format!("{}", x))?),
                    _ => return Err("Unexpected key while parsing AnotherXmlObject".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(AnotherXmlObject {
            inner_string: intermediate_rep.inner_string.into_iter().next(),
        })
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
// Methods for converting between IntoHeaderValue<DuplicateXmlObject> and HeaderValue

impl From<IntoHeaderValue<DuplicateXmlObject>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<DuplicateXmlObject>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<DuplicateXmlObject> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(DuplicateXmlObject::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}


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

/// Converts the DuplicateXmlObject value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for DuplicateXmlObject {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref inner_string) = self.inner_string {
            params.push("inner_string".to_string());
            params.push(inner_string.to_string());
        }

        // Skipping inner_array in query parameter serialization

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a DuplicateXmlObject value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for DuplicateXmlObject {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub inner_string: Vec<String>,
            pub inner_array: Vec<models::XmlArray>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return Err("Missing value while parsing DuplicateXmlObject".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "inner_string" => intermediate_rep.inner_string.push(String::from_str(val).map_err(|x| format!("{}", x))?),
                    "inner_array" => intermediate_rep.inner_array.push(models::XmlArray::from_str(val).map_err(|x| format!("{}", x))?),
                    _ => return Err("Unexpected key while parsing DuplicateXmlObject".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(DuplicateXmlObject {
            inner_string: intermediate_rep.inner_string.into_iter().next(),
            inner_array: intermediate_rep.inner_array.into_iter().next().ok_or("inner_array missing in DuplicateXmlObject".to_string())?,
        })
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
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "FOO" => Ok(EnumWithStarObject::FOO),
            "BAR" => Ok(EnumWithStarObject::BAR),
            "*" => Ok(EnumWithStarObject::STAR),
            _ => Err(format!("Value not valid: {}", s)),
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

// Methods for converting between IntoHeaderValue<InlineResponse201> and HeaderValue

impl From<IntoHeaderValue<InlineResponse201>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<InlineResponse201>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<InlineResponse201> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(InlineResponse201::from_str(hdr_value.to_str().unwrap()).unwrap())
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

/// Converts the InlineResponse201 value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for InlineResponse201 {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref foo) = self.foo {
            params.push("foo".to_string());
            params.push(foo.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a InlineResponse201 value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for InlineResponse201 {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub foo: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return Err("Missing value while parsing InlineResponse201".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "foo" => intermediate_rep.foo.push(String::from_str(val).map_err(|x| format!("{}", x))?),
                    _ => return Err("Unexpected key while parsing InlineResponse201".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(InlineResponse201 {
            foo: intermediate_rep.foo.into_iter().next(),
        })
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

// Methods for converting between IntoHeaderValue<MyIdList> and HeaderValue

impl From<IntoHeaderValue<MyIdList>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<MyIdList>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<MyIdList> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(MyIdList::from_str(hdr_value.to_str().unwrap()).unwrap())
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

/// Converts the MyIdList value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for MyIdList {
    fn to_string(&self) -> String {
        self.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MyIdList value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for MyIdList {
    type Err = <i32 as ::std::str::FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut items = vec![];
        for item in s.split(',')
        {
            items.push(item.parse()?);
        }
        Ok(MyIdList(items))
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

// Methods for converting between IntoHeaderValue<NullableTest> and HeaderValue

impl From<IntoHeaderValue<NullableTest>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<NullableTest>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<NullableTest> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(NullableTest::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct NullableTest {
    #[serde(rename = "nullable")]
    pub nullable: swagger::Nullable<String>,

    #[serde(rename = "nullableWithNullDefault")]
    #[serde(deserialize_with = "swagger::nullable_format::deserialize_optional_nullable")]
    #[serde(default = "swagger::nullable_format::default_optional_nullable")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub nullable_with_null_default: Option<swagger::Nullable<String>>,

    #[serde(rename = "nullableWithPresentDefault")]
    #[serde(deserialize_with = "swagger::nullable_format::deserialize_optional_nullable")]
    #[serde(default = "swagger::nullable_format::default_optional_nullable")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub nullable_with_present_default: Option<swagger::Nullable<String>>,

    #[serde(rename = "nullableWithNoDefault")]
    #[serde(deserialize_with = "swagger::nullable_format::deserialize_optional_nullable")]
    #[serde(default = "swagger::nullable_format::default_optional_nullable")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub nullable_with_no_default: Option<swagger::Nullable<String>>,

    #[serde(rename = "nullableArray")]
    #[serde(deserialize_with = "swagger::nullable_format::deserialize_optional_nullable")]
    #[serde(default = "swagger::nullable_format::default_optional_nullable")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub nullable_array: Option<swagger::Nullable<Vec<String>>>,

}

impl NullableTest {
    pub fn new(nullable: swagger::Nullable<String>, ) -> NullableTest {
        NullableTest {
            nullable: nullable,
            nullable_with_null_default: Some(swagger::Nullable::Null),
            nullable_with_present_default: Some(swagger::Nullable::Present("default".to_string())),
            nullable_with_no_default: None,
            nullable_array: None,
        }
    }
}

/// Converts the NullableTest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for NullableTest {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        params.push("nullable".to_string());
        params.push(self.nullable.as_ref().map_or("null".to_string(), |x| x.to_string()));


        if let Some(ref nullable_with_null_default) = self.nullable_with_null_default {
            params.push("nullableWithNullDefault".to_string());
            params.push(nullable_with_null_default.as_ref().map_or("null".to_string(), |x| x.to_string()));
        }


        if let Some(ref nullable_with_present_default) = self.nullable_with_present_default {
            params.push("nullableWithPresentDefault".to_string());
            params.push(nullable_with_present_default.as_ref().map_or("null".to_string(), |x| x.to_string()));
        }


        if let Some(ref nullable_with_no_default) = self.nullable_with_no_default {
            params.push("nullableWithNoDefault".to_string());
            params.push(nullable_with_no_default.as_ref().map_or("null".to_string(), |x| x.to_string()));
        }


        if let Some(ref nullable_array) = self.nullable_array {
            params.push("nullableArray".to_string());
            params.push(nullable_array.as_ref().map_or("null".to_string(), |x| x.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",").to_string()));
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a NullableTest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for NullableTest {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub nullable: Vec<String>,
            pub nullable_with_null_default: Vec<String>,
            pub nullable_with_present_default: Vec<String>,
            pub nullable_with_no_default: Vec<String>,
            pub nullable_array: Vec<Vec<String>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return Err("Missing value while parsing NullableTest".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "nullable" => return Err("Parsing a nullable type in this style is not supported in NullableTest".to_string()),
                    "nullableWithNullDefault" => return Err("Parsing a nullable type in this style is not supported in NullableTest".to_string()),
                    "nullableWithPresentDefault" => return Err("Parsing a nullable type in this style is not supported in NullableTest".to_string()),
                    "nullableWithNoDefault" => return Err("Parsing a nullable type in this style is not supported in NullableTest".to_string()),
                    "nullableArray" => return Err("Parsing a container in this style is not supported in NullableTest".to_string()),
                    _ => return Err("Unexpected key while parsing NullableTest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(NullableTest {
            nullable: Err("Nullable types not supported in NullableTest".to_string())?,
            nullable_with_null_default: Err("Nullable types not supported in NullableTest".to_string())?,
            nullable_with_present_default: Err("Nullable types not supported in NullableTest".to_string())?,
            nullable_with_no_default: Err("Nullable types not supported in NullableTest".to_string())?,
            nullable_array: Err("Nullable types not supported in NullableTest".to_string())?,
        })
    }
}


impl NullableTest {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

// Methods for converting between IntoHeaderValue<ObjectHeader> and HeaderValue

impl From<IntoHeaderValue<ObjectHeader>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<ObjectHeader>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<ObjectHeader> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(ObjectHeader::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct ObjectHeader {
    #[serde(rename = "requiredObjectHeader")]
    pub required_object_header: bool,

    #[serde(rename = "optionalObjectHeader")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub optional_object_header: Option<isize>,

}

impl ObjectHeader {
    pub fn new(required_object_header: bool, ) -> ObjectHeader {
        ObjectHeader {
            required_object_header: required_object_header,
            optional_object_header: None,
        }
    }
}

/// Converts the ObjectHeader value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for ObjectHeader {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        params.push("requiredObjectHeader".to_string());
        params.push(self.required_object_header.to_string());


        if let Some(ref optional_object_header) = self.optional_object_header {
            params.push("optionalObjectHeader".to_string());
            params.push(optional_object_header.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectHeader value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for ObjectHeader {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub required_object_header: Vec<bool>,
            pub optional_object_header: Vec<isize>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return Err("Missing value while parsing ObjectHeader".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "requiredObjectHeader" => intermediate_rep.required_object_header.push(bool::from_str(val).map_err(|x| format!("{}", x))?),
                    "optionalObjectHeader" => intermediate_rep.optional_object_header.push(isize::from_str(val).map_err(|x| format!("{}", x))?),
                    _ => return Err("Unexpected key while parsing ObjectHeader".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(ObjectHeader {
            required_object_header: intermediate_rep.required_object_header.into_iter().next().ok_or("requiredObjectHeader missing in ObjectHeader".to_string())?,
            optional_object_header: intermediate_rep.optional_object_header.into_iter().next(),
        })
    }
}


impl ObjectHeader {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

// Methods for converting between IntoHeaderValue<ObjectParam> and HeaderValue

impl From<IntoHeaderValue<ObjectParam>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<ObjectParam>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<ObjectParam> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(ObjectParam::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct ObjectParam {
    #[serde(rename = "requiredParam")]
    pub required_param: bool,

    #[serde(rename = "optionalParam")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub optional_param: Option<isize>,

}

impl ObjectParam {
    pub fn new(required_param: bool, ) -> ObjectParam {
        ObjectParam {
            required_param: required_param,
            optional_param: None,
        }
    }
}

/// Converts the ObjectParam value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for ObjectParam {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        params.push("requiredParam".to_string());
        params.push(self.required_param.to_string());


        if let Some(ref optional_param) = self.optional_param {
            params.push("optionalParam".to_string());
            params.push(optional_param.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectParam value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for ObjectParam {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub required_param: Vec<bool>,
            pub optional_param: Vec<isize>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return Err("Missing value while parsing ObjectParam".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "requiredParam" => intermediate_rep.required_param.push(bool::from_str(val).map_err(|x| format!("{}", x))?),
                    "optionalParam" => intermediate_rep.optional_param.push(isize::from_str(val).map_err(|x| format!("{}", x))?),
                    _ => return Err("Unexpected key while parsing ObjectParam".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(ObjectParam {
            required_param: intermediate_rep.required_param.into_iter().next().ok_or("requiredParam missing in ObjectParam".to_string())?,
            optional_param: intermediate_rep.optional_param.into_iter().next(),
        })
    }
}


impl ObjectParam {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

// Methods for converting between IntoHeaderValue<ObjectUntypedProps> and HeaderValue

impl From<IntoHeaderValue<ObjectUntypedProps>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<ObjectUntypedProps>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<ObjectUntypedProps> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(ObjectUntypedProps::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct ObjectUntypedProps {
    #[serde(rename = "required_untyped")]
    pub required_untyped: serde_json::Value,

    #[serde(rename = "required_untyped_nullable")]
    pub required_untyped_nullable: serde_json::Value,

    #[serde(rename = "not_required_untyped")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub not_required_untyped: Option<serde_json::Value>,

    #[serde(rename = "not_required_untyped_nullable")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub not_required_untyped_nullable: Option<serde_json::Value>,

}

impl ObjectUntypedProps {
    pub fn new(required_untyped: serde_json::Value, required_untyped_nullable: serde_json::Value, ) -> ObjectUntypedProps {
        ObjectUntypedProps {
            required_untyped: required_untyped,
            required_untyped_nullable: required_untyped_nullable,
            not_required_untyped: None,
            not_required_untyped_nullable: None,
        }
    }
}

/// Converts the ObjectUntypedProps value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for ObjectUntypedProps {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];
        // Skipping required_untyped in query parameter serialization

        // Skipping required_untyped_nullable in query parameter serialization

        // Skipping not_required_untyped in query parameter serialization

        // Skipping not_required_untyped_nullable in query parameter serialization

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectUntypedProps value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for ObjectUntypedProps {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub required_untyped: Vec<serde_json::Value>,
            pub required_untyped_nullable: Vec<serde_json::Value>,
            pub not_required_untyped: Vec<serde_json::Value>,
            pub not_required_untyped_nullable: Vec<serde_json::Value>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return Err("Missing value while parsing ObjectUntypedProps".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "required_untyped" => intermediate_rep.required_untyped.push(serde_json::Value::from_str(val).map_err(|x| format!("{}", x))?),
                    "required_untyped_nullable" => intermediate_rep.required_untyped_nullable.push(serde_json::Value::from_str(val).map_err(|x| format!("{}", x))?),
                    "not_required_untyped" => intermediate_rep.not_required_untyped.push(serde_json::Value::from_str(val).map_err(|x| format!("{}", x))?),
                    "not_required_untyped_nullable" => intermediate_rep.not_required_untyped_nullable.push(serde_json::Value::from_str(val).map_err(|x| format!("{}", x))?),
                    _ => return Err("Unexpected key while parsing ObjectUntypedProps".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(ObjectUntypedProps {
            required_untyped: intermediate_rep.required_untyped.into_iter().next().ok_or("required_untyped missing in ObjectUntypedProps".to_string())?,
            required_untyped_nullable: intermediate_rep.required_untyped_nullable.into_iter().next().ok_or("required_untyped_nullable missing in ObjectUntypedProps".to_string())?,
            not_required_untyped: intermediate_rep.not_required_untyped.into_iter().next(),
            not_required_untyped_nullable: intermediate_rep.not_required_untyped_nullable.into_iter().next(),
        })
    }
}


impl ObjectUntypedProps {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

// Methods for converting between IntoHeaderValue<ObjectWithArrayOfObjects> and HeaderValue

impl From<IntoHeaderValue<ObjectWithArrayOfObjects>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<ObjectWithArrayOfObjects>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<ObjectWithArrayOfObjects> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(ObjectWithArrayOfObjects::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct ObjectWithArrayOfObjects {
    #[serde(rename = "objectArray")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub object_array: Option<Vec<models::StringObject>>,

}

impl ObjectWithArrayOfObjects {
    pub fn new() -> ObjectWithArrayOfObjects {
        ObjectWithArrayOfObjects {
            object_array: None,
        }
    }
}

/// Converts the ObjectWithArrayOfObjects value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for ObjectWithArrayOfObjects {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref object_array) = self.object_array {
            params.push("objectArray".to_string());
            params.push(object_array.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",").to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectWithArrayOfObjects value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for ObjectWithArrayOfObjects {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub object_array: Vec<Vec<models::StringObject>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return Err("Missing value while parsing ObjectWithArrayOfObjects".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "objectArray" => return Err("Parsing a container in this style is not supported in ObjectWithArrayOfObjects".to_string()),
                    _ => return Err("Unexpected key while parsing ObjectWithArrayOfObjects".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(ObjectWithArrayOfObjects {
            object_array: intermediate_rep.object_array.into_iter().next(),
        })
    }
}


impl ObjectWithArrayOfObjects {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct OptionalObjectHeader(i32);

impl ::std::convert::From<i32> for OptionalObjectHeader {
    fn from(x: i32) -> Self {
        OptionalObjectHeader(x)
    }
}


impl ::std::convert::From<OptionalObjectHeader> for i32 {
    fn from(x: OptionalObjectHeader) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for OptionalObjectHeader {
    type Target = i32;
    fn deref(&self) -> &i32 {
        &self.0
    }
}

impl ::std::ops::DerefMut for OptionalObjectHeader {
    fn deref_mut(&mut self) -> &mut i32 {
        &mut self.0
    }
}


impl OptionalObjectHeader {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct RequiredObjectHeader(bool);

impl ::std::convert::From<bool> for RequiredObjectHeader {
    fn from(x: bool) -> Self {
        RequiredObjectHeader(x)
    }
}


impl ::std::convert::From<RequiredObjectHeader> for bool {
    fn from(x: RequiredObjectHeader) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for RequiredObjectHeader {
    type Target = bool;
    fn deref(&self) -> &bool {
        &self.0
    }
}

impl ::std::ops::DerefMut for RequiredObjectHeader {
    fn deref_mut(&mut self) -> &mut bool {
        &mut self.0
    }
}


impl RequiredObjectHeader {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGenericEnum))]
pub enum StringEnum { 
    #[serde(rename = "FOO")]
    FOO,
    #[serde(rename = "BAR")]
    BAR,
}

impl ::std::fmt::Display for StringEnum {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self { 
            StringEnum::FOO => write!(f, "{}", "FOO"),
            StringEnum::BAR => write!(f, "{}", "BAR"),
        }
    }
}

impl ::std::str::FromStr for StringEnum {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "FOO" => Ok(StringEnum::FOO),
            "BAR" => Ok(StringEnum::BAR),
            _ => Err(format!("Value not valid: {}", s)),
        }
    }
}

impl StringEnum {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct StringObject(String);

impl ::std::convert::From<String> for StringObject {
    fn from(x: String) -> Self {
        StringObject(x)
    }
}

impl std::str::FromStr for StringObject {
    type Err = ParseError;
    fn from_str(x: &str) -> Result<Self, Self::Err> {
        Ok(StringObject(x.to_string()))
    }
}

impl ::std::convert::From<StringObject> for String {
    fn from(x: StringObject) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for StringObject {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl ::std::ops::DerefMut for StringObject {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}


impl StringObject {
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

// Methods for converting between IntoHeaderValue<XmlArray> and HeaderValue

impl From<IntoHeaderValue<XmlArray>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<XmlArray>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<XmlArray> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(XmlArray::from_str(hdr_value.to_str().unwrap()).unwrap())
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

/// Converts the XmlArray value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for XmlArray {
    fn to_string(&self) -> String {
        self.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a XmlArray value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for XmlArray {
    type Err = <String as ::std::str::FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut items = vec![];
        for item in s.split(',')
        {
            items.push(item.parse()?);
        }
        Ok(XmlArray(items))
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
// Methods for converting between IntoHeaderValue<XmlObject> and HeaderValue

impl From<IntoHeaderValue<XmlObject>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<XmlObject>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<XmlObject> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(XmlObject::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}


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

/// Converts the XmlObject value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for XmlObject {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref inner_string) = self.inner_string {
            params.push("innerString".to_string());
            params.push(inner_string.to_string());
        }


        if let Some(ref other_inner_rename) = self.other_inner_rename {
            params.push("other_inner_rename".to_string());
            params.push(other_inner_rename.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a XmlObject value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for XmlObject {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub inner_string: Vec<String>,
            pub other_inner_rename: Vec<isize>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return Err("Missing value while parsing XmlObject".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "innerString" => intermediate_rep.inner_string.push(String::from_str(val).map_err(|x| format!("{}", x))?),
                    "other_inner_rename" => intermediate_rep.other_inner_rename.push(isize::from_str(val).map_err(|x| format!("{}", x))?),
                    _ => return Err("Unexpected key while parsing XmlObject".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(XmlObject {
            inner_string: intermediate_rep.inner_string.into_iter().next(),
            other_inner_rename: intermediate_rep.other_inner_rename.into_iter().next(),
        })
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
