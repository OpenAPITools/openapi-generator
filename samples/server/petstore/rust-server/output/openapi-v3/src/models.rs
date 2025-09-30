#![allow(unused_qualifications)]

use validator::Validate;

use crate::models;
#[cfg(any(feature = "client", feature = "server"))]
use crate::header;

/// Check that an object with only additional properties that references another object (e.g. an anyOf object) isn't treated as freeForm
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AdditionalPropertiesReferencedAnyOfObject(std::collections::HashMap<String, models::AnyOfProperty>);

impl std::convert::From<std::collections::HashMap<String, models::AnyOfProperty>> for AdditionalPropertiesReferencedAnyOfObject {
    fn from(x: std::collections::HashMap<String, models::AnyOfProperty>) -> Self {
        AdditionalPropertiesReferencedAnyOfObject(x)
    }
}

impl std::convert::From<AdditionalPropertiesReferencedAnyOfObject> for std::collections::HashMap<String, models::AnyOfProperty> {
    fn from(x: AdditionalPropertiesReferencedAnyOfObject) -> Self {
        x.0
    }
}

impl std::ops::Deref for AdditionalPropertiesReferencedAnyOfObject {
    type Target = std::collections::HashMap<String, models::AnyOfProperty>;
    fn deref(&self) -> &std::collections::HashMap<String, models::AnyOfProperty> {
        &self.0
    }
}

impl std::ops::DerefMut for AdditionalPropertiesReferencedAnyOfObject {
    fn deref_mut(&mut self) -> &mut std::collections::HashMap<String, models::AnyOfProperty> {
        &mut self.0
    }
}

/// Converts the AdditionalPropertiesReferencedAnyOfObject value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for AdditionalPropertiesReferencedAnyOfObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Display for this model is not supported
        write!(f, "")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AdditionalPropertiesReferencedAnyOfObject value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for AdditionalPropertiesReferencedAnyOfObject {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Err("Parsing AdditionalPropertiesReferencedAnyOfObject is not supported")
    }
}

// Methods for converting between header::IntoHeaderValue<AdditionalPropertiesReferencedAnyOfObject> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<AdditionalPropertiesReferencedAnyOfObject>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AdditionalPropertiesReferencedAnyOfObject>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AdditionalPropertiesReferencedAnyOfObject - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<AdditionalPropertiesReferencedAnyOfObject> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AdditionalPropertiesReferencedAnyOfObject as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into AdditionalPropertiesReferencedAnyOfObject - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<AdditionalPropertiesReferencedAnyOfObject>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<AdditionalPropertiesReferencedAnyOfObject>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<AdditionalPropertiesReferencedAnyOfObject>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<AdditionalPropertiesReferencedAnyOfObject> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <AdditionalPropertiesReferencedAnyOfObject as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into AdditionalPropertiesReferencedAnyOfObject - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl AdditionalPropertiesReferencedAnyOfObject {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AdditionalPropertiesWithList(std::collections::HashMap<String, Vec<String>>);

impl std::convert::From<std::collections::HashMap<String, Vec<String>>> for AdditionalPropertiesWithList {
    fn from(x: std::collections::HashMap<String, Vec<String>>) -> Self {
        AdditionalPropertiesWithList(x)
    }
}

impl std::convert::From<AdditionalPropertiesWithList> for std::collections::HashMap<String, Vec<String>> {
    fn from(x: AdditionalPropertiesWithList) -> Self {
        x.0
    }
}

impl std::ops::Deref for AdditionalPropertiesWithList {
    type Target = std::collections::HashMap<String, Vec<String>>;
    fn deref(&self) -> &std::collections::HashMap<String, Vec<String>> {
        &self.0
    }
}

impl std::ops::DerefMut for AdditionalPropertiesWithList {
    fn deref_mut(&mut self) -> &mut std::collections::HashMap<String, Vec<String>> {
        &mut self.0
    }
}

/// Converts the AdditionalPropertiesWithList value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for AdditionalPropertiesWithList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Display for this model is not supported
        write!(f, "")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AdditionalPropertiesWithList value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for AdditionalPropertiesWithList {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Err("Parsing AdditionalPropertiesWithList is not supported")
    }
}

// Methods for converting between header::IntoHeaderValue<AdditionalPropertiesWithList> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<AdditionalPropertiesWithList>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AdditionalPropertiesWithList>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AdditionalPropertiesWithList - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<AdditionalPropertiesWithList> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AdditionalPropertiesWithList as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into AdditionalPropertiesWithList - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<AdditionalPropertiesWithList>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<AdditionalPropertiesWithList>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<AdditionalPropertiesWithList>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<AdditionalPropertiesWithList> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <AdditionalPropertiesWithList as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into AdditionalPropertiesWithList - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl AdditionalPropertiesWithList {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AdditionalPropertiesWithNullable {
    #[serde(rename = "nullableString")]
    #[serde(deserialize_with = "swagger::nullable_format::deserialize_optional_nullable")]
    #[serde(default = "swagger::nullable_format::default_optional_nullable")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub nullable_string: Option<swagger::Nullable<String>>,

    #[serde(rename = "nullableMap")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub nullable_map: Option<std::collections::HashMap<String, swagger::Nullable<models::NullableObject>>>,

}


impl AdditionalPropertiesWithNullable {
    #[allow(clippy::new_without_default)]
    pub fn new() -> AdditionalPropertiesWithNullable {
        AdditionalPropertiesWithNullable {
            nullable_string: None,
            nullable_map: None,
        }
    }
}

/// Converts the AdditionalPropertiesWithNullable value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for AdditionalPropertiesWithNullable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            self.nullable_string.as_ref().map(|nullable_string| {
                [
                    "nullableString".to_string(),
                    nullable_string.as_ref().map_or("null".to_string(), |x| x.to_string()),
                ].join(",")
            }),
            // Skipping map nullableMap in query parameter serialization
        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AdditionalPropertiesWithNullable value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AdditionalPropertiesWithNullable {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub nullable_string: Vec<swagger::Nullable<String>>,
            pub nullable_map: Vec<std::collections::HashMap<String, swagger::Nullable<models::NullableObject>>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing AdditionalPropertiesWithNullable".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "nullableString" => return std::result::Result::Err("Parsing a nullable type in this style is not supported in AdditionalPropertiesWithNullable".to_string()),
                    "nullableMap" => return std::result::Result::Err("Parsing a container in this style is not supported in AdditionalPropertiesWithNullable".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing AdditionalPropertiesWithNullable".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(AdditionalPropertiesWithNullable {
            nullable_string: std::result::Result::Err("Nullable types not supported in AdditionalPropertiesWithNullable".to_string())?,
            nullable_map: intermediate_rep.nullable_map.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<AdditionalPropertiesWithNullable> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<AdditionalPropertiesWithNullable>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AdditionalPropertiesWithNullable>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AdditionalPropertiesWithNullable - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<AdditionalPropertiesWithNullable> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AdditionalPropertiesWithNullable as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into AdditionalPropertiesWithNullable - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<AdditionalPropertiesWithNullable>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<AdditionalPropertiesWithNullable>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<AdditionalPropertiesWithNullable>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<AdditionalPropertiesWithNullable> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <AdditionalPropertiesWithNullable as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into AdditionalPropertiesWithNullable - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl AdditionalPropertiesWithNullable {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

// Utility function for wrapping list elements when serializing xml
#[allow(non_snake_case)]
fn wrap_in_snake_another_xml_inner<S>(items: &Vec<String>, serializer: S) -> std::result::Result<S::Ok, S::Error>
where
    S: serde::ser::Serializer,
{
    use serde::ser::SerializeMap;

    let mut map = serializer.serialize_map(None)?;
    for ref item in items {
        map.serialize_key("snake_another_xml_inner")?;
        map.serialize_value(item)?;
    }
    map.end()
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AnotherXmlArray(
    #[serde(serialize_with = "wrap_in_snake_another_xml_inner")]
    Vec<String>
);

impl std::convert::From<Vec<String>> for AnotherXmlArray {
    fn from(x: Vec<String>) -> Self {
        AnotherXmlArray(x)
    }
}

impl std::convert::From<AnotherXmlArray> for Vec<String> {
    fn from(x: AnotherXmlArray) -> Self {
        x.0
    }
}

impl std::iter::FromIterator<String> for AnotherXmlArray {
    fn from_iter<U: IntoIterator<Item=String>>(u: U) -> Self {
        AnotherXmlArray(Vec::<String>::from_iter(u))
    }
}

impl std::iter::IntoIterator for AnotherXmlArray {
    type Item = String;
    type IntoIter = std::vec::IntoIter<String>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> std::iter::IntoIterator for &'a AnotherXmlArray {
    type Item = &'a String;
    type IntoIter = std::slice::Iter<'a, String>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a> std::iter::IntoIterator for &'a mut AnotherXmlArray {
    type Item = &'a mut String;
    type IntoIter = std::slice::IterMut<'a, String>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl std::ops::Deref for AnotherXmlArray {
    type Target = Vec<String>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for AnotherXmlArray {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Converts the AnotherXmlArray value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for AnotherXmlArray {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnotherXmlArray value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AnotherXmlArray {
    type Err = <String as std::str::FromStr>::Err;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut items = vec![];
        for item in s.split(',')
        {
            items.push(item.parse()?);
        }
        std::result::Result::Ok(AnotherXmlArray(items))
    }
}


// Methods for converting between header::IntoHeaderValue<AnotherXmlArray> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<AnotherXmlArray>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AnotherXmlArray>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AnotherXmlArray - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<AnotherXmlArray> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AnotherXmlArray as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into AnotherXmlArray - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<AnotherXmlArray>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<AnotherXmlArray>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<AnotherXmlArray>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<AnotherXmlArray> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <AnotherXmlArray as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into AnotherXmlArray - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl AnotherXmlArray {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
#[serde(rename = "snake_another_xml_inner")]
pub struct AnotherXmlInner(String);

impl std::convert::From<String> for AnotherXmlInner {
    fn from(x: String) -> Self {
        AnotherXmlInner(x)
    }
}

impl std::convert::From<AnotherXmlInner> for String {
    fn from(x: AnotherXmlInner) -> Self {
        x.0
    }
}

impl std::ops::Deref for AnotherXmlInner {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for AnotherXmlInner {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

impl std::fmt::Display for AnotherXmlInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.clone())
    }
}

impl std::str::FromStr for AnotherXmlInner {
    type Err = ::std::convert::Infallible;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(AnotherXmlInner(x.to_owned()))
    }
}

// Methods for converting between header::IntoHeaderValue<AnotherXmlInner> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<AnotherXmlInner>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AnotherXmlInner>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AnotherXmlInner - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<AnotherXmlInner> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AnotherXmlInner as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into AnotherXmlInner - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<AnotherXmlInner>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<AnotherXmlInner>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<AnotherXmlInner>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<AnotherXmlInner> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <AnotherXmlInner as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into AnotherXmlInner - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl AnotherXmlInner {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// An XML object
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
#[serde(rename = "snake_another_xml_object")]
pub struct AnotherXmlObject {
    #[serde(rename = "inner_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub inner_string: Option<String>,

}


impl AnotherXmlObject {
    #[allow(clippy::new_without_default)]
    pub fn new() -> AnotherXmlObject {
        AnotherXmlObject {
            inner_string: None,
        }
    }
}

/// Converts the AnotherXmlObject value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for AnotherXmlObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            self.inner_string.as_ref().map(|inner_string| {
                [
                    "inner_string".to_string(),
                    inner_string.to_string(),
                ].join(",")
            }),
        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnotherXmlObject value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AnotherXmlObject {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub inner_string: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing AnotherXmlObject".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "inner_string" => intermediate_rep.inner_string.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing AnotherXmlObject".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(AnotherXmlObject {
            inner_string: intermediate_rep.inner_string.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<AnotherXmlObject> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<AnotherXmlObject>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AnotherXmlObject>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AnotherXmlObject - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<AnotherXmlObject> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AnotherXmlObject as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into AnotherXmlObject - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<AnotherXmlObject>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<AnotherXmlObject>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<AnotherXmlObject>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<AnotherXmlObject> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <AnotherXmlObject as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into AnotherXmlObject - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
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
    pub(crate) fn as_xml(&self) -> String {
        // An empty string is used to indicate a global namespace in xmltree.
        let config = serde_xml_rs::SerdeXml::new()
            .namespace("", Self::NAMESPACE);
        config.to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AnyOfGet202Response(swagger::AnyOf2<models::StringObject,models::UuidObject>);

impl std::convert::From<swagger::AnyOf2<models::StringObject,models::UuidObject>> for AnyOfGet202Response {
    fn from(x: swagger::AnyOf2<models::StringObject,models::UuidObject>) -> Self {
        AnyOfGet202Response(x)
    }
}

impl std::convert::From<AnyOfGet202Response> for swagger::AnyOf2<models::StringObject,models::UuidObject> {
    fn from(x: AnyOfGet202Response) -> Self {
        x.0
    }
}

impl std::ops::Deref for AnyOfGet202Response {
    type Target = swagger::AnyOf2<models::StringObject,models::UuidObject>;
    fn deref(&self) -> &swagger::AnyOf2<models::StringObject,models::UuidObject> {
        &self.0
    }
}

impl std::ops::DerefMut for AnyOfGet202Response {
    fn deref_mut(&mut self) -> &mut swagger::AnyOf2<models::StringObject,models::UuidObject> {
        &mut self.0
    }
}

/// Converts the AnyOfGet202Response value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for AnyOfGet202Response {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Display for this model is not supported
        write!(f, "")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnyOfGet202Response value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for AnyOfGet202Response {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Err("Parsing AnyOfGet202Response is not supported")
    }
}

// Methods for converting between header::IntoHeaderValue<AnyOfGet202Response> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<AnyOfGet202Response>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AnyOfGet202Response>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AnyOfGet202Response - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<AnyOfGet202Response> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AnyOfGet202Response as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into AnyOfGet202Response - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<AnyOfGet202Response>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<AnyOfGet202Response>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<AnyOfGet202Response>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<AnyOfGet202Response> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <AnyOfGet202Response as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into AnyOfGet202Response - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl AnyOfGet202Response {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Test a model containing an anyOf of a hash map
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AnyOfHashMapObject(swagger::AnyOf2<String,std::collections::HashMap<String, String>>);

impl std::convert::From<swagger::AnyOf2<String,std::collections::HashMap<String, String>>> for AnyOfHashMapObject {
    fn from(x: swagger::AnyOf2<String,std::collections::HashMap<String, String>>) -> Self {
        AnyOfHashMapObject(x)
    }
}

impl std::convert::From<AnyOfHashMapObject> for swagger::AnyOf2<String,std::collections::HashMap<String, String>> {
    fn from(x: AnyOfHashMapObject) -> Self {
        x.0
    }
}

impl std::ops::Deref for AnyOfHashMapObject {
    type Target = swagger::AnyOf2<String,std::collections::HashMap<String, String>>;
    fn deref(&self) -> &swagger::AnyOf2<String,std::collections::HashMap<String, String>> {
        &self.0
    }
}

impl std::ops::DerefMut for AnyOfHashMapObject {
    fn deref_mut(&mut self) -> &mut swagger::AnyOf2<String,std::collections::HashMap<String, String>> {
        &mut self.0
    }
}

/// Converts the AnyOfHashMapObject value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for AnyOfHashMapObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Display for this model is not supported
        write!(f, "")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnyOfHashMapObject value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for AnyOfHashMapObject {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Err("Parsing AnyOfHashMapObject is not supported")
    }
}

// Methods for converting between header::IntoHeaderValue<AnyOfHashMapObject> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<AnyOfHashMapObject>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AnyOfHashMapObject>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AnyOfHashMapObject - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<AnyOfHashMapObject> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AnyOfHashMapObject as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into AnyOfHashMapObject - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<AnyOfHashMapObject>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<AnyOfHashMapObject>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<AnyOfHashMapObject>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<AnyOfHashMapObject> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <AnyOfHashMapObject as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into AnyOfHashMapObject - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl AnyOfHashMapObject {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Test a model containing an anyOf
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AnyOfObject(swagger::AnyOf2<models::AnyOfObjectAnyOf,String>);

impl std::convert::From<swagger::AnyOf2<models::AnyOfObjectAnyOf,String>> for AnyOfObject {
    fn from(x: swagger::AnyOf2<models::AnyOfObjectAnyOf,String>) -> Self {
        AnyOfObject(x)
    }
}

impl std::convert::From<AnyOfObject> for swagger::AnyOf2<models::AnyOfObjectAnyOf,String> {
    fn from(x: AnyOfObject) -> Self {
        x.0
    }
}

impl std::ops::Deref for AnyOfObject {
    type Target = swagger::AnyOf2<models::AnyOfObjectAnyOf,String>;
    fn deref(&self) -> &swagger::AnyOf2<models::AnyOfObjectAnyOf,String> {
        &self.0
    }
}

impl std::ops::DerefMut for AnyOfObject {
    fn deref_mut(&mut self) -> &mut swagger::AnyOf2<models::AnyOfObjectAnyOf,String> {
        &mut self.0
    }
}

/// Converts the AnyOfObject value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for AnyOfObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Display for this model is not supported
        write!(f, "")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnyOfObject value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for AnyOfObject {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Err("Parsing AnyOfObject is not supported")
    }
}

// Methods for converting between header::IntoHeaderValue<AnyOfObject> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<AnyOfObject>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AnyOfObject>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AnyOfObject - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<AnyOfObject> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AnyOfObject as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into AnyOfObject - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<AnyOfObject>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<AnyOfObject>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<AnyOfObject>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<AnyOfObject> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <AnyOfObject as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into AnyOfObject - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl AnyOfObject {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize, Hash)]
#[cfg_attr(feature = "conversion", derive(frunk_enum_derive::LabelledGenericEnum))]
pub enum AnyOfObjectAnyOf {
    #[serde(rename = "FOO")]
    Foo,
    #[serde(rename = "BAR")]
    Bar,
}

impl std::fmt::Display for AnyOfObjectAnyOf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            AnyOfObjectAnyOf::Foo => write!(f, "FOO"),
            AnyOfObjectAnyOf::Bar => write!(f, "BAR"),
        }
    }
}

impl std::str::FromStr for AnyOfObjectAnyOf {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "FOO" => std::result::Result::Ok(AnyOfObjectAnyOf::Foo),
            "BAR" => std::result::Result::Ok(AnyOfObjectAnyOf::Bar),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<AnyOfObjectAnyOf> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<AnyOfObjectAnyOf>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AnyOfObjectAnyOf>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AnyOfObjectAnyOf - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<AnyOfObjectAnyOf> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AnyOfObjectAnyOf as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into AnyOfObjectAnyOf - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<AnyOfObjectAnyOf>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<AnyOfObjectAnyOf>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<AnyOfObjectAnyOf>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<AnyOfObjectAnyOf> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <AnyOfObjectAnyOf as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into AnyOfObjectAnyOf - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl AnyOfObjectAnyOf {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Test containing an anyOf object
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AnyOfProperty {
    #[serde(rename = "requiredAnyOf")]
    pub required_any_of: models::AnyOfObject,

    #[serde(rename = "optionalAnyOf")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub optional_any_of: Option<models::Model12345AnyOfObject>,

}


impl AnyOfProperty {
    #[allow(clippy::new_without_default)]
    pub fn new(required_any_of: models::AnyOfObject, ) -> AnyOfProperty {
        AnyOfProperty {
            required_any_of,
            optional_any_of: None,
        }
    }
}

/// Converts the AnyOfProperty value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for AnyOfProperty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping non-primitive type requiredAnyOf in query parameter serialization
            // Skipping non-primitive type optionalAnyOf in query parameter serialization
        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnyOfProperty value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AnyOfProperty {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub required_any_of: Vec<models::AnyOfObject>,
            pub optional_any_of: Vec<models::Model12345AnyOfObject>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing AnyOfProperty".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "requiredAnyOf" => intermediate_rep.required_any_of.push(<models::AnyOfObject as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "optionalAnyOf" => intermediate_rep.optional_any_of.push(<models::Model12345AnyOfObject as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing AnyOfProperty".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(AnyOfProperty {
            required_any_of: intermediate_rep.required_any_of.into_iter().next().ok_or_else(|| "requiredAnyOf missing in AnyOfProperty".to_string())?,
            optional_any_of: intermediate_rep.optional_any_of.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<AnyOfProperty> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<AnyOfProperty>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AnyOfProperty>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AnyOfProperty - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<AnyOfProperty> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AnyOfProperty as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into AnyOfProperty - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<AnyOfProperty>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<AnyOfProperty>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<AnyOfProperty>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<AnyOfProperty> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <AnyOfProperty as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into AnyOfProperty - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl AnyOfProperty {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// An XML object
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
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
    #[allow(clippy::new_without_default)]
    pub fn new(inner_array: models::XmlArray, ) -> DuplicateXmlObject {
        DuplicateXmlObject {
            inner_string: None,
            inner_array,
        }
    }
}

/// Converts the DuplicateXmlObject value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for DuplicateXmlObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            self.inner_string.as_ref().map(|inner_string| {
                [
                    "inner_string".to_string(),
                    inner_string.to_string(),
                ].join(",")
            }),
            // Skipping non-primitive type inner_array in query parameter serialization
        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a DuplicateXmlObject value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl std::str::FromStr for DuplicateXmlObject {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub inner_string: Vec<String>,
            pub inner_array: Vec<models::XmlArray>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing DuplicateXmlObject".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "inner_string" => intermediate_rep.inner_string.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "inner_array" => intermediate_rep.inner_array.push(<models::XmlArray as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing DuplicateXmlObject".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(DuplicateXmlObject {
            inner_string: intermediate_rep.inner_string.into_iter().next(),
            inner_array: intermediate_rep.inner_array.into_iter().next().ok_or_else(|| "inner_array missing in DuplicateXmlObject".to_string())?,
        })
    }
}

// Methods for converting between header::IntoHeaderValue<DuplicateXmlObject> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<DuplicateXmlObject>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<DuplicateXmlObject>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for DuplicateXmlObject - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<DuplicateXmlObject> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <DuplicateXmlObject as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into DuplicateXmlObject - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<DuplicateXmlObject>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<DuplicateXmlObject>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<DuplicateXmlObject>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<DuplicateXmlObject> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <DuplicateXmlObject as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into DuplicateXmlObject - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
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
    pub(crate) fn as_xml(&self) -> String {
        // An empty string is used to indicate a global namespace in xmltree.
        let config = serde_xml_rs::SerdeXml::new()
            .namespace("", Self::NAMESPACE);
        config.to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Test a model containing a special character in the enum
/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize, Hash)]
#[cfg_attr(feature = "conversion", derive(frunk_enum_derive::LabelledGenericEnum))]
pub enum EnumWithStarObject {
    #[serde(rename = "FOO")]
    Foo,
    #[serde(rename = "BAR")]
    Bar,
    #[serde(rename = "*")]
    Star,
}

impl std::fmt::Display for EnumWithStarObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            EnumWithStarObject::Foo => write!(f, "FOO"),
            EnumWithStarObject::Bar => write!(f, "BAR"),
            EnumWithStarObject::Star => write!(f, "*"),
        }
    }
}

impl std::str::FromStr for EnumWithStarObject {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "FOO" => std::result::Result::Ok(EnumWithStarObject::Foo),
            "BAR" => std::result::Result::Ok(EnumWithStarObject::Bar),
            "*" => std::result::Result::Ok(EnumWithStarObject::Star),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<EnumWithStarObject> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<EnumWithStarObject>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<EnumWithStarObject>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for EnumWithStarObject - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<EnumWithStarObject> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <EnumWithStarObject as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into EnumWithStarObject - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<EnumWithStarObject>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<EnumWithStarObject>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<EnumWithStarObject>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<EnumWithStarObject> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <EnumWithStarObject as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into EnumWithStarObject - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl EnumWithStarObject {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Err(String);

impl std::convert::From<String> for Err {
    fn from(x: String) -> Self {
        Err(x)
    }
}

impl std::convert::From<Err> for String {
    fn from(x: Err) -> Self {
        x.0
    }
}

impl std::ops::Deref for Err {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for Err {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

impl std::fmt::Display for Err {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.clone())
    }
}

impl std::str::FromStr for Err {
    type Err = ::std::convert::Infallible;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(Err(x.to_owned()))
    }
}

// Methods for converting between header::IntoHeaderValue<Err> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Err>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Err>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Err - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Err> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Err as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Err - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Err>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Err>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Err>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Err> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Err as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Err - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Err {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Error(String);

impl std::convert::From<String> for Error {
    fn from(x: String) -> Self {
        Error(x)
    }
}

impl std::convert::From<Error> for String {
    fn from(x: Error) -> Self {
        x.0
    }
}

impl std::ops::Deref for Error {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for Error {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.clone())
    }
}

impl std::str::FromStr for Error {
    type Err = ::std::convert::Infallible;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(Error(x.to_owned()))
    }
}

// Methods for converting between header::IntoHeaderValue<Error> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Error>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Error>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Error - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Error> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Error as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Error - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Error>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Error>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Error>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Error> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Error as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Error - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Error {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Test a model containing an anyOf that starts with a number
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Model12345AnyOfObject(swagger::AnyOf2<models::Model12345AnyOfObjectAnyOf,String>);

impl std::convert::From<swagger::AnyOf2<models::Model12345AnyOfObjectAnyOf,String>> for Model12345AnyOfObject {
    fn from(x: swagger::AnyOf2<models::Model12345AnyOfObjectAnyOf,String>) -> Self {
        Model12345AnyOfObject(x)
    }
}

impl std::convert::From<Model12345AnyOfObject> for swagger::AnyOf2<models::Model12345AnyOfObjectAnyOf,String> {
    fn from(x: Model12345AnyOfObject) -> Self {
        x.0
    }
}

impl std::ops::Deref for Model12345AnyOfObject {
    type Target = swagger::AnyOf2<models::Model12345AnyOfObjectAnyOf,String>;
    fn deref(&self) -> &swagger::AnyOf2<models::Model12345AnyOfObjectAnyOf,String> {
        &self.0
    }
}

impl std::ops::DerefMut for Model12345AnyOfObject {
    fn deref_mut(&mut self) -> &mut swagger::AnyOf2<models::Model12345AnyOfObjectAnyOf,String> {
        &mut self.0
    }
}

/// Converts the Model12345AnyOfObject value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for Model12345AnyOfObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Display for this model is not supported
        write!(f, "")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Model12345AnyOfObject value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for Model12345AnyOfObject {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Err("Parsing Model12345AnyOfObject is not supported")
    }
}

// Methods for converting between header::IntoHeaderValue<Model12345AnyOfObject> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Model12345AnyOfObject>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Model12345AnyOfObject>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Model12345AnyOfObject - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Model12345AnyOfObject> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Model12345AnyOfObject as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Model12345AnyOfObject - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Model12345AnyOfObject>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Model12345AnyOfObject>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Model12345AnyOfObject>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Model12345AnyOfObject> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Model12345AnyOfObject as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Model12345AnyOfObject - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Model12345AnyOfObject {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize, Hash)]
#[cfg_attr(feature = "conversion", derive(frunk_enum_derive::LabelledGenericEnum))]
pub enum Model12345AnyOfObjectAnyOf {
    #[serde(rename = "FOO")]
    Foo,
    #[serde(rename = "BAR")]
    Bar,
    #[serde(rename = "*")]
    Star,
}

impl std::fmt::Display for Model12345AnyOfObjectAnyOf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Model12345AnyOfObjectAnyOf::Foo => write!(f, "FOO"),
            Model12345AnyOfObjectAnyOf::Bar => write!(f, "BAR"),
            Model12345AnyOfObjectAnyOf::Star => write!(f, "*"),
        }
    }
}

impl std::str::FromStr for Model12345AnyOfObjectAnyOf {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "FOO" => std::result::Result::Ok(Model12345AnyOfObjectAnyOf::Foo),
            "BAR" => std::result::Result::Ok(Model12345AnyOfObjectAnyOf::Bar),
            "*" => std::result::Result::Ok(Model12345AnyOfObjectAnyOf::Star),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<Model12345AnyOfObjectAnyOf> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Model12345AnyOfObjectAnyOf>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Model12345AnyOfObjectAnyOf>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Model12345AnyOfObjectAnyOf - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Model12345AnyOfObjectAnyOf> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Model12345AnyOfObjectAnyOf as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Model12345AnyOfObjectAnyOf - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Model12345AnyOfObjectAnyOf>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Model12345AnyOfObjectAnyOf>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Model12345AnyOfObjectAnyOf>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Model12345AnyOfObjectAnyOf> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Model12345AnyOfObjectAnyOf as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Model12345AnyOfObjectAnyOf - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Model12345AnyOfObjectAnyOf {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct MultigetGet201Response {
    #[serde(rename = "foo")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub foo: Option<String>,

}


impl MultigetGet201Response {
    #[allow(clippy::new_without_default)]
    pub fn new() -> MultigetGet201Response {
        MultigetGet201Response {
            foo: None,
        }
    }
}

/// Converts the MultigetGet201Response value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for MultigetGet201Response {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            self.foo.as_ref().map(|foo| {
                [
                    "foo".to_string(),
                    foo.to_string(),
                ].join(",")
            }),
        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MultigetGet201Response value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl std::str::FromStr for MultigetGet201Response {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub foo: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing MultigetGet201Response".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "foo" => intermediate_rep.foo.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing MultigetGet201Response".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(MultigetGet201Response {
            foo: intermediate_rep.foo.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<MultigetGet201Response> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<MultigetGet201Response>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<MultigetGet201Response>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for MultigetGet201Response - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<MultigetGet201Response> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <MultigetGet201Response as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into MultigetGet201Response - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<MultigetGet201Response>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<MultigetGet201Response>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<MultigetGet201Response>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<MultigetGet201Response> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <MultigetGet201Response as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into MultigetGet201Response - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl MultigetGet201Response {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct MyId(i32);

impl std::convert::From<i32> for MyId {
    fn from(x: i32) -> Self {
        MyId(x)
    }
}

impl std::convert::From<MyId> for i32 {
    fn from(x: MyId) -> Self {
        x.0
    }
}

impl std::ops::Deref for MyId {
    type Target = i32;
    fn deref(&self) -> &i32 {
        &self.0
    }
}

impl std::ops::DerefMut for MyId {
    fn deref_mut(&mut self) -> &mut i32 {
        &mut self.0
    }
}

/// Converts the MyId value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for MyId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MyId value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for MyId {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match std::str::FromStr::from_str(s) {
             std::result::Result::Ok(r) => std::result::Result::Ok(MyId(r)),
             std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {s} to MyId: {e:?}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<MyId> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<MyId>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<MyId>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for MyId - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<MyId> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <MyId as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into MyId - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<MyId>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<MyId>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<MyId>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<MyId> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <MyId as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into MyId - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl MyId {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct MyIdList(
    Vec<i32>
);

impl std::convert::From<Vec<i32>> for MyIdList {
    fn from(x: Vec<i32>) -> Self {
        MyIdList(x)
    }
}

impl std::convert::From<MyIdList> for Vec<i32> {
    fn from(x: MyIdList) -> Self {
        x.0
    }
}

impl std::iter::FromIterator<i32> for MyIdList {
    fn from_iter<U: IntoIterator<Item=i32>>(u: U) -> Self {
        MyIdList(Vec::<i32>::from_iter(u))
    }
}

impl std::iter::IntoIterator for MyIdList {
    type Item = i32;
    type IntoIter = std::vec::IntoIter<i32>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> std::iter::IntoIterator for &'a MyIdList {
    type Item = &'a i32;
    type IntoIter = std::slice::Iter<'a, i32>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a> std::iter::IntoIterator for &'a mut MyIdList {
    type Item = &'a mut i32;
    type IntoIter = std::slice::IterMut<'a, i32>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl std::ops::Deref for MyIdList {
    type Target = Vec<i32>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for MyIdList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Converts the MyIdList value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for MyIdList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MyIdList value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl std::str::FromStr for MyIdList {
    type Err = <i32 as std::str::FromStr>::Err;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut items = vec![];
        for item in s.split(',')
        {
            items.push(item.parse()?);
        }
        std::result::Result::Ok(MyIdList(items))
    }
}


// Methods for converting between header::IntoHeaderValue<MyIdList> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<MyIdList>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<MyIdList>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for MyIdList - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<MyIdList> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <MyIdList as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into MyIdList - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<MyIdList>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<MyIdList>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<MyIdList>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<MyIdList> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <MyIdList as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into MyIdList - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl MyIdList {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct NullableObject(String);

impl std::convert::From<String> for NullableObject {
    fn from(x: String) -> Self {
        NullableObject(x)
    }
}

impl std::convert::From<NullableObject> for String {
    fn from(x: NullableObject) -> Self {
        x.0
    }
}

impl std::ops::Deref for NullableObject {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for NullableObject {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

impl std::fmt::Display for NullableObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.clone())
    }
}

impl std::str::FromStr for NullableObject {
    type Err = ::std::convert::Infallible;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(NullableObject(x.to_owned()))
    }
}

// Methods for converting between header::IntoHeaderValue<NullableObject> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<NullableObject>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<NullableObject>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for NullableObject - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<NullableObject> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <NullableObject as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into NullableObject - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<NullableObject>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<NullableObject>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<NullableObject>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<NullableObject> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <NullableObject as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into NullableObject - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl NullableObject {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
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

    #[serde(rename = "min_item_test")]
    #[validate(
            length(min = 1),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub min_item_test: Option<Vec<i32>>,

    #[serde(rename = "max_item_test")]
    #[validate(
            length(max = 2),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub max_item_test: Option<Vec<i32>>,

    #[serde(rename = "min_max_item_test")]
    #[validate(
            length(min = 1, max = 3),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub min_max_item_test: Option<Vec<i32>>,

}


impl NullableTest {
    #[allow(clippy::new_without_default)]
    pub fn new(nullable: swagger::Nullable<String>, ) -> NullableTest {
        NullableTest {
            nullable,
            nullable_with_null_default: None,
            nullable_with_present_default: Some(swagger::Nullable::Present("default".to_string())),
            nullable_with_no_default: None,
            nullable_array: None,
            min_item_test: None,
            max_item_test: None,
            min_max_item_test: None,
        }
    }
}

/// Converts the NullableTest value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for NullableTest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            Some("nullable".to_string()),
            Some(self.nullable.as_ref().map_or("null".to_string(), |x| x.to_string())),
            self.nullable_with_null_default.as_ref().map(|nullable_with_null_default| {
                [
                    "nullableWithNullDefault".to_string(),
                    nullable_with_null_default.as_ref().map_or("null".to_string(), |x| x.to_string()),
                ].join(",")
            }),
            self.nullable_with_present_default.as_ref().map(|nullable_with_present_default| {
                [
                    "nullableWithPresentDefault".to_string(),
                    nullable_with_present_default.as_ref().map_or("null".to_string(), |x| x.to_string()),
                ].join(",")
            }),
            self.nullable_with_no_default.as_ref().map(|nullable_with_no_default| {
                [
                    "nullableWithNoDefault".to_string(),
                    nullable_with_no_default.as_ref().map_or("null".to_string(), |x| x.to_string()),
                ].join(",")
            }),
            self.nullable_array.as_ref().map(|nullable_array| {
                [
                    "nullableArray".to_string(),
                    nullable_array.as_ref().map_or("null".to_string(), |x| x.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",")),
                ].join(",")
            }),
            self.min_item_test.as_ref().map(|min_item_test| {
                [
                    "min_item_test".to_string(),
                    min_item_test.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","),
                ].join(",")
            }),
            self.max_item_test.as_ref().map(|max_item_test| {
                [
                    "max_item_test".to_string(),
                    max_item_test.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","),
                ].join(",")
            }),
            self.min_max_item_test.as_ref().map(|min_max_item_test| {
                [
                    "min_max_item_test".to_string(),
                    min_max_item_test.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","),
                ].join(",")
            }),
        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a NullableTest value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl std::str::FromStr for NullableTest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub nullable: Vec<swagger::Nullable<String>>,
            pub nullable_with_null_default: Vec<swagger::Nullable<String>>,
            pub nullable_with_present_default: Vec<swagger::Nullable<String>>,
            pub nullable_with_no_default: Vec<swagger::Nullable<String>>,
            pub nullable_array: Vec<swagger::Nullable<Vec<String>>>,
            pub min_item_test: Vec<Vec<i32>>,
            pub max_item_test: Vec<Vec<i32>>,
            pub min_max_item_test: Vec<Vec<i32>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing NullableTest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "nullable" => return std::result::Result::Err("Parsing a nullable type in this style is not supported in NullableTest".to_string()),
                    "nullableWithNullDefault" => return std::result::Result::Err("Parsing a nullable type in this style is not supported in NullableTest".to_string()),
                    "nullableWithPresentDefault" => return std::result::Result::Err("Parsing a nullable type in this style is not supported in NullableTest".to_string()),
                    "nullableWithNoDefault" => return std::result::Result::Err("Parsing a nullable type in this style is not supported in NullableTest".to_string()),
                    "nullableArray" => return std::result::Result::Err("Parsing a container in this style is not supported in NullableTest".to_string()),
                    "min_item_test" => return std::result::Result::Err("Parsing a container in this style is not supported in NullableTest".to_string()),
                    "max_item_test" => return std::result::Result::Err("Parsing a container in this style is not supported in NullableTest".to_string()),
                    "min_max_item_test" => return std::result::Result::Err("Parsing a container in this style is not supported in NullableTest".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing NullableTest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(NullableTest {
            nullable: std::result::Result::Err("Nullable types not supported in NullableTest".to_string())?,
            nullable_with_null_default: std::result::Result::Err("Nullable types not supported in NullableTest".to_string())?,
            nullable_with_present_default: std::result::Result::Err("Nullable types not supported in NullableTest".to_string())?,
            nullable_with_no_default: std::result::Result::Err("Nullable types not supported in NullableTest".to_string())?,
            nullable_array: std::result::Result::Err("Nullable types not supported in NullableTest".to_string())?,
            min_item_test: intermediate_rep.min_item_test.into_iter().next(),
            max_item_test: intermediate_rep.max_item_test.into_iter().next(),
            min_max_item_test: intermediate_rep.min_max_item_test.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<NullableTest> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<NullableTest>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<NullableTest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for NullableTest - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<NullableTest> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <NullableTest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into NullableTest - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<NullableTest>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<NullableTest>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<NullableTest>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<NullableTest> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <NullableTest as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into NullableTest - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl NullableTest {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ObjectHeader {
    #[serde(rename = "requiredObjectHeader")]
    pub required_object_header: bool,

    #[serde(rename = "optionalObjectHeader")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub optional_object_header: Option<i32>,

}


impl ObjectHeader {
    #[allow(clippy::new_without_default)]
    pub fn new(required_object_header: bool, ) -> ObjectHeader {
        ObjectHeader {
            required_object_header,
            optional_object_header: None,
        }
    }
}

/// Converts the ObjectHeader value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for ObjectHeader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            Some("requiredObjectHeader".to_string()),
            Some(self.required_object_header.to_string()),
            self.optional_object_header.as_ref().map(|optional_object_header| {
                [
                    "optionalObjectHeader".to_string(),
                    optional_object_header.to_string(),
                ].join(",")
            }),
        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectHeader value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ObjectHeader {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub required_object_header: Vec<bool>,
            pub optional_object_header: Vec<i32>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ObjectHeader".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "requiredObjectHeader" => intermediate_rep.required_object_header.push(<bool as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "optionalObjectHeader" => intermediate_rep.optional_object_header.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ObjectHeader".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ObjectHeader {
            required_object_header: intermediate_rep.required_object_header.into_iter().next().ok_or_else(|| "requiredObjectHeader missing in ObjectHeader".to_string())?,
            optional_object_header: intermediate_rep.optional_object_header.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ObjectHeader> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectHeader>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ObjectHeader>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ObjectHeader - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<ObjectHeader> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ObjectHeader as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into ObjectHeader - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<ObjectHeader>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<ObjectHeader>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<ObjectHeader>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<ObjectHeader> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <ObjectHeader as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into ObjectHeader - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl ObjectHeader {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ObjectParam {
    #[serde(rename = "requiredParam")]
    pub required_param: bool,

    #[serde(rename = "optionalParam")]
    #[validate(
            range(min = 1u64, max = 10000000000000000000u64),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub optional_param: Option<u64>,

}


impl ObjectParam {
    #[allow(clippy::new_without_default)]
    pub fn new(required_param: bool, ) -> ObjectParam {
        ObjectParam {
            required_param,
            optional_param: None,
        }
    }
}

/// Converts the ObjectParam value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for ObjectParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            Some("requiredParam".to_string()),
            Some(self.required_param.to_string()),
            self.optional_param.as_ref().map(|optional_param| {
                [
                    "optionalParam".to_string(),
                    optional_param.to_string(),
                ].join(",")
            }),
        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectParam value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ObjectParam {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub required_param: Vec<bool>,
            pub optional_param: Vec<u64>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ObjectParam".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "requiredParam" => intermediate_rep.required_param.push(<bool as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "optionalParam" => intermediate_rep.optional_param.push(<u64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ObjectParam".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ObjectParam {
            required_param: intermediate_rep.required_param.into_iter().next().ok_or_else(|| "requiredParam missing in ObjectParam".to_string())?,
            optional_param: intermediate_rep.optional_param.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ObjectParam> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectParam>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ObjectParam>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ObjectParam - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<ObjectParam> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ObjectParam as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into ObjectParam - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<ObjectParam>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<ObjectParam>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<ObjectParam>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<ObjectParam> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <ObjectParam as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into ObjectParam - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl ObjectParam {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ObjectUntypedProps {
    #[serde(rename = "required_untyped")]
    pub required_untyped: serde_json::Value,

    #[serde(rename = "required_untyped_nullable")]
    pub required_untyped_nullable: swagger::Nullable<serde_json::Value>,

    #[serde(rename = "not_required_untyped")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub not_required_untyped: Option<serde_json::Value>,

    #[serde(rename = "not_required_untyped_nullable")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub not_required_untyped_nullable: Option<serde_json::Value>,

}


impl ObjectUntypedProps {
    #[allow(clippy::new_without_default)]
    pub fn new(required_untyped: serde_json::Value, required_untyped_nullable: swagger::Nullable<serde_json::Value>, ) -> ObjectUntypedProps {
        ObjectUntypedProps {
            required_untyped,
            required_untyped_nullable,
            not_required_untyped: None,
            not_required_untyped_nullable: None,
        }
    }
}

/// Converts the ObjectUntypedProps value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for ObjectUntypedProps {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping non-primitive type required_untyped in query parameter serialization
            // Skipping non-primitive type required_untyped_nullable in query parameter serialization
            // Skipping non-primitive type not_required_untyped in query parameter serialization
            // Skipping non-primitive type not_required_untyped_nullable in query parameter serialization
        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectUntypedProps value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ObjectUntypedProps {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub required_untyped: Vec<serde_json::Value>,
            pub required_untyped_nullable: Vec<swagger::Nullable<serde_json::Value>>,
            pub not_required_untyped: Vec<serde_json::Value>,
            pub not_required_untyped_nullable: Vec<serde_json::Value>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ObjectUntypedProps".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "required_untyped" => intermediate_rep.required_untyped.push(<serde_json::Value as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    "required_untyped_nullable" => return std::result::Result::Err("Parsing a nullable type in this style is not supported in ObjectUntypedProps".to_string()),
                    #[allow(clippy::redundant_clone)]
                    "not_required_untyped" => intermediate_rep.not_required_untyped.push(<serde_json::Value as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "not_required_untyped_nullable" => intermediate_rep.not_required_untyped_nullable.push(<serde_json::Value as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ObjectUntypedProps".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ObjectUntypedProps {
            required_untyped: intermediate_rep.required_untyped.into_iter().next().ok_or_else(|| "required_untyped missing in ObjectUntypedProps".to_string())?,
            required_untyped_nullable: std::result::Result::Err("Nullable types not supported in ObjectUntypedProps".to_string())?,
            not_required_untyped: intermediate_rep.not_required_untyped.into_iter().next(),
            not_required_untyped_nullable: intermediate_rep.not_required_untyped_nullable.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ObjectUntypedProps> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectUntypedProps>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ObjectUntypedProps>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ObjectUntypedProps - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<ObjectUntypedProps> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ObjectUntypedProps as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into ObjectUntypedProps - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<ObjectUntypedProps>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<ObjectUntypedProps>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<ObjectUntypedProps>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<ObjectUntypedProps> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <ObjectUntypedProps as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into ObjectUntypedProps - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl ObjectUntypedProps {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ObjectWithArrayOfObjects {
    #[serde(rename = "objectArray")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub object_array: Option<Vec<models::StringObject>>,

}


impl ObjectWithArrayOfObjects {
    #[allow(clippy::new_without_default)]
    pub fn new() -> ObjectWithArrayOfObjects {
        ObjectWithArrayOfObjects {
            object_array: None,
        }
    }
}

/// Converts the ObjectWithArrayOfObjects value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for ObjectWithArrayOfObjects {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            self.object_array.as_ref().map(|object_array| {
                [
                    "objectArray".to_string(),
                    object_array.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","),
                ].join(",")
            }),
        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectWithArrayOfObjects value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ObjectWithArrayOfObjects {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub object_array: Vec<Vec<models::StringObject>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ObjectWithArrayOfObjects".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "objectArray" => return std::result::Result::Err("Parsing a container in this style is not supported in ObjectWithArrayOfObjects".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing ObjectWithArrayOfObjects".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ObjectWithArrayOfObjects {
            object_array: intermediate_rep.object_array.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ObjectWithArrayOfObjects> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectWithArrayOfObjects>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ObjectWithArrayOfObjects>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ObjectWithArrayOfObjects - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<ObjectWithArrayOfObjects> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ObjectWithArrayOfObjects as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into ObjectWithArrayOfObjects - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<ObjectWithArrayOfObjects>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<ObjectWithArrayOfObjects>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<ObjectWithArrayOfObjects>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<ObjectWithArrayOfObjects> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <ObjectWithArrayOfObjects as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into ObjectWithArrayOfObjects - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl ObjectWithArrayOfObjects {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Ok(String);

impl std::convert::From<String> for Ok {
    fn from(x: String) -> Self {
        Ok(x)
    }
}

impl std::convert::From<Ok> for String {
    fn from(x: Ok) -> Self {
        x.0
    }
}

impl std::ops::Deref for Ok {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for Ok {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

impl std::fmt::Display for Ok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.clone())
    }
}

impl std::str::FromStr for Ok {
    type Err = ::std::convert::Infallible;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(Ok(x.to_owned()))
    }
}

// Methods for converting between header::IntoHeaderValue<Ok> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Ok>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Ok>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Ok - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Ok> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Ok as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Ok - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Ok>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Ok>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Ok>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Ok> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Ok as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Ok - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Ok {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct OneOfGet200Response(swagger::OneOf2<i32,Vec<String>>);

impl std::convert::From<swagger::OneOf2<i32,Vec<String>>> for OneOfGet200Response {
    fn from(x: swagger::OneOf2<i32,Vec<String>>) -> Self {
        OneOfGet200Response(x)
    }
}

impl std::convert::From<OneOfGet200Response> for swagger::OneOf2<i32,Vec<String>> {
    fn from(x: OneOfGet200Response) -> Self {
        x.0
    }
}

impl std::ops::Deref for OneOfGet200Response {
    type Target = swagger::OneOf2<i32,Vec<String>>;
    fn deref(&self) -> &swagger::OneOf2<i32,Vec<String>> {
        &self.0
    }
}

impl std::ops::DerefMut for OneOfGet200Response {
    fn deref_mut(&mut self) -> &mut swagger::OneOf2<i32,Vec<String>> {
        &mut self.0
    }
}

/// Converts the OneOfGet200Response value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for OneOfGet200Response {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Display for this model is not supported
        write!(f, "")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a OneOfGet200Response value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for OneOfGet200Response {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Err("Parsing OneOfGet200Response is not supported")
    }
}

// Methods for converting between header::IntoHeaderValue<OneOfGet200Response> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<OneOfGet200Response>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<OneOfGet200Response>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for OneOfGet200Response - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<OneOfGet200Response> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <OneOfGet200Response as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into OneOfGet200Response - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<OneOfGet200Response>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<OneOfGet200Response>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<OneOfGet200Response>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<OneOfGet200Response> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <OneOfGet200Response as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into OneOfGet200Response - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl OneOfGet200Response {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct OptionalObjectHeader(i32);

impl std::convert::From<i32> for OptionalObjectHeader {
    fn from(x: i32) -> Self {
        OptionalObjectHeader(x)
    }
}

impl std::convert::From<OptionalObjectHeader> for i32 {
    fn from(x: OptionalObjectHeader) -> Self {
        x.0
    }
}

impl std::ops::Deref for OptionalObjectHeader {
    type Target = i32;
    fn deref(&self) -> &i32 {
        &self.0
    }
}

impl std::ops::DerefMut for OptionalObjectHeader {
    fn deref_mut(&mut self) -> &mut i32 {
        &mut self.0
    }
}

/// Converts the OptionalObjectHeader value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for OptionalObjectHeader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a OptionalObjectHeader value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for OptionalObjectHeader {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match std::str::FromStr::from_str(s) {
             std::result::Result::Ok(r) => std::result::Result::Ok(OptionalObjectHeader(r)),
             std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {s} to OptionalObjectHeader: {e:?}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<OptionalObjectHeader> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<OptionalObjectHeader>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<OptionalObjectHeader>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for OptionalObjectHeader - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<OptionalObjectHeader> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <OptionalObjectHeader as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into OptionalObjectHeader - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<OptionalObjectHeader>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<OptionalObjectHeader>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<OptionalObjectHeader>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<OptionalObjectHeader> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <OptionalObjectHeader as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into OptionalObjectHeader - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl OptionalObjectHeader {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct RequiredObjectHeader(bool);

impl std::convert::From<bool> for RequiredObjectHeader {
    fn from(x: bool) -> Self {
        RequiredObjectHeader(x)
    }
}

impl std::convert::From<RequiredObjectHeader> for bool {
    fn from(x: RequiredObjectHeader) -> Self {
        x.0
    }
}

impl std::ops::Deref for RequiredObjectHeader {
    type Target = bool;
    fn deref(&self) -> &bool {
        &self.0
    }
}

impl std::ops::DerefMut for RequiredObjectHeader {
    fn deref_mut(&mut self) -> &mut bool {
        &mut self.0
    }
}

/// Converts the RequiredObjectHeader value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for RequiredObjectHeader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a RequiredObjectHeader value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for RequiredObjectHeader {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match std::str::FromStr::from_str(s) {
             std::result::Result::Ok(r) => std::result::Result::Ok(RequiredObjectHeader(r)),
             std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {s} to RequiredObjectHeader: {e:?}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<RequiredObjectHeader> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<RequiredObjectHeader>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<RequiredObjectHeader>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for RequiredObjectHeader - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<RequiredObjectHeader> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <RequiredObjectHeader as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into RequiredObjectHeader - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<RequiredObjectHeader>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<RequiredObjectHeader>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<RequiredObjectHeader>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<RequiredObjectHeader> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <RequiredObjectHeader as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into RequiredObjectHeader - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl RequiredObjectHeader {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Result(String);

impl std::convert::From<String> for Result {
    fn from(x: String) -> Self {
        Result(x)
    }
}

impl std::convert::From<Result> for String {
    fn from(x: Result) -> Self {
        x.0
    }
}

impl std::ops::Deref for Result {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for Result {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

impl std::fmt::Display for Result {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.clone())
    }
}

impl std::str::FromStr for Result {
    type Err = ::std::convert::Infallible;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(Result(x.to_owned()))
    }
}

// Methods for converting between header::IntoHeaderValue<Result> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Result>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Result>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Result - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Result> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Result as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Result - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Result>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Result>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Result>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Result> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Result as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Result - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Result {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize, Hash)]
#[cfg_attr(feature = "conversion", derive(frunk_enum_derive::LabelledGenericEnum))]
pub enum StringEnum {
    #[serde(rename = "FOO")]
    Foo,
    #[serde(rename = "BAR")]
    Bar,
}

impl std::fmt::Display for StringEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            StringEnum::Foo => write!(f, "FOO"),
            StringEnum::Bar => write!(f, "BAR"),
        }
    }
}

impl std::str::FromStr for StringEnum {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "FOO" => std::result::Result::Ok(StringEnum::Foo),
            "BAR" => std::result::Result::Ok(StringEnum::Bar),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<StringEnum> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<StringEnum>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<StringEnum>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for StringEnum - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<StringEnum> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <StringEnum as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into StringEnum - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<StringEnum>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<StringEnum>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<StringEnum>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<StringEnum> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <StringEnum as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into StringEnum - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl StringEnum {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct StringObject(String);

impl std::convert::From<String> for StringObject {
    fn from(x: String) -> Self {
        StringObject(x)
    }
}

impl std::convert::From<StringObject> for String {
    fn from(x: StringObject) -> Self {
        x.0
    }
}

impl std::ops::Deref for StringObject {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for StringObject {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

impl std::fmt::Display for StringObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.clone())
    }
}

impl std::str::FromStr for StringObject {
    type Err = ::std::convert::Infallible;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(StringObject(x.to_owned()))
    }
}

// Methods for converting between header::IntoHeaderValue<StringObject> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<StringObject>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<StringObject>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for StringObject - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<StringObject> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <StringObject as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into StringObject - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<StringObject>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<StringObject>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<StringObject>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<StringObject> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <StringObject as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into StringObject - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl StringObject {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Test a model containing a UUID
#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct UuidObject(uuid::Uuid);

impl std::convert::From<uuid::Uuid> for UuidObject {
    fn from(x: uuid::Uuid) -> Self {
        UuidObject(x)
    }
}

impl std::convert::From<UuidObject> for uuid::Uuid {
    fn from(x: UuidObject) -> Self {
        x.0
    }
}

impl std::ops::Deref for UuidObject {
    type Target = uuid::Uuid;
    fn deref(&self) -> &uuid::Uuid {
        &self.0
    }
}

impl std::ops::DerefMut for UuidObject {
    fn deref_mut(&mut self) -> &mut uuid::Uuid {
        &mut self.0
    }
}

/// Converts the UuidObject value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for UuidObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a UuidObject value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for UuidObject {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match std::str::FromStr::from_str(s) {
             std::result::Result::Ok(r) => std::result::Result::Ok(UuidObject(r)),
             std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {s} to UuidObject: {e:?}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<UuidObject> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<UuidObject>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<UuidObject>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for UuidObject - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<UuidObject> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <UuidObject as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into UuidObject - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<UuidObject>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<UuidObject>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<UuidObject>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<UuidObject> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <UuidObject as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into UuidObject - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl UuidObject {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

// Utility function for wrapping list elements when serializing xml
#[allow(non_snake_case)]
fn wrap_in_camelXmlInner<S>(items: &Vec<String>, serializer: S) -> std::result::Result<S::Ok, S::Error>
where
    S: serde::ser::Serializer,
{
    use serde::ser::SerializeMap;

    let mut map = serializer.serialize_map(None)?;
    for ref item in items {
        map.serialize_key("camelXmlInner")?;
        map.serialize_value(item)?;
    }
    map.end()
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct XmlArray(
    #[serde(serialize_with = "wrap_in_camelXmlInner")]
    Vec<String>
);

impl std::convert::From<Vec<String>> for XmlArray {
    fn from(x: Vec<String>) -> Self {
        XmlArray(x)
    }
}

impl std::convert::From<XmlArray> for Vec<String> {
    fn from(x: XmlArray) -> Self {
        x.0
    }
}

impl std::iter::FromIterator<String> for XmlArray {
    fn from_iter<U: IntoIterator<Item=String>>(u: U) -> Self {
        XmlArray(Vec::<String>::from_iter(u))
    }
}

impl std::iter::IntoIterator for XmlArray {
    type Item = String;
    type IntoIter = std::vec::IntoIter<String>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> std::iter::IntoIterator for &'a XmlArray {
    type Item = &'a String;
    type IntoIter = std::slice::Iter<'a, String>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a> std::iter::IntoIterator for &'a mut XmlArray {
    type Item = &'a mut String;
    type IntoIter = std::slice::IterMut<'a, String>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl std::ops::Deref for XmlArray {
    type Target = Vec<String>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for XmlArray {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Converts the XmlArray value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for XmlArray {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a XmlArray value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl std::str::FromStr for XmlArray {
    type Err = <String as std::str::FromStr>::Err;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut items = vec![];
        for item in s.split(',')
        {
            items.push(item.parse()?);
        }
        std::result::Result::Ok(XmlArray(items))
    }
}


// Methods for converting between header::IntoHeaderValue<XmlArray> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<XmlArray>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<XmlArray>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for XmlArray - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<XmlArray> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <XmlArray as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into XmlArray - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<XmlArray>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<XmlArray>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<XmlArray>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<XmlArray> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <XmlArray as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into XmlArray - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl XmlArray {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
#[serde(rename = "camelXmlInner")]
pub struct XmlInner(String);

impl std::convert::From<String> for XmlInner {
    fn from(x: String) -> Self {
        XmlInner(x)
    }
}

impl std::convert::From<XmlInner> for String {
    fn from(x: XmlInner) -> Self {
        x.0
    }
}

impl std::ops::Deref for XmlInner {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for XmlInner {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

impl std::fmt::Display for XmlInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.clone())
    }
}

impl std::str::FromStr for XmlInner {
    type Err = ::std::convert::Infallible;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(XmlInner(x.to_owned()))
    }
}

// Methods for converting between header::IntoHeaderValue<XmlInner> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<XmlInner>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<XmlInner>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for XmlInner - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<XmlInner> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <XmlInner as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into XmlInner - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<XmlInner>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<XmlInner>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<XmlInner>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<XmlInner> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <XmlInner as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into XmlInner - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl XmlInner {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// An XML object
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
#[serde(rename = "camelXmlObject")]
pub struct XmlObject {
    #[serde(rename = "innerString")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub inner_string: Option<String>,

    #[serde(rename = "other_inner_rename")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub other_inner_rename: Option<i32>,

}


impl XmlObject {
    #[allow(clippy::new_without_default)]
    pub fn new() -> XmlObject {
        XmlObject {
            inner_string: None,
            other_inner_rename: None,
        }
    }
}

/// Converts the XmlObject value to the Query Parameters representation (style=form, explode=false)
/// specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde serializer
impl std::fmt::Display for XmlObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            self.inner_string.as_ref().map(|inner_string| {
                [
                    "innerString".to_string(),
                    inner_string.to_string(),
                ].join(",")
            }),
            self.other_inner_rename.as_ref().map(|other_inner_rename| {
                [
                    "other_inner_rename".to_string(),
                    other_inner_rename.to_string(),
                ].join(",")
            }),
        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a XmlObject value
/// as specified in <https://swagger.io/docs/specification/serialization/>
/// Should be implemented in a serde deserializer
impl std::str::FromStr for XmlObject {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub inner_string: Vec<String>,
            pub other_inner_rename: Vec<i32>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing XmlObject".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "innerString" => intermediate_rep.inner_string.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "other_inner_rename" => intermediate_rep.other_inner_rename.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing XmlObject".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(XmlObject {
            inner_string: intermediate_rep.inner_string.into_iter().next(),
            other_inner_rename: intermediate_rep.other_inner_rename.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<XmlObject> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<XmlObject>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<XmlObject>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for XmlObject - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<XmlObject> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <XmlObject as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into XmlObject - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<XmlObject>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<XmlObject>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}",))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<XmlObject>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<XmlObject> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <XmlObject as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into XmlObject - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
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
    pub(crate) fn as_xml(&self) -> String {
        // An empty string is used to indicate a global namespace in xmltree.
        let config = serde_xml_rs::SerdeXml::new()
            .namespace("", Self::NAMESPACE);
        config.to_string(&self).expect("impossible to fail to serialize")
    }
}
