#![allow(unused_qualifications)]

use http::HeaderValue;
use validator::Validate;

#[cfg(feature = "server")]
use crate::header;
use crate::{models, types::*};

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AnyOfGetQueryParams {
    /// list of any of objects
    #[serde(rename = "any-of")]
    #[validate(length(min = 1))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub any_of: Option<Vec<models::AnyOfObject>>,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct CallbackWithHeaderPostQueryParams {
    #[serde(rename = "url")]
    pub url: String,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ComplexQueryParamGetQueryParams {
    #[serde(rename = "list-of-strings")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub list_of_strings: Option<Vec<models::StringObject>>,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct EnumInPathPathParamGetPathParams {
    pub path_param: models::StringEnum,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct JsonComplexQueryParamGetQueryParams {
    #[serde(rename = "list-of-strings")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub list_of_strings: Option<Vec<models::StringObject>>,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct MandatoryRequestHeaderGetHeaderParams {
    pub x_header: String,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ParamgetGetQueryParams {
    /// The stuff to get
    #[serde(rename = "uuid")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub uuid: Option<uuid::Uuid>,
    /// Some object to pass as query parameter
    #[serde(rename = "someObject")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub some_object: Option<crate::types::Object>,
    /// Some list to pass as query parameter
    #[serde(rename = "someList")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub some_list: Option<Vec<models::MyId>>,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct RegisterCallbackPostQueryParams {
    #[serde(rename = "url")]
    pub url: String,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct GetRepoInfoPathParams {
    pub repo_id: String,
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AdditionalPropertiesWithList(std::collections::HashMap<String, Vec<String>>);

impl validator::Validate for AdditionalPropertiesWithList {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<std::collections::HashMap<String, Vec<String>>>
    for AdditionalPropertiesWithList
{
    fn from(x: std::collections::HashMap<String, Vec<String>>) -> Self {
        AdditionalPropertiesWithList(x)
    }
}

impl std::convert::From<AdditionalPropertiesWithList>
    for std::collections::HashMap<String, Vec<String>>
{
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
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for AdditionalPropertiesWithList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Skipping additionalProperties in query parameter serialization
        write!(f, "")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AdditionalPropertiesWithList value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for AdditionalPropertiesWithList {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Err(
            "Parsing additionalProperties for AdditionalPropertiesWithList is not supported",
        )
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AnotherXmlArray(Vec<String>);

impl validator::Validate for AnotherXmlArray {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

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
    fn from_iter<U: IntoIterator<Item = String>>(u: U) -> Self {
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
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for AnotherXmlArray {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnotherXmlArray value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AnotherXmlArray {
    type Err = <String as std::str::FromStr>::Err;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut items = vec![];
        for item in s.split(',') {
            items.push(item.parse()?);
        }
        std::result::Result::Ok(AnotherXmlArray(items))
    }
}

// Methods for converting between header::IntoHeaderValue<AnotherXmlArray> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<AnotherXmlArray>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<AnotherXmlArray>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Invalid header value for AnotherXmlArray - value: {} is invalid {}",
                hdr_value, e
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<AnotherXmlArray> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <AnotherXmlArray as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        "Unable to convert header value '{}' into AnotherXmlArray - {}",
                        value, err
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Unable to convert header: {:?} to string: {}",
                hdr_value, e
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AnotherXmlInner(String);

impl validator::Validate for AnotherXmlInner {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<String> for AnotherXmlInner {
    fn from(x: String) -> Self {
        AnotherXmlInner(x)
    }
}

impl std::fmt::Display for AnotherXmlInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl std::str::FromStr for AnotherXmlInner {
    type Err = std::string::ParseError;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(AnotherXmlInner(x.to_string()))
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

/// An XML object

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AnotherXmlObject {
    #[serde(rename = "inner_string")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inner_string: Option<String>,
}

impl AnotherXmlObject {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> AnotherXmlObject {
        AnotherXmlObject { inner_string: None }
    }
}

/// Converts the AnotherXmlObject value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for AnotherXmlObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![self
            .inner_string
            .as_ref()
            .map(|inner_string| ["inner_string".to_string(), inner_string.to_string()].join(","))];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnotherXmlObject value
/// as specified in https://swagger.io/docs/specification/serialization/
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
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing AnotherXmlObject".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "inner_string" => intermediate_rep.inner_string.push(
                        <String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing AnotherXmlObject".to_string(),
                        )
                    }
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

// Methods for converting between header::IntoHeaderValue<AnotherXmlObject> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<AnotherXmlObject>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<AnotherXmlObject>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Invalid header value for AnotherXmlObject - value: {} is invalid {}",
                hdr_value, e
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<AnotherXmlObject> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <AnotherXmlObject as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        "Unable to convert header value '{}' into AnotherXmlObject - {}",
                        value, err
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Unable to convert header: {:?} to string: {}",
                hdr_value, e
            )),
        }
    }
}

/// Any of:
/// - String
/// - uuid::Uuid
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnyOfGet202Response(Box<serde_json::value::RawValue>);

impl validator::Validate for AnyOfGet202Response {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnyOfGet202Response value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AnyOfGet202Response {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

impl PartialEq for AnyOfGet202Response {
    fn eq(&self, other: &Self) -> bool {
        self.0.get() == other.0.get()
    }
}

/// Test a model containing an anyOf

/// Any of:
/// - String
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnyOfObject(Box<serde_json::value::RawValue>);

impl validator::Validate for AnyOfObject {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnyOfObject value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AnyOfObject {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

impl PartialEq for AnyOfObject {
    fn eq(&self, other: &Self) -> bool {
        self.0.get() == other.0.get()
    }
}

/// Test containing an anyOf object

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AnyOfProperty {
    #[serde(rename = "requiredAnyOf")]
    pub required_any_of: models::AnyOfObject,

    #[serde(rename = "optionalAnyOf")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub optional_any_of: Option<models::Model12345AnyOfObject>,
}

impl AnyOfProperty {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(required_any_of: models::AnyOfObject) -> AnyOfProperty {
        AnyOfProperty {
            required_any_of,
            optional_any_of: None,
        }
    }
}

/// Converts the AnyOfProperty value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for AnyOfProperty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping requiredAnyOf in query parameter serialization

            // Skipping optionalAnyOf in query parameter serialization

        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnyOfProperty value
/// as specified in https://swagger.io/docs/specification/serialization/
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
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing AnyOfProperty".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "requiredAnyOf" => intermediate_rep.required_any_of.push(
                        <models::AnyOfObject as std::str::FromStr>::from_str(val)
                            .map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "optionalAnyOf" => intermediate_rep.optional_any_of.push(
                        <models::Model12345AnyOfObject as std::str::FromStr>::from_str(val)
                            .map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing AnyOfProperty".to_string(),
                        )
                    }
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(AnyOfProperty {
            required_any_of: intermediate_rep
                .required_any_of
                .into_iter()
                .next()
                .ok_or_else(|| "requiredAnyOf missing in AnyOfProperty".to_string())?,
            optional_any_of: intermediate_rep.optional_any_of.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<AnyOfProperty> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<AnyOfProperty>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<AnyOfProperty>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Invalid header value for AnyOfProperty - value: {} is invalid {}",
                hdr_value, e
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<AnyOfProperty> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <AnyOfProperty as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        "Unable to convert header value '{}' into AnyOfProperty - {}",
                        value, err
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Unable to convert header: {:?} to string: {}",
                hdr_value, e
            )),
        }
    }
}

/// An XML object

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct DuplicateXmlObject {
    #[serde(rename = "inner_string")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inner_string: Option<String>,

    #[serde(rename = "inner_array")]
    pub inner_array: models::XmlArray,
}

impl DuplicateXmlObject {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(inner_array: models::XmlArray) -> DuplicateXmlObject {
        DuplicateXmlObject {
            inner_string: None,
            inner_array,
        }
    }
}

/// Converts the DuplicateXmlObject value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for DuplicateXmlObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            self.inner_string.as_ref().map(|inner_string| {
                ["inner_string".to_string(), inner_string.to_string()].join(",")
            }),
            // Skipping inner_array in query parameter serialization
        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a DuplicateXmlObject value
/// as specified in https://swagger.io/docs/specification/serialization/
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
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing DuplicateXmlObject".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "inner_string" => intermediate_rep.inner_string.push(
                        <String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "inner_array" => intermediate_rep.inner_array.push(
                        <models::XmlArray as std::str::FromStr>::from_str(val)
                            .map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing DuplicateXmlObject".to_string(),
                        )
                    }
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(DuplicateXmlObject {
            inner_string: intermediate_rep.inner_string.into_iter().next(),
            inner_array: intermediate_rep
                .inner_array
                .into_iter()
                .next()
                .ok_or_else(|| "inner_array missing in DuplicateXmlObject".to_string())?,
        })
    }
}

// Methods for converting between header::IntoHeaderValue<DuplicateXmlObject> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<DuplicateXmlObject>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<DuplicateXmlObject>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Invalid header value for DuplicateXmlObject - value: {} is invalid {}",
                hdr_value, e
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<DuplicateXmlObject> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <DuplicateXmlObject as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        "Unable to convert header value '{}' into DuplicateXmlObject - {}",
                        value, err
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Unable to convert header: {:?} to string: {}",
                hdr_value, e
            )),
        }
    }
}

/// Test a model containing a special character in the enum
/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
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
            _ => std::result::Result::Err(format!("Value not valid: {}", s)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Err(String);

impl validator::Validate for Err {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<String> for Err {
    fn from(x: String) -> Self {
        Err(x)
    }
}

impl std::fmt::Display for Err {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl std::str::FromStr for Err {
    type Err = std::string::ParseError;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(Err(x.to_string()))
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

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Error(String);

impl validator::Validate for Error {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<String> for Error {
    fn from(x: String) -> Self {
        Error(x)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl std::str::FromStr for Error {
    type Err = std::string::ParseError;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(Error(x.to_string()))
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

/// Test a model containing an anyOf that starts with a number

/// Any of:
/// - String
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Model12345AnyOfObject(Box<serde_json::value::RawValue>);

impl validator::Validate for Model12345AnyOfObject {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Model12345AnyOfObject value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Model12345AnyOfObject {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

impl PartialEq for Model12345AnyOfObject {
    fn eq(&self, other: &Self) -> bool {
        self.0.get() == other.0.get()
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct MultigetGet201Response {
    #[serde(rename = "foo")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub foo: Option<String>,
}

impl MultigetGet201Response {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> MultigetGet201Response {
        MultigetGet201Response { foo: None }
    }
}

/// Converts the MultigetGet201Response value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for MultigetGet201Response {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![self
            .foo
            .as_ref()
            .map(|foo| ["foo".to_string(), foo.to_string()].join(","))];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MultigetGet201Response value
/// as specified in https://swagger.io/docs/specification/serialization/
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
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing MultigetGet201Response".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "foo" => intermediate_rep.foo.push(
                        <String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing MultigetGet201Response".to_string(),
                        )
                    }
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

// Methods for converting between header::IntoHeaderValue<MultigetGet201Response> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<MultigetGet201Response>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<MultigetGet201Response>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Invalid header value for MultigetGet201Response - value: {} is invalid {}",
                hdr_value, e
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<MultigetGet201Response> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <MultigetGet201Response as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        "Unable to convert header value '{}' into MultigetGet201Response - {}",
                        value, err
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Unable to convert header: {:?} to string: {}",
                hdr_value, e
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct MyId(i32);

impl validator::Validate for MyId {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

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

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct MyIdList(Vec<i32>);

impl validator::Validate for MyIdList {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

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
    fn from_iter<U: IntoIterator<Item = i32>>(u: U) -> Self {
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
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for MyIdList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MyIdList value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for MyIdList {
    type Err = <i32 as std::str::FromStr>::Err;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut items = vec![];
        for item in s.split(',') {
            items.push(item.parse()?);
        }
        std::result::Result::Ok(MyIdList(items))
    }
}

// Methods for converting between header::IntoHeaderValue<MyIdList> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<MyIdList>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<MyIdList>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Invalid header value for MyIdList - value: {} is invalid {}",
                hdr_value, e
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<MyIdList> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <MyIdList as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        "Unable to convert header value '{}' into MyIdList - {}",
                        value, err
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Unable to convert header: {:?} to string: {}",
                hdr_value, e
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct NullableTest {
    #[serde(rename = "nullable")]
    pub nullable: Nullable<String>,

    #[serde(rename = "nullableWithNullDefault")]
    #[serde(deserialize_with = "deserialize_optional_nullable")]
    #[serde(default = "default_optional_nullable")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nullable_with_null_default: Option<Nullable<String>>,

    #[serde(rename = "nullableWithPresentDefault")]
    #[serde(deserialize_with = "deserialize_optional_nullable")]
    #[serde(default = "default_optional_nullable")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nullable_with_present_default: Option<Nullable<String>>,

    #[serde(rename = "nullableWithNoDefault")]
    #[serde(deserialize_with = "deserialize_optional_nullable")]
    #[serde(default = "default_optional_nullable")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nullable_with_no_default: Option<Nullable<String>>,

    #[serde(rename = "nullableArray")]
    #[serde(deserialize_with = "deserialize_optional_nullable")]
    #[serde(default = "default_optional_nullable")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nullable_array: Option<Nullable<Vec<String>>>,

    #[serde(rename = "min_item_test")]
    #[validate(length(min = 1))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_item_test: Option<Vec<i32>>,

    #[serde(rename = "max_item_test")]
    #[validate(length(max = 2))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_item_test: Option<Vec<i32>>,

    #[serde(rename = "min_max_item_test")]
    #[validate(length(min = 1, max = 3))]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_max_item_test: Option<Vec<i32>>,
}

impl NullableTest {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(nullable: Nullable<String>) -> NullableTest {
        NullableTest {
            nullable,
            nullable_with_null_default: None,
            nullable_with_present_default: Some(Nullable::Present("default".to_string())),
            nullable_with_no_default: None,
            nullable_array: None,
            min_item_test: None,
            max_item_test: None,
            min_max_item_test: None,
        }
    }
}

/// Converts the NullableTest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for NullableTest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            Some("nullable".to_string()),
            Some(
                self.nullable
                    .as_ref()
                    .map_or("null".to_string(), |x| x.to_string()),
            ),
            self.nullable_with_null_default
                .as_ref()
                .map(|nullable_with_null_default| {
                    [
                        "nullableWithNullDefault".to_string(),
                        nullable_with_null_default
                            .as_ref()
                            .map_or("null".to_string(), |x| x.to_string()),
                    ]
                    .join(",")
                }),
            self.nullable_with_present_default
                .as_ref()
                .map(|nullable_with_present_default| {
                    [
                        "nullableWithPresentDefault".to_string(),
                        nullable_with_present_default
                            .as_ref()
                            .map_or("null".to_string(), |x| x.to_string()),
                    ]
                    .join(",")
                }),
            self.nullable_with_no_default
                .as_ref()
                .map(|nullable_with_no_default| {
                    [
                        "nullableWithNoDefault".to_string(),
                        nullable_with_no_default
                            .as_ref()
                            .map_or("null".to_string(), |x| x.to_string()),
                    ]
                    .join(",")
                }),
            self.nullable_array.as_ref().map(|nullable_array| {
                [
                    "nullableArray".to_string(),
                    nullable_array.as_ref().map_or("null".to_string(), |x| {
                        x.iter()
                            .map(|x| x.to_string())
                            .collect::<Vec<_>>()
                            .join(",")
                    }),
                ]
                .join(",")
            }),
            self.min_item_test.as_ref().map(|min_item_test| {
                [
                    "min_item_test".to_string(),
                    min_item_test
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(","),
                ]
                .join(",")
            }),
            self.max_item_test.as_ref().map(|max_item_test| {
                [
                    "max_item_test".to_string(),
                    max_item_test
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(","),
                ]
                .join(",")
            }),
            self.min_max_item_test.as_ref().map(|min_max_item_test| {
                [
                    "min_max_item_test".to_string(),
                    min_max_item_test
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(","),
                ]
                .join(",")
            }),
        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a NullableTest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for NullableTest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub nullable: Vec<String>,
            pub nullable_with_null_default: Vec<String>,
            pub nullable_with_present_default: Vec<String>,
            pub nullable_with_no_default: Vec<String>,
            pub nullable_array: Vec<Vec<String>>,
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
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing NullableTest".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "nullable" => return std::result::Result::Err(
                        "Parsing a nullable type in this style is not supported in NullableTest"
                            .to_string(),
                    ),
                    "nullableWithNullDefault" => return std::result::Result::Err(
                        "Parsing a nullable type in this style is not supported in NullableTest"
                            .to_string(),
                    ),
                    "nullableWithPresentDefault" => return std::result::Result::Err(
                        "Parsing a nullable type in this style is not supported in NullableTest"
                            .to_string(),
                    ),
                    "nullableWithNoDefault" => return std::result::Result::Err(
                        "Parsing a nullable type in this style is not supported in NullableTest"
                            .to_string(),
                    ),
                    "nullableArray" => {
                        return std::result::Result::Err(
                            "Parsing a container in this style is not supported in NullableTest"
                                .to_string(),
                        )
                    }
                    "min_item_test" => {
                        return std::result::Result::Err(
                            "Parsing a container in this style is not supported in NullableTest"
                                .to_string(),
                        )
                    }
                    "max_item_test" => {
                        return std::result::Result::Err(
                            "Parsing a container in this style is not supported in NullableTest"
                                .to_string(),
                        )
                    }
                    "min_max_item_test" => {
                        return std::result::Result::Err(
                            "Parsing a container in this style is not supported in NullableTest"
                                .to_string(),
                        )
                    }
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing NullableTest".to_string(),
                        )
                    }
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(NullableTest {
            nullable: std::result::Result::Err(
                "Nullable types not supported in NullableTest".to_string(),
            )?,
            nullable_with_null_default: std::result::Result::Err(
                "Nullable types not supported in NullableTest".to_string(),
            )?,
            nullable_with_present_default: std::result::Result::Err(
                "Nullable types not supported in NullableTest".to_string(),
            )?,
            nullable_with_no_default: std::result::Result::Err(
                "Nullable types not supported in NullableTest".to_string(),
            )?,
            nullable_array: std::result::Result::Err(
                "Nullable types not supported in NullableTest".to_string(),
            )?,
            min_item_test: intermediate_rep.min_item_test.into_iter().next(),
            max_item_test: intermediate_rep.max_item_test.into_iter().next(),
            min_max_item_test: intermediate_rep.min_max_item_test.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<NullableTest> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<NullableTest>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<NullableTest>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Invalid header value for NullableTest - value: {} is invalid {}",
                hdr_value, e
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<NullableTest> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <NullableTest as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        "Unable to convert header value '{}' into NullableTest - {}",
                        value, err
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Unable to convert header: {:?} to string: {}",
                hdr_value, e
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ObjectHeader {
    #[serde(rename = "requiredObjectHeader")]
    pub required_object_header: bool,

    #[serde(rename = "optionalObjectHeader")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub optional_object_header: Option<i32>,
}

impl ObjectHeader {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(required_object_header: bool) -> ObjectHeader {
        ObjectHeader {
            required_object_header,
            optional_object_header: None,
        }
    }
}

/// Converts the ObjectHeader value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for ObjectHeader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            Some("requiredObjectHeader".to_string()),
            Some(self.required_object_header.to_string()),
            self.optional_object_header
                .as_ref()
                .map(|optional_object_header| {
                    [
                        "optionalObjectHeader".to_string(),
                        optional_object_header.to_string(),
                    ]
                    .join(",")
                }),
        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectHeader value
/// as specified in https://swagger.io/docs/specification/serialization/
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
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing ObjectHeader".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "requiredObjectHeader" => intermediate_rep.required_object_header.push(
                        <bool as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "optionalObjectHeader" => intermediate_rep.optional_object_header.push(
                        <i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing ObjectHeader".to_string(),
                        )
                    }
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ObjectHeader {
            required_object_header: intermediate_rep
                .required_object_header
                .into_iter()
                .next()
                .ok_or_else(|| "requiredObjectHeader missing in ObjectHeader".to_string())?,
            optional_object_header: intermediate_rep.optional_object_header.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ObjectHeader> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectHeader>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<ObjectHeader>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Invalid header value for ObjectHeader - value: {} is invalid {}",
                hdr_value, e
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<ObjectHeader> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <ObjectHeader as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        "Unable to convert header value '{}' into ObjectHeader - {}",
                        value, err
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Unable to convert header: {:?} to string: {}",
                hdr_value, e
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ObjectParam {
    #[serde(rename = "requiredParam")]
    pub required_param: bool,

    #[serde(rename = "optionalParam")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub optional_param: Option<i32>,
}

impl ObjectParam {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(required_param: bool) -> ObjectParam {
        ObjectParam {
            required_param,
            optional_param: None,
        }
    }
}

/// Converts the ObjectParam value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for ObjectParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            Some("requiredParam".to_string()),
            Some(self.required_param.to_string()),
            self.optional_param.as_ref().map(|optional_param| {
                ["optionalParam".to_string(), optional_param.to_string()].join(",")
            }),
        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectParam value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ObjectParam {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub required_param: Vec<bool>,
            pub optional_param: Vec<i32>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing ObjectParam".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "requiredParam" => intermediate_rep.required_param.push(
                        <bool as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "optionalParam" => intermediate_rep.optional_param.push(
                        <i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing ObjectParam".to_string(),
                        )
                    }
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ObjectParam {
            required_param: intermediate_rep
                .required_param
                .into_iter()
                .next()
                .ok_or_else(|| "requiredParam missing in ObjectParam".to_string())?,
            optional_param: intermediate_rep.optional_param.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ObjectParam> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectParam>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<ObjectParam>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Invalid header value for ObjectParam - value: {} is invalid {}",
                hdr_value, e
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<ObjectParam> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <ObjectParam as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        "Unable to convert header value '{}' into ObjectParam - {}",
                        value, err
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Unable to convert header: {:?} to string: {}",
                hdr_value, e
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ObjectUntypedProps {
    #[serde(rename = "required_untyped")]
    pub required_untyped: crate::types::Object,

    #[serde(rename = "required_untyped_nullable")]
    pub required_untyped_nullable: Nullable<crate::types::Object>,

    #[serde(rename = "not_required_untyped")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub not_required_untyped: Option<crate::types::Object>,

    #[serde(rename = "not_required_untyped_nullable")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub not_required_untyped_nullable: Option<crate::types::Object>,
}

impl ObjectUntypedProps {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(
        required_untyped: crate::types::Object,
        required_untyped_nullable: Nullable<crate::types::Object>,
    ) -> ObjectUntypedProps {
        ObjectUntypedProps {
            required_untyped,
            required_untyped_nullable,
            not_required_untyped: None,
            not_required_untyped_nullable: None,
        }
    }
}

/// Converts the ObjectUntypedProps value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for ObjectUntypedProps {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping required_untyped in query parameter serialization

            // Skipping required_untyped_nullable in query parameter serialization

            // Skipping not_required_untyped in query parameter serialization

            // Skipping not_required_untyped_nullable in query parameter serialization

        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectUntypedProps value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ObjectUntypedProps {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub required_untyped: Vec<crate::types::Object>,
            pub required_untyped_nullable: Vec<crate::types::Object>,
            pub not_required_untyped: Vec<crate::types::Object>,
            pub not_required_untyped_nullable: Vec<crate::types::Object>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing ObjectUntypedProps".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "required_untyped" => intermediate_rep.required_untyped.push(<crate::types::Object as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    "required_untyped_nullable" => return std::result::Result::Err("Parsing a nullable type in this style is not supported in ObjectUntypedProps".to_string()),
                    #[allow(clippy::redundant_clone)]
                    "not_required_untyped" => intermediate_rep.not_required_untyped.push(<crate::types::Object as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "not_required_untyped_nullable" => intermediate_rep.not_required_untyped_nullable.push(<crate::types::Object as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ObjectUntypedProps".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ObjectUntypedProps {
            required_untyped: intermediate_rep
                .required_untyped
                .into_iter()
                .next()
                .ok_or_else(|| "required_untyped missing in ObjectUntypedProps".to_string())?,
            required_untyped_nullable: std::result::Result::Err(
                "Nullable types not supported in ObjectUntypedProps".to_string(),
            )?,
            not_required_untyped: intermediate_rep.not_required_untyped.into_iter().next(),
            not_required_untyped_nullable: intermediate_rep
                .not_required_untyped_nullable
                .into_iter()
                .next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ObjectUntypedProps> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectUntypedProps>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<ObjectUntypedProps>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Invalid header value for ObjectUntypedProps - value: {} is invalid {}",
                hdr_value, e
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<ObjectUntypedProps> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <ObjectUntypedProps as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        "Unable to convert header value '{}' into ObjectUntypedProps - {}",
                        value, err
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Unable to convert header: {:?} to string: {}",
                hdr_value, e
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ObjectWithArrayOfObjects {
    #[serde(rename = "objectArray")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub object_array: Option<Vec<models::StringObject>>,
}

impl ObjectWithArrayOfObjects {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> ObjectWithArrayOfObjects {
        ObjectWithArrayOfObjects { object_array: None }
    }
}

/// Converts the ObjectWithArrayOfObjects value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for ObjectWithArrayOfObjects {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![self.object_array.as_ref().map(|object_array| {
            [
                "objectArray".to_string(),
                object_array
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(","),
            ]
            .join(",")
        })];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectWithArrayOfObjects value
/// as specified in https://swagger.io/docs/specification/serialization/
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
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing ObjectWithArrayOfObjects".to_string(),
                    )
                }
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

// Methods for converting between header::IntoHeaderValue<ObjectWithArrayOfObjects> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectWithArrayOfObjects>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<ObjectWithArrayOfObjects>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Invalid header value for ObjectWithArrayOfObjects - value: {} is invalid {}",
                hdr_value, e
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<ObjectWithArrayOfObjects> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <ObjectWithArrayOfObjects as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        "Unable to convert header value '{}' into ObjectWithArrayOfObjects - {}",
                        value, err
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Unable to convert header: {:?} to string: {}",
                hdr_value, e
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Ok(String);

impl validator::Validate for Ok {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<String> for Ok {
    fn from(x: String) -> Self {
        Ok(x)
    }
}

impl std::fmt::Display for Ok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl std::str::FromStr for Ok {
    type Err = std::string::ParseError;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(Ok(x.to_string()))
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

/// One of:
/// - Vec<String>
/// - i32
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct OneOfGet200Response(Box<serde_json::value::RawValue>);

impl validator::Validate for OneOfGet200Response {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a OneOfGet200Response value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for OneOfGet200Response {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

impl PartialEq for OneOfGet200Response {
    fn eq(&self, other: &Self) -> bool {
        self.0.get() == other.0.get()
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct OptionalObjectHeader(i32);

impl validator::Validate for OptionalObjectHeader {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

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

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct RequiredObjectHeader(bool);

impl validator::Validate for RequiredObjectHeader {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

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

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Result(String);

impl validator::Validate for Result {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<String> for Result {
    fn from(x: String) -> Self {
        Result(x)
    }
}

impl std::fmt::Display for Result {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl std::str::FromStr for Result {
    type Err = std::string::ParseError;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(Result(x.to_string()))
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

/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
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
            _ => std::result::Result::Err(format!("Value not valid: {}", s)),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct StringObject(String);

impl validator::Validate for StringObject {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<String> for StringObject {
    fn from(x: String) -> Self {
        StringObject(x)
    }
}

impl std::fmt::Display for StringObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl std::str::FromStr for StringObject {
    type Err = std::string::ParseError;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(StringObject(x.to_string()))
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

/// Test a model containing a UUID
#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct UuidObject(uuid::Uuid);

impl validator::Validate for UuidObject {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

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

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct XmlArray(Vec<String>);

impl validator::Validate for XmlArray {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

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
    fn from_iter<U: IntoIterator<Item = String>>(u: U) -> Self {
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
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for XmlArray {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.iter()
                .map(|x| x.to_string())
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a XmlArray value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for XmlArray {
    type Err = <String as std::str::FromStr>::Err;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut items = vec![];
        for item in s.split(',') {
            items.push(item.parse()?);
        }
        std::result::Result::Ok(XmlArray(items))
    }
}

// Methods for converting between header::IntoHeaderValue<XmlArray> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<XmlArray>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<XmlArray>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Invalid header value for XmlArray - value: {} is invalid {}",
                hdr_value, e
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<XmlArray> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <XmlArray as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        "Unable to convert header value '{}' into XmlArray - {}",
                        value, err
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Unable to convert header: {:?} to string: {}",
                hdr_value, e
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct XmlInner(String);

impl validator::Validate for XmlInner {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<String> for XmlInner {
    fn from(x: String) -> Self {
        XmlInner(x)
    }
}

impl std::fmt::Display for XmlInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl std::str::FromStr for XmlInner {
    type Err = std::string::ParseError;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(XmlInner(x.to_string()))
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

/// An XML object

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct XmlObject {
    #[serde(rename = "innerString")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inner_string: Option<String>,

    #[serde(rename = "other_inner_rename")]
    #[serde(skip_serializing_if = "Option::is_none")]
    pub other_inner_rename: Option<i32>,
}

impl XmlObject {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
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
impl std::fmt::Display for XmlObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            self.inner_string.as_ref().map(|inner_string| {
                ["innerString".to_string(), inner_string.to_string()].join(",")
            }),
            self.other_inner_rename.as_ref().map(|other_inner_rename| {
                [
                    "other_inner_rename".to_string(),
                    other_inner_rename.to_string(),
                ]
                .join(",")
            }),
        ];

        write!(
            f,
            "{}",
            params.into_iter().flatten().collect::<Vec<_>>().join(",")
        )
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a XmlObject value
/// as specified in https://swagger.io/docs/specification/serialization/
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
                None => {
                    return std::result::Result::Err(
                        "Missing value while parsing XmlObject".to_string(),
                    )
                }
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "innerString" => intermediate_rep.inner_string.push(
                        <String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    #[allow(clippy::redundant_clone)]
                    "other_inner_rename" => intermediate_rep.other_inner_rename.push(
                        <i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?,
                    ),
                    _ => {
                        return std::result::Result::Err(
                            "Unexpected key while parsing XmlObject".to_string(),
                        )
                    }
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

// Methods for converting between header::IntoHeaderValue<XmlObject> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<XmlObject>> for HeaderValue {
    type Error = String;

    fn try_from(
        hdr_value: header::IntoHeaderValue<XmlObject>,
    ) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
            std::result::Result::Ok(value) => std::result::Result::Ok(value),
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Invalid header value for XmlObject - value: {} is invalid {}",
                hdr_value, e
            )),
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<XmlObject> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
            std::result::Result::Ok(value) => {
                match <XmlObject as std::str::FromStr>::from_str(value) {
                    std::result::Result::Ok(value) => {
                        std::result::Result::Ok(header::IntoHeaderValue(value))
                    }
                    std::result::Result::Err(err) => std::result::Result::Err(format!(
                        "Unable to convert header value '{}' into XmlObject - {}",
                        value, err
                    )),
                }
            }
            std::result::Result::Err(e) => std::result::Result::Err(format!(
                "Unable to convert header: {:?} to string: {}",
                hdr_value, e
            )),
        }
    }
}
