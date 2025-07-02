#![allow(unused_qualifications)]
#![allow(clippy::to_string_trait_impl)]

use validator::Validate;

use crate::models;
#[cfg(any(feature = "client", feature = "server"))]
use crate::header;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AdditionalPropertiesClass {
    #[serde(rename = "map_property")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map_property: Option<std::collections::HashMap<String, String>>,

    #[serde(rename = "map_of_map_property")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map_of_map_property: Option<std::collections::HashMap<String, std::collections::HashMap<String, String>>>,

}


impl AdditionalPropertiesClass {
    #[allow(clippy::new_without_default)]
    pub fn new() -> AdditionalPropertiesClass {
        AdditionalPropertiesClass {
            map_property: None,
            map_of_map_property: None,
        }
    }
}

/// Converts the AdditionalPropertiesClass value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for AdditionalPropertiesClass {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            // Skipping map map_property in query parameter serialization
            // Skipping map map_of_map_property in query parameter serialization
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AdditionalPropertiesClass value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AdditionalPropertiesClass {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub map_property: Vec<std::collections::HashMap<String, String>>,
            pub map_of_map_property: Vec<std::collections::HashMap<String, std::collections::HashMap<String, String>>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing AdditionalPropertiesClass".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "map_property" => return std::result::Result::Err("Parsing a container in this style is not supported in AdditionalPropertiesClass".to_string()),
                    "map_of_map_property" => return std::result::Result::Err("Parsing a container in this style is not supported in AdditionalPropertiesClass".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing AdditionalPropertiesClass".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(AdditionalPropertiesClass {
            map_property: intermediate_rep.map_property.into_iter().next(),
            map_of_map_property: intermediate_rep.map_of_map_property.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<AdditionalPropertiesClass> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<AdditionalPropertiesClass>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AdditionalPropertiesClass>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AdditionalPropertiesClass - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<AdditionalPropertiesClass> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AdditionalPropertiesClass as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into AdditionalPropertiesClass - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<AdditionalPropertiesClass>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<AdditionalPropertiesClass>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<AdditionalPropertiesClass>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<AdditionalPropertiesClass> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <AdditionalPropertiesClass as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into AdditionalPropertiesClass - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl AdditionalPropertiesClass {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Animal {
    #[serde(rename = "className")]
    pub class_name: String,

    #[serde(rename = "color")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub color: Option<String>,

}


impl Animal {
    #[allow(clippy::new_without_default)]
    pub fn new(class_name: String, ) -> Animal {
        Animal {
            class_name,
            color: Some("red".to_string()),
        }
    }
}

/// Converts the Animal value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for Animal {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            Some("className".to_string()),
            Some(self.class_name.to_string()),
            self.color.as_ref().map(|color| {
                [
                    "color".to_string(),
                    color.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Animal value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Animal {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub class_name: Vec<String>,
            pub color: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Animal".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "className" => intermediate_rep.class_name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "color" => intermediate_rep.color.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Animal".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Animal {
            class_name: intermediate_rep.class_name.into_iter().next().ok_or_else(|| "className missing in Animal".to_string())?,
            color: intermediate_rep.color.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Animal> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Animal>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Animal>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Animal - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Animal> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Animal as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Animal - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Animal>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Animal>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Animal>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Animal> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Animal as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Animal - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Animal {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AnimalFarm(
    Vec<Animal>
);

impl std::convert::From<Vec<Animal>> for AnimalFarm {
    fn from(x: Vec<Animal>) -> Self {
        AnimalFarm(x)
    }
}

impl std::convert::From<AnimalFarm> for Vec<Animal> {
    fn from(x: AnimalFarm) -> Self {
        x.0
    }
}

impl std::iter::FromIterator<Animal> for AnimalFarm {
    fn from_iter<U: IntoIterator<Item=Animal>>(u: U) -> Self {
        AnimalFarm(Vec::<Animal>::from_iter(u))
    }
}

impl std::iter::IntoIterator for AnimalFarm {
    type Item = Animal;
    type IntoIter = std::vec::IntoIter<Animal>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> std::iter::IntoIterator for &'a AnimalFarm {
    type Item = &'a Animal;
    type IntoIter = std::slice::Iter<'a, Animal>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a> std::iter::IntoIterator for &'a mut AnimalFarm {
    type Item = &'a mut Animal;
    type IntoIter = std::slice::IterMut<'a, Animal>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl std::ops::Deref for AnimalFarm {
    type Target = Vec<Animal>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for AnimalFarm {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Converts the AnimalFarm value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for AnimalFarm {
    fn to_string(&self) -> String {
        self.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnimalFarm value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AnimalFarm {
    type Err = <Animal as std::str::FromStr>::Err;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut items = vec![];
        for item in s.split(',')
        {
            items.push(item.parse()?);
        }
        std::result::Result::Ok(AnimalFarm(items))
    }
}


// Methods for converting between header::IntoHeaderValue<AnimalFarm> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<AnimalFarm>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AnimalFarm>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AnimalFarm - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<AnimalFarm> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AnimalFarm as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into AnimalFarm - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<AnimalFarm>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<AnimalFarm>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<AnimalFarm>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<AnimalFarm> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <AnimalFarm as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into AnimalFarm - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl AnimalFarm {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ApiResponse {
    #[serde(rename = "code")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub code: Option<i32>,

    #[serde(rename = "type")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub r#type: Option<String>,

    #[serde(rename = "message")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub message: Option<String>,

}


impl ApiResponse {
    #[allow(clippy::new_without_default)]
    pub fn new() -> ApiResponse {
        ApiResponse {
            code: None,
            r#type: None,
            message: None,
        }
    }
}

/// Converts the ApiResponse value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for ApiResponse {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.code.as_ref().map(|code| {
                [
                    "code".to_string(),
                    code.to_string(),
                ].join(",")
            }),
            self.r#type.as_ref().map(|r#type| {
                [
                    "type".to_string(),
                    r#type.to_string(),
                ].join(",")
            }),
            self.message.as_ref().map(|message| {
                [
                    "message".to_string(),
                    message.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ApiResponse value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ApiResponse {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub code: Vec<i32>,
            pub r#type: Vec<String>,
            pub message: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ApiResponse".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "code" => intermediate_rep.code.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "type" => intermediate_rep.r#type.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "message" => intermediate_rep.message.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ApiResponse".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ApiResponse {
            code: intermediate_rep.code.into_iter().next(),
            r#type: intermediate_rep.r#type.into_iter().next(),
            message: intermediate_rep.message.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ApiResponse> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<ApiResponse>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ApiResponse>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ApiResponse - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<ApiResponse> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ApiResponse as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into ApiResponse - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<ApiResponse>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<ApiResponse>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<ApiResponse>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<ApiResponse> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <ApiResponse as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into ApiResponse - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl ApiResponse {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ArrayOfArrayOfNumberOnly {
    #[serde(rename = "ArrayArrayNumber")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_array_number: Option<Vec<Vec<f64>>>,

}


impl ArrayOfArrayOfNumberOnly {
    #[allow(clippy::new_without_default)]
    pub fn new() -> ArrayOfArrayOfNumberOnly {
        ArrayOfArrayOfNumberOnly {
            array_array_number: None,
        }
    }
}

/// Converts the ArrayOfArrayOfNumberOnly value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for ArrayOfArrayOfNumberOnly {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            // Skipping non-primitive type ArrayArrayNumber in query parameter serialization
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ArrayOfArrayOfNumberOnly value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ArrayOfArrayOfNumberOnly {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub array_array_number: Vec<Vec<Vec<f64>>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ArrayOfArrayOfNumberOnly".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "ArrayArrayNumber" => return std::result::Result::Err("Parsing a container in this style is not supported in ArrayOfArrayOfNumberOnly".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing ArrayOfArrayOfNumberOnly".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ArrayOfArrayOfNumberOnly {
            array_array_number: intermediate_rep.array_array_number.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ArrayOfArrayOfNumberOnly> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<ArrayOfArrayOfNumberOnly>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ArrayOfArrayOfNumberOnly>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ArrayOfArrayOfNumberOnly - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<ArrayOfArrayOfNumberOnly> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ArrayOfArrayOfNumberOnly as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into ArrayOfArrayOfNumberOnly - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<ArrayOfArrayOfNumberOnly>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<ArrayOfArrayOfNumberOnly>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<ArrayOfArrayOfNumberOnly>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<ArrayOfArrayOfNumberOnly> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <ArrayOfArrayOfNumberOnly as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into ArrayOfArrayOfNumberOnly - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl ArrayOfArrayOfNumberOnly {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ArrayOfNumberOnly {
    #[serde(rename = "ArrayNumber")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_number: Option<Vec<f64>>,

}


impl ArrayOfNumberOnly {
    #[allow(clippy::new_without_default)]
    pub fn new() -> ArrayOfNumberOnly {
        ArrayOfNumberOnly {
            array_number: None,
        }
    }
}

/// Converts the ArrayOfNumberOnly value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for ArrayOfNumberOnly {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.array_number.as_ref().map(|array_number| {
                [
                    "ArrayNumber".to_string(),
                    array_number.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ArrayOfNumberOnly value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ArrayOfNumberOnly {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub array_number: Vec<Vec<f64>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ArrayOfNumberOnly".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "ArrayNumber" => return std::result::Result::Err("Parsing a container in this style is not supported in ArrayOfNumberOnly".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing ArrayOfNumberOnly".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ArrayOfNumberOnly {
            array_number: intermediate_rep.array_number.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ArrayOfNumberOnly> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<ArrayOfNumberOnly>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ArrayOfNumberOnly>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ArrayOfNumberOnly - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<ArrayOfNumberOnly> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ArrayOfNumberOnly as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into ArrayOfNumberOnly - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<ArrayOfNumberOnly>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<ArrayOfNumberOnly>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<ArrayOfNumberOnly>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<ArrayOfNumberOnly> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <ArrayOfNumberOnly as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into ArrayOfNumberOnly - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl ArrayOfNumberOnly {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ArrayTest {
    #[serde(rename = "array_of_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_of_string: Option<Vec<String>>,

    #[serde(rename = "array_array_of_integer")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_array_of_integer: Option<Vec<Vec<i64>>>,

    #[serde(rename = "array_array_of_model")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_array_of_model: Option<Vec<Vec<models::ReadOnlyFirst>>>,

    #[serde(rename = "array_of_enum")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_of_enum: Option<Vec<models::MapTestMapMapOfEnumValueValue>>,

}


impl ArrayTest {
    #[allow(clippy::new_without_default)]
    pub fn new() -> ArrayTest {
        ArrayTest {
            array_of_string: None,
            array_array_of_integer: None,
            array_array_of_model: None,
            array_of_enum: None,
        }
    }
}

/// Converts the ArrayTest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for ArrayTest {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.array_of_string.as_ref().map(|array_of_string| {
                [
                    "array_of_string".to_string(),
                    array_of_string.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","),
                ].join(",")
            }),
            // Skipping non-primitive type array_array_of_integer in query parameter serialization
            // Skipping non-primitive type array_array_of_model in query parameter serialization
            // Skipping non-primitive type array_of_enum in query parameter serialization
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ArrayTest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ArrayTest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub array_of_string: Vec<Vec<String>>,
            pub array_array_of_integer: Vec<Vec<Vec<i64>>>,
            pub array_array_of_model: Vec<Vec<Vec<models::ReadOnlyFirst>>>,
            pub array_of_enum: Vec<Vec<models::MapTestMapMapOfEnumValueValue>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ArrayTest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "array_of_string" => return std::result::Result::Err("Parsing a container in this style is not supported in ArrayTest".to_string()),
                    "array_array_of_integer" => return std::result::Result::Err("Parsing a container in this style is not supported in ArrayTest".to_string()),
                    "array_array_of_model" => return std::result::Result::Err("Parsing a container in this style is not supported in ArrayTest".to_string()),
                    "array_of_enum" => return std::result::Result::Err("Parsing a container in this style is not supported in ArrayTest".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing ArrayTest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ArrayTest {
            array_of_string: intermediate_rep.array_of_string.into_iter().next(),
            array_array_of_integer: intermediate_rep.array_array_of_integer.into_iter().next(),
            array_array_of_model: intermediate_rep.array_array_of_model.into_iter().next(),
            array_of_enum: intermediate_rep.array_of_enum.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ArrayTest> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<ArrayTest>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ArrayTest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ArrayTest - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<ArrayTest> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ArrayTest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into ArrayTest - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<ArrayTest>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<ArrayTest>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<ArrayTest>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<ArrayTest> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <ArrayTest as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into ArrayTest - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl ArrayTest {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Capitalization {
    #[serde(rename = "smallCamel")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub small_camel: Option<String>,

    #[serde(rename = "CapitalCamel")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub capital_camel: Option<String>,

    #[serde(rename = "small_Snake")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub small_snake: Option<String>,

    #[serde(rename = "Capital_Snake")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub capital_snake: Option<String>,

    #[serde(rename = "SCA_ETH_Flow_Points")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub sca_eth_flow_points: Option<String>,

    /// Name of the pet 
    #[serde(rename = "ATT_NAME")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub att_name: Option<String>,

}


impl Capitalization {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Capitalization {
        Capitalization {
            small_camel: None,
            capital_camel: None,
            small_snake: None,
            capital_snake: None,
            sca_eth_flow_points: None,
            att_name: None,
        }
    }
}

/// Converts the Capitalization value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for Capitalization {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.small_camel.as_ref().map(|small_camel| {
                [
                    "smallCamel".to_string(),
                    small_camel.to_string(),
                ].join(",")
            }),
            self.capital_camel.as_ref().map(|capital_camel| {
                [
                    "CapitalCamel".to_string(),
                    capital_camel.to_string(),
                ].join(",")
            }),
            self.small_snake.as_ref().map(|small_snake| {
                [
                    "small_Snake".to_string(),
                    small_snake.to_string(),
                ].join(",")
            }),
            self.capital_snake.as_ref().map(|capital_snake| {
                [
                    "Capital_Snake".to_string(),
                    capital_snake.to_string(),
                ].join(",")
            }),
            self.sca_eth_flow_points.as_ref().map(|sca_eth_flow_points| {
                [
                    "SCA_ETH_Flow_Points".to_string(),
                    sca_eth_flow_points.to_string(),
                ].join(",")
            }),
            self.att_name.as_ref().map(|att_name| {
                [
                    "ATT_NAME".to_string(),
                    att_name.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Capitalization value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Capitalization {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub small_camel: Vec<String>,
            pub capital_camel: Vec<String>,
            pub small_snake: Vec<String>,
            pub capital_snake: Vec<String>,
            pub sca_eth_flow_points: Vec<String>,
            pub att_name: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Capitalization".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "smallCamel" => intermediate_rep.small_camel.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "CapitalCamel" => intermediate_rep.capital_camel.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "small_Snake" => intermediate_rep.small_snake.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "Capital_Snake" => intermediate_rep.capital_snake.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "SCA_ETH_Flow_Points" => intermediate_rep.sca_eth_flow_points.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "ATT_NAME" => intermediate_rep.att_name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Capitalization".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Capitalization {
            small_camel: intermediate_rep.small_camel.into_iter().next(),
            capital_camel: intermediate_rep.capital_camel.into_iter().next(),
            small_snake: intermediate_rep.small_snake.into_iter().next(),
            capital_snake: intermediate_rep.capital_snake.into_iter().next(),
            sca_eth_flow_points: intermediate_rep.sca_eth_flow_points.into_iter().next(),
            att_name: intermediate_rep.att_name.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Capitalization> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Capitalization>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Capitalization>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Capitalization - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Capitalization> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Capitalization as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Capitalization - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Capitalization>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Capitalization>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Capitalization>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Capitalization> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Capitalization as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Capitalization - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Capitalization {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Cat {
    #[serde(rename = "className")]
    pub class_name: String,

    #[serde(rename = "color")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub color: Option<String>,

    #[serde(rename = "declawed")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub declawed: Option<bool>,

}


impl Cat {
    #[allow(clippy::new_without_default)]
    pub fn new(class_name: String, ) -> Cat {
        Cat {
            class_name,
            color: Some("red".to_string()),
            declawed: None,
        }
    }
}

/// Converts the Cat value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for Cat {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            Some("className".to_string()),
            Some(self.class_name.to_string()),
            self.color.as_ref().map(|color| {
                [
                    "color".to_string(),
                    color.to_string(),
                ].join(",")
            }),
            self.declawed.as_ref().map(|declawed| {
                [
                    "declawed".to_string(),
                    declawed.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Cat value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Cat {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub class_name: Vec<String>,
            pub color: Vec<String>,
            pub declawed: Vec<bool>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Cat".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "className" => intermediate_rep.class_name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "color" => intermediate_rep.color.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "declawed" => intermediate_rep.declawed.push(<bool as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Cat".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Cat {
            class_name: intermediate_rep.class_name.into_iter().next().ok_or_else(|| "className missing in Cat".to_string())?,
            color: intermediate_rep.color.into_iter().next(),
            declawed: intermediate_rep.declawed.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Cat> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Cat>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Cat>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Cat - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Cat> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Cat as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Cat - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Cat>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Cat>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Cat>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Cat> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Cat as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Cat - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Cat {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
#[serde(rename = "Category")]
pub struct Category {
    #[serde(rename = "id")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub id: Option<i64>,

    #[serde(rename = "name")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub name: Option<String>,

}


impl Category {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Category {
        Category {
            id: None,
            name: None,
        }
    }
}

/// Converts the Category value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for Category {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.id.as_ref().map(|id| {
                [
                    "id".to_string(),
                    id.to_string(),
                ].join(",")
            }),
            self.name.as_ref().map(|name| {
                [
                    "name".to_string(),
                    name.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Category value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Category {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub id: Vec<i64>,
            pub name: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Category".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "id" => intermediate_rep.id.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "name" => intermediate_rep.name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Category".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Category {
            id: intermediate_rep.id.into_iter().next(),
            name: intermediate_rep.name.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Category> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Category>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Category>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Category - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Category> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Category as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Category - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Category>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Category>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Category>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Category> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Category as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Category - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Category {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Model for testing model with \"_class\" property
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ClassModel {
    #[serde(rename = "_class")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub _class: Option<String>,

}


impl ClassModel {
    #[allow(clippy::new_without_default)]
    pub fn new() -> ClassModel {
        ClassModel {
            _class: None,
        }
    }
}

/// Converts the ClassModel value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for ClassModel {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self._class.as_ref().map(|_class| {
                [
                    "_class".to_string(),
                    _class.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ClassModel value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ClassModel {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub _class: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ClassModel".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "_class" => intermediate_rep._class.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ClassModel".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ClassModel {
            _class: intermediate_rep._class.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ClassModel> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<ClassModel>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ClassModel>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ClassModel - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<ClassModel> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ClassModel as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into ClassModel - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<ClassModel>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<ClassModel>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<ClassModel>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<ClassModel> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <ClassModel as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into ClassModel - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl ClassModel {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Client {
    #[serde(rename = "client")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub client: Option<String>,

}


impl Client {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Client {
        Client {
            client: None,
        }
    }
}

/// Converts the Client value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for Client {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.client.as_ref().map(|client| {
                [
                    "client".to_string(),
                    client.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Client value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Client {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub client: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Client".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "client" => intermediate_rep.client.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Client".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Client {
            client: intermediate_rep.client.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Client> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Client>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Client>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Client - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Client> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Client as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Client - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Client>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Client>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Client>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Client> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Client as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Client - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Client {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Dog {
    #[serde(rename = "className")]
    pub class_name: String,

    #[serde(rename = "color")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub color: Option<String>,

    #[serde(rename = "breed")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub breed: Option<String>,

}


impl Dog {
    #[allow(clippy::new_without_default)]
    pub fn new(class_name: String, ) -> Dog {
        Dog {
            class_name,
            color: Some("red".to_string()),
            breed: None,
        }
    }
}

/// Converts the Dog value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for Dog {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            Some("className".to_string()),
            Some(self.class_name.to_string()),
            self.color.as_ref().map(|color| {
                [
                    "color".to_string(),
                    color.to_string(),
                ].join(",")
            }),
            self.breed.as_ref().map(|breed| {
                [
                    "breed".to_string(),
                    breed.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Dog value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Dog {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub class_name: Vec<String>,
            pub color: Vec<String>,
            pub breed: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Dog".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "className" => intermediate_rep.class_name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "color" => intermediate_rep.color.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "breed" => intermediate_rep.breed.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Dog".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Dog {
            class_name: intermediate_rep.class_name.into_iter().next().ok_or_else(|| "className missing in Dog".to_string())?,
            color: intermediate_rep.color.into_iter().next(),
            breed: intermediate_rep.breed.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Dog> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Dog>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Dog>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Dog - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Dog> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Dog as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Dog - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Dog>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Dog>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Dog>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Dog> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Dog as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Dog - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Dog {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
#[serde(rename = "$special[model.name]")]
pub struct DollarSpecialLeftSquareBracketModelNameRightSquareBracket {
    #[serde(rename = "$special[property.name]")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub dollar_special_left_square_bracket_property_name_right_square_bracket: Option<i64>,

}


impl DollarSpecialLeftSquareBracketModelNameRightSquareBracket {
    #[allow(clippy::new_without_default)]
    pub fn new() -> DollarSpecialLeftSquareBracketModelNameRightSquareBracket {
        DollarSpecialLeftSquareBracketModelNameRightSquareBracket {
            dollar_special_left_square_bracket_property_name_right_square_bracket: None,
        }
    }
}

/// Converts the DollarSpecialLeftSquareBracketModelNameRightSquareBracket value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for DollarSpecialLeftSquareBracketModelNameRightSquareBracket {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.dollar_special_left_square_bracket_property_name_right_square_bracket.as_ref().map(|dollar_special_left_square_bracket_property_name_right_square_bracket| {
                [
                    "$special[property.name]".to_string(),
                    dollar_special_left_square_bracket_property_name_right_square_bracket.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a DollarSpecialLeftSquareBracketModelNameRightSquareBracket value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for DollarSpecialLeftSquareBracketModelNameRightSquareBracket {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub dollar_special_left_square_bracket_property_name_right_square_bracket: Vec<i64>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing DollarSpecialLeftSquareBracketModelNameRightSquareBracket".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "$special[property.name]" => intermediate_rep.dollar_special_left_square_bracket_property_name_right_square_bracket.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing DollarSpecialLeftSquareBracketModelNameRightSquareBracket".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(DollarSpecialLeftSquareBracketModelNameRightSquareBracket {
            dollar_special_left_square_bracket_property_name_right_square_bracket: intermediate_rep.dollar_special_left_square_bracket_property_name_right_square_bracket.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<DollarSpecialLeftSquareBracketModelNameRightSquareBracket> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<DollarSpecialLeftSquareBracketModelNameRightSquareBracket>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<DollarSpecialLeftSquareBracketModelNameRightSquareBracket>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for DollarSpecialLeftSquareBracketModelNameRightSquareBracket - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<DollarSpecialLeftSquareBracketModelNameRightSquareBracket> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <DollarSpecialLeftSquareBracketModelNameRightSquareBracket as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into DollarSpecialLeftSquareBracketModelNameRightSquareBracket - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<DollarSpecialLeftSquareBracketModelNameRightSquareBracket>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<DollarSpecialLeftSquareBracketModelNameRightSquareBracket>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<DollarSpecialLeftSquareBracketModelNameRightSquareBracket>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<DollarSpecialLeftSquareBracketModelNameRightSquareBracket> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <DollarSpecialLeftSquareBracketModelNameRightSquareBracket as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into DollarSpecialLeftSquareBracketModelNameRightSquareBracket - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl DollarSpecialLeftSquareBracketModelNameRightSquareBracket {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct EnumArrays {
    #[serde(rename = "just_symbol")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub just_symbol: Option<models::EnumArraysJustSymbol>,

    #[serde(rename = "array_enum")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_enum: Option<Vec<models::EnumArraysArrayEnumInner>>,

    #[serde(rename = "array_array_enum")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_array_enum: Option<Vec<Vec<models::EnumArraysArrayArrayEnumInnerInner>>>,

}


impl EnumArrays {
    #[allow(clippy::new_without_default)]
    pub fn new() -> EnumArrays {
        EnumArrays {
            just_symbol: None,
            array_enum: None,
            array_array_enum: None,
        }
    }
}

/// Converts the EnumArrays value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for EnumArrays {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            // Skipping non-primitive type just_symbol in query parameter serialization
            // Skipping non-primitive type array_enum in query parameter serialization
            // Skipping non-primitive type array_array_enum in query parameter serialization
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a EnumArrays value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for EnumArrays {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub just_symbol: Vec<models::EnumArraysJustSymbol>,
            pub array_enum: Vec<Vec<models::EnumArraysArrayEnumInner>>,
            pub array_array_enum: Vec<Vec<Vec<models::EnumArraysArrayArrayEnumInnerInner>>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing EnumArrays".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "just_symbol" => intermediate_rep.just_symbol.push(<models::EnumArraysJustSymbol as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    "array_enum" => return std::result::Result::Err("Parsing a container in this style is not supported in EnumArrays".to_string()),
                    "array_array_enum" => return std::result::Result::Err("Parsing a container in this style is not supported in EnumArrays".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing EnumArrays".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(EnumArrays {
            just_symbol: intermediate_rep.just_symbol.into_iter().next(),
            array_enum: intermediate_rep.array_enum.into_iter().next(),
            array_array_enum: intermediate_rep.array_array_enum.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<EnumArrays> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<EnumArrays>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<EnumArrays>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for EnumArrays - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<EnumArrays> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <EnumArrays as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into EnumArrays - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<EnumArrays>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<EnumArrays>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<EnumArrays>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<EnumArrays> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <EnumArrays as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into EnumArrays - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl EnumArrays {
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
pub enum EnumArraysArrayArrayEnumInnerInner {
    #[serde(rename = "Cat")]
    Cat,
    #[serde(rename = "Dog")]
    Dog,
}

impl std::fmt::Display for EnumArraysArrayArrayEnumInnerInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            EnumArraysArrayArrayEnumInnerInner::Cat => write!(f, "Cat"),
            EnumArraysArrayArrayEnumInnerInner::Dog => write!(f, "Dog"),
        }
    }
}

impl std::str::FromStr for EnumArraysArrayArrayEnumInnerInner {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "Cat" => std::result::Result::Ok(EnumArraysArrayArrayEnumInnerInner::Cat),
            "Dog" => std::result::Result::Ok(EnumArraysArrayArrayEnumInnerInner::Dog),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<EnumArraysArrayArrayEnumInnerInner> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<EnumArraysArrayArrayEnumInnerInner>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<EnumArraysArrayArrayEnumInnerInner>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for EnumArraysArrayArrayEnumInnerInner - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<EnumArraysArrayArrayEnumInnerInner> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <EnumArraysArrayArrayEnumInnerInner as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into EnumArraysArrayArrayEnumInnerInner - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<EnumArraysArrayArrayEnumInnerInner>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<EnumArraysArrayArrayEnumInnerInner>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<EnumArraysArrayArrayEnumInnerInner>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<EnumArraysArrayArrayEnumInnerInner> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <EnumArraysArrayArrayEnumInnerInner as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into EnumArraysArrayArrayEnumInnerInner - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl EnumArraysArrayArrayEnumInnerInner {
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
pub enum EnumArraysArrayEnumInner {
    #[serde(rename = "fish")]
    Fish,
    #[serde(rename = "crab")]
    Crab,
}

impl std::fmt::Display for EnumArraysArrayEnumInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            EnumArraysArrayEnumInner::Fish => write!(f, "fish"),
            EnumArraysArrayEnumInner::Crab => write!(f, "crab"),
        }
    }
}

impl std::str::FromStr for EnumArraysArrayEnumInner {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "fish" => std::result::Result::Ok(EnumArraysArrayEnumInner::Fish),
            "crab" => std::result::Result::Ok(EnumArraysArrayEnumInner::Crab),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<EnumArraysArrayEnumInner> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<EnumArraysArrayEnumInner>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<EnumArraysArrayEnumInner>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for EnumArraysArrayEnumInner - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<EnumArraysArrayEnumInner> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <EnumArraysArrayEnumInner as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into EnumArraysArrayEnumInner - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<EnumArraysArrayEnumInner>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<EnumArraysArrayEnumInner>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<EnumArraysArrayEnumInner>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<EnumArraysArrayEnumInner> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <EnumArraysArrayEnumInner as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into EnumArraysArrayEnumInner - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl EnumArraysArrayEnumInner {
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
pub enum EnumArraysJustSymbol {
    #[serde(rename = ">=")]
    GreaterThanEqual,
    #[serde(rename = "$")]
    Dollar,
}

impl std::fmt::Display for EnumArraysJustSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            EnumArraysJustSymbol::GreaterThanEqual => write!(f, ">="),
            EnumArraysJustSymbol::Dollar => write!(f, "$"),
        }
    }
}

impl std::str::FromStr for EnumArraysJustSymbol {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            ">=" => std::result::Result::Ok(EnumArraysJustSymbol::GreaterThanEqual),
            "$" => std::result::Result::Ok(EnumArraysJustSymbol::Dollar),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<EnumArraysJustSymbol> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<EnumArraysJustSymbol>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<EnumArraysJustSymbol>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for EnumArraysJustSymbol - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<EnumArraysJustSymbol> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <EnumArraysJustSymbol as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into EnumArraysJustSymbol - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<EnumArraysJustSymbol>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<EnumArraysJustSymbol>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<EnumArraysJustSymbol>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<EnumArraysJustSymbol> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <EnumArraysJustSymbol as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into EnumArraysJustSymbol - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl EnumArraysJustSymbol {
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
pub enum EnumClass {
    #[serde(rename = "_abc")]
    Abc,
    #[serde(rename = "-efg")]
    Efg,
    #[serde(rename = "(xyz)")]
    LeftParenthesisXyzRightParenthesis,
}

impl std::fmt::Display for EnumClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            EnumClass::Abc => write!(f, "_abc"),
            EnumClass::Efg => write!(f, "-efg"),
            EnumClass::LeftParenthesisXyzRightParenthesis => write!(f, "(xyz)"),
        }
    }
}

impl std::str::FromStr for EnumClass {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "_abc" => std::result::Result::Ok(EnumClass::Abc),
            "-efg" => std::result::Result::Ok(EnumClass::Efg),
            "(xyz)" => std::result::Result::Ok(EnumClass::LeftParenthesisXyzRightParenthesis),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<EnumClass> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<EnumClass>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<EnumClass>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for EnumClass - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<EnumClass> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <EnumClass as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into EnumClass - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<EnumClass>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<EnumClass>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<EnumClass>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<EnumClass> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <EnumClass as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into EnumClass - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl EnumClass {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct EnumTest {
    #[serde(rename = "enum_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub enum_string: Option<models::EnumTestEnumString>,

    #[serde(rename = "enum_string_required")]
    pub enum_string_required: models::EnumTestEnumString,

    #[serde(rename = "enum_integer")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub enum_integer: Option<models::EnumTestEnumInteger>,

    #[serde(rename = "enum_number")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub enum_number: Option<models::TestEnumParametersEnumQueryDoubleParameter>,

    #[serde(rename = "outerEnum")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub outer_enum: Option<models::OuterEnum>,

}


impl EnumTest {
    #[allow(clippy::new_without_default)]
    pub fn new(enum_string_required: models::EnumTestEnumString, ) -> EnumTest {
        EnumTest {
            enum_string: None,
            enum_string_required,
            enum_integer: None,
            enum_number: None,
            outer_enum: None,
        }
    }
}

/// Converts the EnumTest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for EnumTest {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            // Skipping non-primitive type enum_string in query parameter serialization
            // Skipping non-primitive type enum_string_required in query parameter serialization
            // Skipping non-primitive type enum_integer in query parameter serialization
            // Skipping non-primitive type enum_number in query parameter serialization
            // Skipping non-primitive type outerEnum in query parameter serialization
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a EnumTest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for EnumTest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub enum_string: Vec<models::EnumTestEnumString>,
            pub enum_string_required: Vec<models::EnumTestEnumString>,
            pub enum_integer: Vec<models::EnumTestEnumInteger>,
            pub enum_number: Vec<models::TestEnumParametersEnumQueryDoubleParameter>,
            pub outer_enum: Vec<models::OuterEnum>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing EnumTest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "enum_string" => intermediate_rep.enum_string.push(<models::EnumTestEnumString as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "enum_string_required" => intermediate_rep.enum_string_required.push(<models::EnumTestEnumString as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "enum_integer" => intermediate_rep.enum_integer.push(<models::EnumTestEnumInteger as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "enum_number" => intermediate_rep.enum_number.push(<models::TestEnumParametersEnumQueryDoubleParameter as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "outerEnum" => intermediate_rep.outer_enum.push(<models::OuterEnum as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing EnumTest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(EnumTest {
            enum_string: intermediate_rep.enum_string.into_iter().next(),
            enum_string_required: intermediate_rep.enum_string_required.into_iter().next().ok_or_else(|| "enum_string_required missing in EnumTest".to_string())?,
            enum_integer: intermediate_rep.enum_integer.into_iter().next(),
            enum_number: intermediate_rep.enum_number.into_iter().next(),
            outer_enum: intermediate_rep.outer_enum.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<EnumTest> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<EnumTest>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<EnumTest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for EnumTest - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<EnumTest> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <EnumTest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into EnumTest - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<EnumTest>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<EnumTest>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<EnumTest>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<EnumTest> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <EnumTest as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into EnumTest - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl EnumTest {
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
pub enum EnumTestEnumInteger {
    #[serde(rename = "1")]
    Variant1,
    #[serde(rename = "-1")]
    Variant12,
}

impl std::fmt::Display for EnumTestEnumInteger {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            EnumTestEnumInteger::Variant1 => write!(f, "1"),
            EnumTestEnumInteger::Variant12 => write!(f, "-1"),
        }
    }
}

impl std::str::FromStr for EnumTestEnumInteger {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "1" => std::result::Result::Ok(EnumTestEnumInteger::Variant1),
            "-1" => std::result::Result::Ok(EnumTestEnumInteger::Variant12),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<EnumTestEnumInteger> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<EnumTestEnumInteger>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<EnumTestEnumInteger>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for EnumTestEnumInteger - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<EnumTestEnumInteger> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <EnumTestEnumInteger as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into EnumTestEnumInteger - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<EnumTestEnumInteger>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<EnumTestEnumInteger>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<EnumTestEnumInteger>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<EnumTestEnumInteger> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <EnumTestEnumInteger as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into EnumTestEnumInteger - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl EnumTestEnumInteger {
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
pub enum EnumTestEnumString {
    #[serde(rename = "UPPER")]
    Upper,
    #[serde(rename = "lower")]
    Lower,
    #[serde(rename = "")]
    Empty,
}

impl std::fmt::Display for EnumTestEnumString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            EnumTestEnumString::Upper => write!(f, "UPPER"),
            EnumTestEnumString::Lower => write!(f, "lower"),
            EnumTestEnumString::Empty => write!(f, ""),
        }
    }
}

impl std::str::FromStr for EnumTestEnumString {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "UPPER" => std::result::Result::Ok(EnumTestEnumString::Upper),
            "lower" => std::result::Result::Ok(EnumTestEnumString::Lower),
            "" => std::result::Result::Ok(EnumTestEnumString::Empty),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<EnumTestEnumString> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<EnumTestEnumString>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<EnumTestEnumString>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for EnumTestEnumString - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<EnumTestEnumString> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <EnumTestEnumString as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into EnumTestEnumString - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<EnumTestEnumString>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<EnumTestEnumString>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<EnumTestEnumString>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<EnumTestEnumString> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <EnumTestEnumString as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into EnumTestEnumString - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl EnumTestEnumString {
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
pub enum FindPetsByStatusStatusParameterInner {
    #[serde(rename = "available")]
    Available,
    #[serde(rename = "pending")]
    Pending,
    #[serde(rename = "sold")]
    Sold,
}

impl std::fmt::Display for FindPetsByStatusStatusParameterInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            FindPetsByStatusStatusParameterInner::Available => write!(f, "available"),
            FindPetsByStatusStatusParameterInner::Pending => write!(f, "pending"),
            FindPetsByStatusStatusParameterInner::Sold => write!(f, "sold"),
        }
    }
}

impl std::str::FromStr for FindPetsByStatusStatusParameterInner {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "available" => std::result::Result::Ok(FindPetsByStatusStatusParameterInner::Available),
            "pending" => std::result::Result::Ok(FindPetsByStatusStatusParameterInner::Pending),
            "sold" => std::result::Result::Ok(FindPetsByStatusStatusParameterInner::Sold),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<FindPetsByStatusStatusParameterInner> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<FindPetsByStatusStatusParameterInner>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<FindPetsByStatusStatusParameterInner>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for FindPetsByStatusStatusParameterInner - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<FindPetsByStatusStatusParameterInner> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <FindPetsByStatusStatusParameterInner as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into FindPetsByStatusStatusParameterInner - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<FindPetsByStatusStatusParameterInner>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<FindPetsByStatusStatusParameterInner>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<FindPetsByStatusStatusParameterInner>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<FindPetsByStatusStatusParameterInner> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <FindPetsByStatusStatusParameterInner as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into FindPetsByStatusStatusParameterInner - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl FindPetsByStatusStatusParameterInner {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct FormatTest {
    #[serde(rename = "integer")]
    #[validate(
            range(min = 10, max = 100),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub integer: Option<u8>,

    #[serde(rename = "int32")]
    #[validate(
            range(min = 20, max = 200),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub int32: Option<u32>,

    #[serde(rename = "int64")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub int64: Option<i64>,

    #[serde(rename = "number")]
    #[validate(
            range(min = 32.1, max = 543.2),
        )]
    pub number: f64,

    #[serde(rename = "float")]
    #[validate(
            range(min = 54.3, max = 987.6),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub float: Option<f32>,

    #[serde(rename = "double")]
    #[validate(
            range(min = 67.8, max = 123.4),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub double: Option<f64>,

    #[serde(rename = "string")]
    #[validate(
           regex = "RE_FORMATTEST_STRING",
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub string: Option<String>,

    #[serde(rename = "byte")]
    #[validate(
           custom ="validate_byte_formattest_byte"
        )]
    pub byte: swagger::ByteArray,

    #[serde(rename = "binary")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub binary: Option<swagger::ByteArray>,

    #[serde(rename = "date")]
    pub date: chrono::naive::NaiveDate,

    #[serde(rename = "dateTime")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub date_time: Option<chrono::DateTime::<chrono::Utc>>,

    #[serde(rename = "uuid")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub uuid: Option<uuid::Uuid>,

    #[serde(rename = "password")]
    #[validate(
            length(min = 10, max = 64),
        )]
    pub password: String,

}

lazy_static::lazy_static! {
    static ref RE_FORMATTEST_STRING: regex::Regex = regex::Regex::new(r"/[a-z]/i").unwrap();
}
lazy_static::lazy_static! {
    static ref RE_FORMATTEST_BYTE: regex::bytes::Regex = regex::bytes::Regex::new(r"^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}&#x3D;&#x3D;|[A-Za-z0-9+/]{3}&#x3D;)?$").unwrap();
}
fn validate_byte_formattest_byte(
    b: &swagger::ByteArray
) -> Result<(), validator::ValidationError> {
    if !RE_FORMATTEST_BYTE.is_match(b) {
        return Err(validator::ValidationError::new("Character not allowed"));
    }
    Ok(())
}

impl FormatTest {
    #[allow(clippy::new_without_default)]
    pub fn new(number: f64, byte: swagger::ByteArray, date: chrono::naive::NaiveDate, password: String, ) -> FormatTest {
        FormatTest {
            integer: None,
            int32: None,
            int64: None,
            number,
            float: None,
            double: None,
            string: None,
            byte,
            binary: None,
            date,
            date_time: None,
            uuid: None,
            password,
        }
    }
}

/// Converts the FormatTest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for FormatTest {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.integer.as_ref().map(|integer| {
                [
                    "integer".to_string(),
                    integer.to_string(),
                ].join(",")
            }),
            self.int32.as_ref().map(|int32| {
                [
                    "int32".to_string(),
                    int32.to_string(),
                ].join(",")
            }),
            self.int64.as_ref().map(|int64| {
                [
                    "int64".to_string(),
                    int64.to_string(),
                ].join(",")
            }),
            Some("number".to_string()),
            Some(self.number.to_string()),
            self.float.as_ref().map(|float| {
                [
                    "float".to_string(),
                    float.to_string(),
                ].join(",")
            }),
            self.double.as_ref().map(|double| {
                [
                    "double".to_string(),
                    double.to_string(),
                ].join(",")
            }),
            self.string.as_ref().map(|string| {
                [
                    "string".to_string(),
                    string.to_string(),
                ].join(",")
            }),
            // Skipping byte array byte in query parameter serialization
            // Skipping binary data binary in query parameter serialization
            // Skipping non-primitive type date in query parameter serialization
            // Skipping non-primitive type dateTime in query parameter serialization
            // Skipping non-primitive type uuid in query parameter serialization
            Some("password".to_string()),
            Some(self.password.to_string()),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a FormatTest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for FormatTest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub integer: Vec<u8>,
            pub int32: Vec<u32>,
            pub int64: Vec<i64>,
            pub number: Vec<f64>,
            pub float: Vec<f32>,
            pub double: Vec<f64>,
            pub string: Vec<String>,
            pub byte: Vec<swagger::ByteArray>,
            pub binary: Vec<swagger::ByteArray>,
            pub date: Vec<chrono::naive::NaiveDate>,
            pub date_time: Vec<chrono::DateTime::<chrono::Utc>>,
            pub uuid: Vec<uuid::Uuid>,
            pub password: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing FormatTest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "integer" => intermediate_rep.integer.push(<u8 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "int32" => intermediate_rep.int32.push(<u32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "int64" => intermediate_rep.int64.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "number" => intermediate_rep.number.push(<f64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "float" => intermediate_rep.float.push(<f32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "double" => intermediate_rep.double.push(<f64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "string" => intermediate_rep.string.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    "byte" => return std::result::Result::Err("Parsing binary data in this style is not supported in FormatTest".to_string()),
                    "binary" => return std::result::Result::Err("Parsing binary data in this style is not supported in FormatTest".to_string()),
                    #[allow(clippy::redundant_clone)]
                    "date" => intermediate_rep.date.push(<chrono::naive::NaiveDate as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "dateTime" => intermediate_rep.date_time.push(<chrono::DateTime::<chrono::Utc> as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "uuid" => intermediate_rep.uuid.push(<uuid::Uuid as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "password" => intermediate_rep.password.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing FormatTest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(FormatTest {
            integer: intermediate_rep.integer.into_iter().next(),
            int32: intermediate_rep.int32.into_iter().next(),
            int64: intermediate_rep.int64.into_iter().next(),
            number: intermediate_rep.number.into_iter().next().ok_or_else(|| "number missing in FormatTest".to_string())?,
            float: intermediate_rep.float.into_iter().next(),
            double: intermediate_rep.double.into_iter().next(),
            string: intermediate_rep.string.into_iter().next(),
            byte: intermediate_rep.byte.into_iter().next().ok_or_else(|| "byte missing in FormatTest".to_string())?,
            binary: intermediate_rep.binary.into_iter().next(),
            date: intermediate_rep.date.into_iter().next().ok_or_else(|| "date missing in FormatTest".to_string())?,
            date_time: intermediate_rep.date_time.into_iter().next(),
            uuid: intermediate_rep.uuid.into_iter().next(),
            password: intermediate_rep.password.into_iter().next().ok_or_else(|| "password missing in FormatTest".to_string())?,
        })
    }
}

// Methods for converting between header::IntoHeaderValue<FormatTest> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<FormatTest>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<FormatTest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for FormatTest - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<FormatTest> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <FormatTest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into FormatTest - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<FormatTest>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<FormatTest>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<FormatTest>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<FormatTest> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <FormatTest as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into FormatTest - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl FormatTest {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct HasOnlyReadOnly {
    #[serde(rename = "bar")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub bar: Option<String>,

    #[serde(rename = "foo")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub foo: Option<String>,

}


impl HasOnlyReadOnly {
    #[allow(clippy::new_without_default)]
    pub fn new() -> HasOnlyReadOnly {
        HasOnlyReadOnly {
            bar: None,
            foo: None,
        }
    }
}

/// Converts the HasOnlyReadOnly value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for HasOnlyReadOnly {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.bar.as_ref().map(|bar| {
                [
                    "bar".to_string(),
                    bar.to_string(),
                ].join(",")
            }),
            self.foo.as_ref().map(|foo| {
                [
                    "foo".to_string(),
                    foo.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a HasOnlyReadOnly value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for HasOnlyReadOnly {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub bar: Vec<String>,
            pub foo: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing HasOnlyReadOnly".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "bar" => intermediate_rep.bar.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "foo" => intermediate_rep.foo.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing HasOnlyReadOnly".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(HasOnlyReadOnly {
            bar: intermediate_rep.bar.into_iter().next(),
            foo: intermediate_rep.foo.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<HasOnlyReadOnly> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<HasOnlyReadOnly>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<HasOnlyReadOnly>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for HasOnlyReadOnly - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<HasOnlyReadOnly> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <HasOnlyReadOnly as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into HasOnlyReadOnly - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<HasOnlyReadOnly>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<HasOnlyReadOnly>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<HasOnlyReadOnly>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<HasOnlyReadOnly> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <HasOnlyReadOnly as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into HasOnlyReadOnly - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl HasOnlyReadOnly {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct List {
    #[serde(rename = "123-list")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub param_123_list: Option<String>,

}


impl List {
    #[allow(clippy::new_without_default)]
    pub fn new() -> List {
        List {
            param_123_list: None,
        }
    }
}

/// Converts the List value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for List {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.param_123_list.as_ref().map(|param_123_list| {
                [
                    "123-list".to_string(),
                    param_123_list.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a List value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for List {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub param_123_list: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing List".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "123-list" => intermediate_rep.param_123_list.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing List".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(List {
            param_123_list: intermediate_rep.param_123_list.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<List> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<List>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<List>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for List - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<List> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <List as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into List - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<List>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<List>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<List>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<List> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <List as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into List - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl List {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct MapTest {
    #[serde(rename = "map_map_of_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map_map_of_string: Option<std::collections::HashMap<String, std::collections::HashMap<String, String>>>,

    #[serde(rename = "map_map_of_enum")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map_map_of_enum: Option<std::collections::HashMap<String, std::collections::HashMap<String, models::MapTestMapMapOfEnumValueValue>>>,

    #[serde(rename = "map_of_enum_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map_of_enum_string: Option<std::collections::HashMap<String, models::MapTestMapMapOfEnumValueValue>>,

}


impl MapTest {
    #[allow(clippy::new_without_default)]
    pub fn new() -> MapTest {
        MapTest {
            map_map_of_string: None,
            map_map_of_enum: None,
            map_of_enum_string: None,
        }
    }
}

/// Converts the MapTest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for MapTest {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            // Skipping map map_map_of_string in query parameter serialization
            // Skipping map map_map_of_enum in query parameter serialization
            // Skipping map map_of_enum_string in query parameter serialization
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MapTest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for MapTest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub map_map_of_string: Vec<std::collections::HashMap<String, std::collections::HashMap<String, String>>>,
            pub map_map_of_enum: Vec<std::collections::HashMap<String, std::collections::HashMap<String, models::MapTestMapMapOfEnumValueValue>>>,
            pub map_of_enum_string: Vec<std::collections::HashMap<String, models::MapTestMapMapOfEnumValueValue>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing MapTest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "map_map_of_string" => return std::result::Result::Err("Parsing a container in this style is not supported in MapTest".to_string()),
                    "map_map_of_enum" => return std::result::Result::Err("Parsing a container in this style is not supported in MapTest".to_string()),
                    "map_of_enum_string" => return std::result::Result::Err("Parsing a container in this style is not supported in MapTest".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing MapTest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(MapTest {
            map_map_of_string: intermediate_rep.map_map_of_string.into_iter().next(),
            map_map_of_enum: intermediate_rep.map_map_of_enum.into_iter().next(),
            map_of_enum_string: intermediate_rep.map_of_enum_string.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<MapTest> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<MapTest>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<MapTest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for MapTest - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<MapTest> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <MapTest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into MapTest - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<MapTest>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<MapTest>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<MapTest>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<MapTest> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <MapTest as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into MapTest - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl MapTest {
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
pub enum MapTestMapMapOfEnumValueValue {
    #[serde(rename = "UPPER")]
    Upper,
    #[serde(rename = "lower")]
    Lower,
}

impl std::fmt::Display for MapTestMapMapOfEnumValueValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            MapTestMapMapOfEnumValueValue::Upper => write!(f, "UPPER"),
            MapTestMapMapOfEnumValueValue::Lower => write!(f, "lower"),
        }
    }
}

impl std::str::FromStr for MapTestMapMapOfEnumValueValue {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "UPPER" => std::result::Result::Ok(MapTestMapMapOfEnumValueValue::Upper),
            "lower" => std::result::Result::Ok(MapTestMapMapOfEnumValueValue::Lower),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<MapTestMapMapOfEnumValueValue> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<MapTestMapMapOfEnumValueValue>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<MapTestMapMapOfEnumValueValue>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for MapTestMapMapOfEnumValueValue - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<MapTestMapMapOfEnumValueValue> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <MapTestMapMapOfEnumValueValue as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into MapTestMapMapOfEnumValueValue - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<MapTestMapMapOfEnumValueValue>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<MapTestMapMapOfEnumValueValue>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<MapTestMapMapOfEnumValueValue>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<MapTestMapMapOfEnumValueValue> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <MapTestMapMapOfEnumValueValue as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into MapTestMapMapOfEnumValueValue - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl MapTestMapMapOfEnumValueValue {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct MixedPropertiesAndAdditionalPropertiesClass {
    #[serde(rename = "uuid")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub uuid: Option<uuid::Uuid>,

    #[serde(rename = "dateTime")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub date_time: Option<chrono::DateTime::<chrono::Utc>>,

    #[serde(rename = "map")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map: Option<std::collections::HashMap<String, models::Animal>>,

}


impl MixedPropertiesAndAdditionalPropertiesClass {
    #[allow(clippy::new_without_default)]
    pub fn new() -> MixedPropertiesAndAdditionalPropertiesClass {
        MixedPropertiesAndAdditionalPropertiesClass {
            uuid: None,
            date_time: None,
            map: None,
        }
    }
}

/// Converts the MixedPropertiesAndAdditionalPropertiesClass value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for MixedPropertiesAndAdditionalPropertiesClass {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            // Skipping non-primitive type uuid in query parameter serialization
            // Skipping non-primitive type dateTime in query parameter serialization
            // Skipping map map in query parameter serialization
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MixedPropertiesAndAdditionalPropertiesClass value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for MixedPropertiesAndAdditionalPropertiesClass {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub uuid: Vec<uuid::Uuid>,
            pub date_time: Vec<chrono::DateTime::<chrono::Utc>>,
            pub map: Vec<std::collections::HashMap<String, models::Animal>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing MixedPropertiesAndAdditionalPropertiesClass".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "uuid" => intermediate_rep.uuid.push(<uuid::Uuid as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "dateTime" => intermediate_rep.date_time.push(<chrono::DateTime::<chrono::Utc> as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    "map" => return std::result::Result::Err("Parsing a container in this style is not supported in MixedPropertiesAndAdditionalPropertiesClass".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing MixedPropertiesAndAdditionalPropertiesClass".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(MixedPropertiesAndAdditionalPropertiesClass {
            uuid: intermediate_rep.uuid.into_iter().next(),
            date_time: intermediate_rep.date_time.into_iter().next(),
            map: intermediate_rep.map.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<MixedPropertiesAndAdditionalPropertiesClass> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<MixedPropertiesAndAdditionalPropertiesClass>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<MixedPropertiesAndAdditionalPropertiesClass>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for MixedPropertiesAndAdditionalPropertiesClass - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<MixedPropertiesAndAdditionalPropertiesClass> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <MixedPropertiesAndAdditionalPropertiesClass as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into MixedPropertiesAndAdditionalPropertiesClass - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<MixedPropertiesAndAdditionalPropertiesClass>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<MixedPropertiesAndAdditionalPropertiesClass>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<MixedPropertiesAndAdditionalPropertiesClass>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<MixedPropertiesAndAdditionalPropertiesClass> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <MixedPropertiesAndAdditionalPropertiesClass as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into MixedPropertiesAndAdditionalPropertiesClass - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl MixedPropertiesAndAdditionalPropertiesClass {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Model for testing model name starting with number
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
#[serde(rename = "Name")]
pub struct Model200Response {
    #[serde(rename = "name")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub name: Option<i32>,

    #[serde(rename = "class")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub class: Option<String>,

}


impl Model200Response {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Model200Response {
        Model200Response {
            name: None,
            class: None,
        }
    }
}

/// Converts the Model200Response value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for Model200Response {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.name.as_ref().map(|name| {
                [
                    "name".to_string(),
                    name.to_string(),
                ].join(",")
            }),
            self.class.as_ref().map(|class| {
                [
                    "class".to_string(),
                    class.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Model200Response value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Model200Response {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub name: Vec<i32>,
            pub class: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Model200Response".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "name" => intermediate_rep.name.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "class" => intermediate_rep.class.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Model200Response".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Model200Response {
            name: intermediate_rep.name.into_iter().next(),
            class: intermediate_rep.class.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Model200Response> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Model200Response>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Model200Response>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Model200Response - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Model200Response> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Model200Response as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Model200Response - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Model200Response>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Model200Response>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Model200Response>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Model200Response> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Model200Response as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Model200Response - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Model200Response {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Model for testing model name same as property name
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
#[serde(rename = "Name")]
pub struct Name {
    #[serde(rename = "name")]
    pub name: i32,

    #[serde(rename = "snake_case")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub snake_case: Option<i32>,

    #[serde(rename = "property")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub property: Option<String>,

    #[serde(rename = "123Number")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub param_123_number: Option<i32>,

}


impl Name {
    #[allow(clippy::new_without_default)]
    pub fn new(name: i32, ) -> Name {
        Name {
            name,
            snake_case: None,
            property: None,
            param_123_number: None,
        }
    }
}

/// Converts the Name value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for Name {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            Some("name".to_string()),
            Some(self.name.to_string()),
            self.snake_case.as_ref().map(|snake_case| {
                [
                    "snake_case".to_string(),
                    snake_case.to_string(),
                ].join(",")
            }),
            self.property.as_ref().map(|property| {
                [
                    "property".to_string(),
                    property.to_string(),
                ].join(",")
            }),
            self.param_123_number.as_ref().map(|param_123_number| {
                [
                    "123Number".to_string(),
                    param_123_number.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Name value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Name {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub name: Vec<i32>,
            pub snake_case: Vec<i32>,
            pub property: Vec<String>,
            pub param_123_number: Vec<i32>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Name".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "name" => intermediate_rep.name.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "snake_case" => intermediate_rep.snake_case.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "property" => intermediate_rep.property.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "123Number" => intermediate_rep.param_123_number.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Name".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Name {
            name: intermediate_rep.name.into_iter().next().ok_or_else(|| "name missing in Name".to_string())?,
            snake_case: intermediate_rep.snake_case.into_iter().next(),
            property: intermediate_rep.property.into_iter().next(),
            param_123_number: intermediate_rep.param_123_number.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Name> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Name>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Name>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Name - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Name> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Name as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Name - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Name>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Name>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Name>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Name> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Name as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Name - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Name {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct NumberOnly {
    #[serde(rename = "JustNumber")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub just_number: Option<f64>,

}


impl NumberOnly {
    #[allow(clippy::new_without_default)]
    pub fn new() -> NumberOnly {
        NumberOnly {
            just_number: None,
        }
    }
}

/// Converts the NumberOnly value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for NumberOnly {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.just_number.as_ref().map(|just_number| {
                [
                    "JustNumber".to_string(),
                    just_number.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a NumberOnly value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for NumberOnly {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub just_number: Vec<f64>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing NumberOnly".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "JustNumber" => intermediate_rep.just_number.push(<f64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing NumberOnly".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(NumberOnly {
            just_number: intermediate_rep.just_number.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<NumberOnly> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<NumberOnly>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<NumberOnly>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for NumberOnly - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<NumberOnly> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <NumberOnly as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into NumberOnly - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<NumberOnly>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<NumberOnly>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<NumberOnly>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<NumberOnly> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <NumberOnly as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into NumberOnly - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl NumberOnly {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ObjectContainingObjectWithOnlyAdditionalProperties {
    #[serde(rename = "inner")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub inner: Option<models::ObjectWithOnlyAdditionalProperties>,

}


impl ObjectContainingObjectWithOnlyAdditionalProperties {
    #[allow(clippy::new_without_default)]
    pub fn new() -> ObjectContainingObjectWithOnlyAdditionalProperties {
        ObjectContainingObjectWithOnlyAdditionalProperties {
            inner: None,
        }
    }
}

/// Converts the ObjectContainingObjectWithOnlyAdditionalProperties value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for ObjectContainingObjectWithOnlyAdditionalProperties {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            // Skipping non-primitive type inner in query parameter serialization
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectContainingObjectWithOnlyAdditionalProperties value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ObjectContainingObjectWithOnlyAdditionalProperties {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub inner: Vec<models::ObjectWithOnlyAdditionalProperties>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ObjectContainingObjectWithOnlyAdditionalProperties".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "inner" => intermediate_rep.inner.push(<models::ObjectWithOnlyAdditionalProperties as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ObjectContainingObjectWithOnlyAdditionalProperties".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ObjectContainingObjectWithOnlyAdditionalProperties {
            inner: intermediate_rep.inner.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ObjectContainingObjectWithOnlyAdditionalProperties> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectContainingObjectWithOnlyAdditionalProperties>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ObjectContainingObjectWithOnlyAdditionalProperties>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ObjectContainingObjectWithOnlyAdditionalProperties - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<ObjectContainingObjectWithOnlyAdditionalProperties> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ObjectContainingObjectWithOnlyAdditionalProperties as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into ObjectContainingObjectWithOnlyAdditionalProperties - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<ObjectContainingObjectWithOnlyAdditionalProperties>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<ObjectContainingObjectWithOnlyAdditionalProperties>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<ObjectContainingObjectWithOnlyAdditionalProperties>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<ObjectContainingObjectWithOnlyAdditionalProperties> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <ObjectContainingObjectWithOnlyAdditionalProperties as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into ObjectContainingObjectWithOnlyAdditionalProperties - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl ObjectContainingObjectWithOnlyAdditionalProperties {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ObjectWithOnlyAdditionalProperties(std::collections::HashMap<String, String>);

impl std::convert::From<std::collections::HashMap<String, String>> for ObjectWithOnlyAdditionalProperties {
    fn from(x: std::collections::HashMap<String, String>) -> Self {
        ObjectWithOnlyAdditionalProperties(x)
    }
}

impl std::convert::From<ObjectWithOnlyAdditionalProperties> for std::collections::HashMap<String, String> {
    fn from(x: ObjectWithOnlyAdditionalProperties) -> Self {
        x.0
    }
}

impl std::ops::Deref for ObjectWithOnlyAdditionalProperties {
    type Target = std::collections::HashMap<String, String>;
    fn deref(&self) -> &std::collections::HashMap<String, String> {
        &self.0
    }
}

impl std::ops::DerefMut for ObjectWithOnlyAdditionalProperties {
    fn deref_mut(&mut self) -> &mut std::collections::HashMap<String, String> {
        &mut self.0
    }
}

/// Converts the ObjectWithOnlyAdditionalProperties value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for ObjectWithOnlyAdditionalProperties {
    fn to_string(&self) -> String {
        // ToString for this model is not supported
        "".to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectWithOnlyAdditionalProperties value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for ObjectWithOnlyAdditionalProperties {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Err("Parsing ObjectWithOnlyAdditionalProperties is not supported")
    }
}

// Methods for converting between header::IntoHeaderValue<ObjectWithOnlyAdditionalProperties> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectWithOnlyAdditionalProperties>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ObjectWithOnlyAdditionalProperties>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ObjectWithOnlyAdditionalProperties - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<ObjectWithOnlyAdditionalProperties> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ObjectWithOnlyAdditionalProperties as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into ObjectWithOnlyAdditionalProperties - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<ObjectWithOnlyAdditionalProperties>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<ObjectWithOnlyAdditionalProperties>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<ObjectWithOnlyAdditionalProperties>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<ObjectWithOnlyAdditionalProperties> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <ObjectWithOnlyAdditionalProperties as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into ObjectWithOnlyAdditionalProperties - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl ObjectWithOnlyAdditionalProperties {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
#[serde(rename = "Order")]
pub struct Order {
    #[serde(rename = "id")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub id: Option<i64>,

    #[serde(rename = "petId")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub pet_id: Option<i64>,

    #[serde(rename = "quantity")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub quantity: Option<i32>,

    #[serde(rename = "shipDate")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub ship_date: Option<chrono::DateTime::<chrono::Utc>>,

    #[serde(rename = "status")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub status: Option<models::OrderStatus>,

    #[serde(rename = "complete")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub complete: Option<bool>,

}


impl Order {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Order {
        Order {
            id: None,
            pet_id: None,
            quantity: None,
            ship_date: None,
            status: None,
            complete: Some(false),
        }
    }
}

/// Converts the Order value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for Order {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.id.as_ref().map(|id| {
                [
                    "id".to_string(),
                    id.to_string(),
                ].join(",")
            }),
            self.pet_id.as_ref().map(|pet_id| {
                [
                    "petId".to_string(),
                    pet_id.to_string(),
                ].join(",")
            }),
            self.quantity.as_ref().map(|quantity| {
                [
                    "quantity".to_string(),
                    quantity.to_string(),
                ].join(",")
            }),
            // Skipping non-primitive type shipDate in query parameter serialization
            // Skipping non-primitive type status in query parameter serialization
            self.complete.as_ref().map(|complete| {
                [
                    "complete".to_string(),
                    complete.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Order value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Order {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub id: Vec<i64>,
            pub pet_id: Vec<i64>,
            pub quantity: Vec<i32>,
            pub ship_date: Vec<chrono::DateTime::<chrono::Utc>>,
            pub status: Vec<models::OrderStatus>,
            pub complete: Vec<bool>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Order".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "id" => intermediate_rep.id.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "petId" => intermediate_rep.pet_id.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "quantity" => intermediate_rep.quantity.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "shipDate" => intermediate_rep.ship_date.push(<chrono::DateTime::<chrono::Utc> as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "status" => intermediate_rep.status.push(<models::OrderStatus as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "complete" => intermediate_rep.complete.push(<bool as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Order".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Order {
            id: intermediate_rep.id.into_iter().next(),
            pet_id: intermediate_rep.pet_id.into_iter().next(),
            quantity: intermediate_rep.quantity.into_iter().next(),
            ship_date: intermediate_rep.ship_date.into_iter().next(),
            status: intermediate_rep.status.into_iter().next(),
            complete: intermediate_rep.complete.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Order> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Order>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Order>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Order - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Order> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Order as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Order - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Order>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Order>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Order>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Order> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Order as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Order - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Order {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Order Status
/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize, Hash)]
#[cfg_attr(feature = "conversion", derive(frunk_enum_derive::LabelledGenericEnum))]
pub enum OrderStatus {
    #[serde(rename = "placed")]
    Placed,
    #[serde(rename = "approved")]
    Approved,
    #[serde(rename = "delivered")]
    Delivered,
}

impl std::fmt::Display for OrderStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            OrderStatus::Placed => write!(f, "placed"),
            OrderStatus::Approved => write!(f, "approved"),
            OrderStatus::Delivered => write!(f, "delivered"),
        }
    }
}

impl std::str::FromStr for OrderStatus {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "placed" => std::result::Result::Ok(OrderStatus::Placed),
            "approved" => std::result::Result::Ok(OrderStatus::Approved),
            "delivered" => std::result::Result::Ok(OrderStatus::Delivered),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<OrderStatus> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<OrderStatus>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<OrderStatus>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for OrderStatus - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<OrderStatus> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <OrderStatus as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into OrderStatus - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<OrderStatus>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<OrderStatus>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<OrderStatus>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<OrderStatus> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <OrderStatus as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into OrderStatus - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl OrderStatus {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct OuterBoolean(bool);

impl std::convert::From<bool> for OuterBoolean {
    fn from(x: bool) -> Self {
        OuterBoolean(x)
    }
}

impl std::convert::From<OuterBoolean> for bool {
    fn from(x: OuterBoolean) -> Self {
        x.0
    }
}

impl std::ops::Deref for OuterBoolean {
    type Target = bool;
    fn deref(&self) -> &bool {
        &self.0
    }
}

impl std::ops::DerefMut for OuterBoolean {
    fn deref_mut(&mut self) -> &mut bool {
        &mut self.0
    }
}

/// Converts the OuterBoolean value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for OuterBoolean {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a OuterBoolean value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for OuterBoolean {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match std::str::FromStr::from_str(s) {
             std::result::Result::Ok(r) => std::result::Result::Ok(OuterBoolean(r)),
             std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {s} to OuterBoolean: {e:?}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<OuterBoolean> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<OuterBoolean>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<OuterBoolean>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for OuterBoolean - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<OuterBoolean> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <OuterBoolean as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into OuterBoolean - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<OuterBoolean>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<OuterBoolean>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<OuterBoolean>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<OuterBoolean> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <OuterBoolean as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into OuterBoolean - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl OuterBoolean {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct OuterComposite {
    #[serde(rename = "my_number")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub my_number: Option<f64>,

    #[serde(rename = "my_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub my_string: Option<String>,

    #[serde(rename = "my_boolean")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub my_boolean: Option<bool>,

}


impl OuterComposite {
    #[allow(clippy::new_without_default)]
    pub fn new() -> OuterComposite {
        OuterComposite {
            my_number: None,
            my_string: None,
            my_boolean: None,
        }
    }
}

/// Converts the OuterComposite value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for OuterComposite {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.my_number.as_ref().map(|my_number| {
                [
                    "my_number".to_string(),
                    my_number.to_string(),
                ].join(",")
            }),
            self.my_string.as_ref().map(|my_string| {
                [
                    "my_string".to_string(),
                    my_string.to_string(),
                ].join(",")
            }),
            self.my_boolean.as_ref().map(|my_boolean| {
                [
                    "my_boolean".to_string(),
                    my_boolean.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a OuterComposite value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for OuterComposite {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub my_number: Vec<f64>,
            pub my_string: Vec<String>,
            pub my_boolean: Vec<bool>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing OuterComposite".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "my_number" => intermediate_rep.my_number.push(<f64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "my_string" => intermediate_rep.my_string.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "my_boolean" => intermediate_rep.my_boolean.push(<bool as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing OuterComposite".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(OuterComposite {
            my_number: intermediate_rep.my_number.into_iter().next(),
            my_string: intermediate_rep.my_string.into_iter().next(),
            my_boolean: intermediate_rep.my_boolean.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<OuterComposite> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<OuterComposite>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<OuterComposite>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for OuterComposite - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<OuterComposite> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <OuterComposite as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into OuterComposite - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<OuterComposite>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<OuterComposite>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<OuterComposite>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<OuterComposite> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <OuterComposite as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into OuterComposite - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl OuterComposite {
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
pub enum OuterEnum {
    #[serde(rename = "placed")]
    Placed,
    #[serde(rename = "approved")]
    Approved,
    #[serde(rename = "delivered")]
    Delivered,
}

impl std::fmt::Display for OuterEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            OuterEnum::Placed => write!(f, "placed"),
            OuterEnum::Approved => write!(f, "approved"),
            OuterEnum::Delivered => write!(f, "delivered"),
        }
    }
}

impl std::str::FromStr for OuterEnum {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "placed" => std::result::Result::Ok(OuterEnum::Placed),
            "approved" => std::result::Result::Ok(OuterEnum::Approved),
            "delivered" => std::result::Result::Ok(OuterEnum::Delivered),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<OuterEnum> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<OuterEnum>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<OuterEnum>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for OuterEnum - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<OuterEnum> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <OuterEnum as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into OuterEnum - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<OuterEnum>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<OuterEnum>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<OuterEnum>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<OuterEnum> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <OuterEnum as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into OuterEnum - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl OuterEnum {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct OuterNumber(f64);

impl std::convert::From<f64> for OuterNumber {
    fn from(x: f64) -> Self {
        OuterNumber(x)
    }
}

impl std::convert::From<OuterNumber> for f64 {
    fn from(x: OuterNumber) -> Self {
        x.0
    }
}

impl std::ops::Deref for OuterNumber {
    type Target = f64;
    fn deref(&self) -> &f64 {
        &self.0
    }
}

impl std::ops::DerefMut for OuterNumber {
    fn deref_mut(&mut self) -> &mut f64 {
        &mut self.0
    }
}

/// Converts the OuterNumber value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for OuterNumber {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a OuterNumber value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for OuterNumber {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match std::str::FromStr::from_str(s) {
             std::result::Result::Ok(r) => std::result::Result::Ok(OuterNumber(r)),
             std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {s} to OuterNumber: {e:?}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<OuterNumber> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<OuterNumber>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<OuterNumber>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for OuterNumber - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<OuterNumber> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <OuterNumber as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into OuterNumber - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<OuterNumber>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<OuterNumber>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<OuterNumber>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<OuterNumber> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <OuterNumber as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into OuterNumber - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl OuterNumber {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct OuterString(String);

impl std::convert::From<String> for OuterString {
    fn from(x: String) -> Self {
        OuterString(x)
    }
}

impl std::convert::From<OuterString> for String {
    fn from(x: OuterString) -> Self {
        x.0
    }
}

impl std::ops::Deref for OuterString {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for OuterString {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

impl std::string::ToString for OuterString {
    fn to_string(&self) -> String {
       self.0.clone()
    }
}

impl std::str::FromStr for OuterString {
    type Err = ::std::convert::Infallible;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(OuterString(x.to_owned()))
    }
}

// Methods for converting between header::IntoHeaderValue<OuterString> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<OuterString>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<OuterString>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for OuterString - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<OuterString> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <OuterString as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into OuterString - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<OuterString>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<OuterString>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<OuterString>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<OuterString> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <OuterString as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into OuterString - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl OuterString {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
#[serde(rename = "Pet")]
pub struct Pet {
    #[serde(rename = "id")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub id: Option<i64>,

    #[serde(rename = "category")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub category: Option<models::Category>,

    #[serde(rename = "name")]
    pub name: String,

    #[serde(rename = "photoUrls")]
    pub photo_urls: Vec<String>,

    #[serde(rename = "tags")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub tags: Option<Vec<models::Tag>>,

    #[serde(rename = "status")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub status: Option<models::PetStatus>,

}


impl Pet {
    #[allow(clippy::new_without_default)]
    pub fn new(name: String, photo_urls: Vec<String>, ) -> Pet {
        Pet {
            id: None,
            category: None,
            name,
            photo_urls,
            tags: None,
            status: None,
        }
    }
}

/// Converts the Pet value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for Pet {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.id.as_ref().map(|id| {
                [
                    "id".to_string(),
                    id.to_string(),
                ].join(",")
            }),
            // Skipping non-primitive type category in query parameter serialization
            Some("name".to_string()),
            Some(self.name.to_string()),
            Some("photoUrls".to_string()),
            Some(self.photo_urls.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",")),
            // Skipping non-primitive type tags in query parameter serialization
            // Skipping non-primitive type status in query parameter serialization
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Pet value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Pet {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub id: Vec<i64>,
            pub category: Vec<models::Category>,
            pub name: Vec<String>,
            pub photo_urls: Vec<Vec<String>>,
            pub tags: Vec<Vec<models::Tag>>,
            pub status: Vec<models::PetStatus>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Pet".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "id" => intermediate_rep.id.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "category" => intermediate_rep.category.push(<models::Category as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "name" => intermediate_rep.name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    "photoUrls" => return std::result::Result::Err("Parsing a container in this style is not supported in Pet".to_string()),
                    "tags" => return std::result::Result::Err("Parsing a container in this style is not supported in Pet".to_string()),
                    #[allow(clippy::redundant_clone)]
                    "status" => intermediate_rep.status.push(<models::PetStatus as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Pet".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Pet {
            id: intermediate_rep.id.into_iter().next(),
            category: intermediate_rep.category.into_iter().next(),
            name: intermediate_rep.name.into_iter().next().ok_or_else(|| "name missing in Pet".to_string())?,
            photo_urls: intermediate_rep.photo_urls.into_iter().next().ok_or_else(|| "photoUrls missing in Pet".to_string())?,
            tags: intermediate_rep.tags.into_iter().next(),
            status: intermediate_rep.status.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Pet> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Pet>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Pet>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Pet - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Pet> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Pet as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Pet - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Pet>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Pet>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Pet>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Pet> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Pet as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Pet - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Pet {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// pet status in the store
/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize, Hash)]
#[cfg_attr(feature = "conversion", derive(frunk_enum_derive::LabelledGenericEnum))]
pub enum PetStatus {
    #[serde(rename = "available")]
    Available,
    #[serde(rename = "pending")]
    Pending,
    #[serde(rename = "sold")]
    Sold,
}

impl std::fmt::Display for PetStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            PetStatus::Available => write!(f, "available"),
            PetStatus::Pending => write!(f, "pending"),
            PetStatus::Sold => write!(f, "sold"),
        }
    }
}

impl std::str::FromStr for PetStatus {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "available" => std::result::Result::Ok(PetStatus::Available),
            "pending" => std::result::Result::Ok(PetStatus::Pending),
            "sold" => std::result::Result::Ok(PetStatus::Sold),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<PetStatus> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<PetStatus>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<PetStatus>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for PetStatus - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<PetStatus> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <PetStatus as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into PetStatus - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<PetStatus>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<PetStatus>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<PetStatus>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<PetStatus> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <PetStatus as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into PetStatus - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl PetStatus {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ReadOnlyFirst {
    #[serde(rename = "bar")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub bar: Option<String>,

    #[serde(rename = "baz")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub baz: Option<String>,

}


impl ReadOnlyFirst {
    #[allow(clippy::new_without_default)]
    pub fn new() -> ReadOnlyFirst {
        ReadOnlyFirst {
            bar: None,
            baz: None,
        }
    }
}

/// Converts the ReadOnlyFirst value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for ReadOnlyFirst {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.bar.as_ref().map(|bar| {
                [
                    "bar".to_string(),
                    bar.to_string(),
                ].join(",")
            }),
            self.baz.as_ref().map(|baz| {
                [
                    "baz".to_string(),
                    baz.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ReadOnlyFirst value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ReadOnlyFirst {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub bar: Vec<String>,
            pub baz: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ReadOnlyFirst".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "bar" => intermediate_rep.bar.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "baz" => intermediate_rep.baz.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ReadOnlyFirst".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ReadOnlyFirst {
            bar: intermediate_rep.bar.into_iter().next(),
            baz: intermediate_rep.baz.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ReadOnlyFirst> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<ReadOnlyFirst>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ReadOnlyFirst>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ReadOnlyFirst - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<ReadOnlyFirst> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ReadOnlyFirst as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into ReadOnlyFirst - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<ReadOnlyFirst>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<ReadOnlyFirst>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<ReadOnlyFirst>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<ReadOnlyFirst> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <ReadOnlyFirst as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into ReadOnlyFirst - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl ReadOnlyFirst {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Model for testing reserved words
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
#[serde(rename = "Return")]
pub struct Return {
    #[serde(rename = "return")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub r#return: Option<i32>,

}


impl Return {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Return {
        Return {
            r#return: None,
        }
    }
}

/// Converts the Return value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for Return {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.r#return.as_ref().map(|r#return| {
                [
                    "return".to_string(),
                    r#return.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Return value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Return {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub r#return: Vec<i32>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Return".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "return" => intermediate_rep.r#return.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Return".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Return {
            r#return: intermediate_rep.r#return.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Return> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Return>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Return>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Return - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Return> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Return as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Return - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Return>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Return>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Return>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Return> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Return as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Return - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Return {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
#[serde(rename = "Tag")]
pub struct Tag {
    #[serde(rename = "id")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub id: Option<i64>,

    #[serde(rename = "name")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub name: Option<String>,

}


impl Tag {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Tag {
        Tag {
            id: None,
            name: None,
        }
    }
}

/// Converts the Tag value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for Tag {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.id.as_ref().map(|id| {
                [
                    "id".to_string(),
                    id.to_string(),
                ].join(",")
            }),
            self.name.as_ref().map(|name| {
                [
                    "name".to_string(),
                    name.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Tag value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Tag {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub id: Vec<i64>,
            pub name: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Tag".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "id" => intermediate_rep.id.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "name" => intermediate_rep.name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Tag".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Tag {
            id: intermediate_rep.id.into_iter().next(),
            name: intermediate_rep.name.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Tag> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<Tag>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Tag>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Tag - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Tag> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Tag as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into Tag - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<Tag>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<Tag>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<Tag>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<Tag> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <Tag as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into Tag - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl Tag {
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
pub enum TestEnumParametersEnumHeaderStringArrayParameterInner {
    #[serde(rename = ">")]
    GreaterThan,
    #[serde(rename = "$")]
    Dollar,
}

impl std::fmt::Display for TestEnumParametersEnumHeaderStringArrayParameterInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            TestEnumParametersEnumHeaderStringArrayParameterInner::GreaterThan => write!(f, ">"),
            TestEnumParametersEnumHeaderStringArrayParameterInner::Dollar => write!(f, "$"),
        }
    }
}

impl std::str::FromStr for TestEnumParametersEnumHeaderStringArrayParameterInner {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            ">" => std::result::Result::Ok(TestEnumParametersEnumHeaderStringArrayParameterInner::GreaterThan),
            "$" => std::result::Result::Ok(TestEnumParametersEnumHeaderStringArrayParameterInner::Dollar),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<TestEnumParametersEnumHeaderStringArrayParameterInner> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<TestEnumParametersEnumHeaderStringArrayParameterInner>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<TestEnumParametersEnumHeaderStringArrayParameterInner>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for TestEnumParametersEnumHeaderStringArrayParameterInner - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<TestEnumParametersEnumHeaderStringArrayParameterInner> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <TestEnumParametersEnumHeaderStringArrayParameterInner as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into TestEnumParametersEnumHeaderStringArrayParameterInner - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<TestEnumParametersEnumHeaderStringArrayParameterInner>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<TestEnumParametersEnumHeaderStringArrayParameterInner>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<TestEnumParametersEnumHeaderStringArrayParameterInner>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<TestEnumParametersEnumHeaderStringArrayParameterInner> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <TestEnumParametersEnumHeaderStringArrayParameterInner as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into TestEnumParametersEnumHeaderStringArrayParameterInner - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl TestEnumParametersEnumHeaderStringArrayParameterInner {
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
pub enum TestEnumParametersEnumHeaderStringParameter {
    #[serde(rename = "_abc")]
    Abc,
    #[serde(rename = "-efg")]
    Efg,
    #[serde(rename = "(xyz)")]
    LeftParenthesisXyzRightParenthesis,
}

impl std::fmt::Display for TestEnumParametersEnumHeaderStringParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            TestEnumParametersEnumHeaderStringParameter::Abc => write!(f, "_abc"),
            TestEnumParametersEnumHeaderStringParameter::Efg => write!(f, "-efg"),
            TestEnumParametersEnumHeaderStringParameter::LeftParenthesisXyzRightParenthesis => write!(f, "(xyz)"),
        }
    }
}

impl std::str::FromStr for TestEnumParametersEnumHeaderStringParameter {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "_abc" => std::result::Result::Ok(TestEnumParametersEnumHeaderStringParameter::Abc),
            "-efg" => std::result::Result::Ok(TestEnumParametersEnumHeaderStringParameter::Efg),
            "(xyz)" => std::result::Result::Ok(TestEnumParametersEnumHeaderStringParameter::LeftParenthesisXyzRightParenthesis),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<TestEnumParametersEnumHeaderStringParameter> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<TestEnumParametersEnumHeaderStringParameter>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<TestEnumParametersEnumHeaderStringParameter>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for TestEnumParametersEnumHeaderStringParameter - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<TestEnumParametersEnumHeaderStringParameter> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <TestEnumParametersEnumHeaderStringParameter as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into TestEnumParametersEnumHeaderStringParameter - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<TestEnumParametersEnumHeaderStringParameter>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<TestEnumParametersEnumHeaderStringParameter>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<TestEnumParametersEnumHeaderStringParameter>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<TestEnumParametersEnumHeaderStringParameter> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <TestEnumParametersEnumHeaderStringParameter as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into TestEnumParametersEnumHeaderStringParameter - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl TestEnumParametersEnumHeaderStringParameter {
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
pub enum TestEnumParametersEnumQueryDoubleParameter {
    #[serde(rename = "1.1")]
    Variant11,
    #[serde(rename = "-1.2")]
    Variant12,
}

impl std::fmt::Display for TestEnumParametersEnumQueryDoubleParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            TestEnumParametersEnumQueryDoubleParameter::Variant11 => write!(f, "1.1"),
            TestEnumParametersEnumQueryDoubleParameter::Variant12 => write!(f, "-1.2"),
        }
    }
}

impl std::str::FromStr for TestEnumParametersEnumQueryDoubleParameter {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "1.1" => std::result::Result::Ok(TestEnumParametersEnumQueryDoubleParameter::Variant11),
            "-1.2" => std::result::Result::Ok(TestEnumParametersEnumQueryDoubleParameter::Variant12),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<TestEnumParametersEnumQueryDoubleParameter> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<TestEnumParametersEnumQueryDoubleParameter>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<TestEnumParametersEnumQueryDoubleParameter>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for TestEnumParametersEnumQueryDoubleParameter - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<TestEnumParametersEnumQueryDoubleParameter> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <TestEnumParametersEnumQueryDoubleParameter as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into TestEnumParametersEnumQueryDoubleParameter - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<TestEnumParametersEnumQueryDoubleParameter>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<TestEnumParametersEnumQueryDoubleParameter>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<TestEnumParametersEnumQueryDoubleParameter>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<TestEnumParametersEnumQueryDoubleParameter> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <TestEnumParametersEnumQueryDoubleParameter as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into TestEnumParametersEnumQueryDoubleParameter - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl TestEnumParametersEnumQueryDoubleParameter {
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
pub enum TestEnumParametersEnumQueryIntegerParameter {
    #[serde(rename = "1")]
    Variant1,
    #[serde(rename = "-2")]
    Variant2,
}

impl std::fmt::Display for TestEnumParametersEnumQueryIntegerParameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            TestEnumParametersEnumQueryIntegerParameter::Variant1 => write!(f, "1"),
            TestEnumParametersEnumQueryIntegerParameter::Variant2 => write!(f, "-2"),
        }
    }
}

impl std::str::FromStr for TestEnumParametersEnumQueryIntegerParameter {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "1" => std::result::Result::Ok(TestEnumParametersEnumQueryIntegerParameter::Variant1),
            "-2" => std::result::Result::Ok(TestEnumParametersEnumQueryIntegerParameter::Variant2),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<TestEnumParametersEnumQueryIntegerParameter> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<TestEnumParametersEnumQueryIntegerParameter>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<TestEnumParametersEnumQueryIntegerParameter>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for TestEnumParametersEnumQueryIntegerParameter - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<TestEnumParametersEnumQueryIntegerParameter> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <TestEnumParametersEnumQueryIntegerParameter as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into TestEnumParametersEnumQueryIntegerParameter - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<TestEnumParametersEnumQueryIntegerParameter>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<TestEnumParametersEnumQueryIntegerParameter>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<TestEnumParametersEnumQueryIntegerParameter>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<TestEnumParametersEnumQueryIntegerParameter> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <TestEnumParametersEnumQueryIntegerParameter as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into TestEnumParametersEnumQueryIntegerParameter - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl TestEnumParametersEnumQueryIntegerParameter {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Form parameter enum test (string)
/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize, Hash)]
#[cfg_attr(feature = "conversion", derive(frunk_enum_derive::LabelledGenericEnum))]
pub enum TestEnumParametersRequestEnumFormString {
    #[serde(rename = "_abc")]
    Abc,
    #[serde(rename = "-efg")]
    Efg,
    #[serde(rename = "(xyz)")]
    LeftParenthesisXyzRightParenthesis,
}

impl std::fmt::Display for TestEnumParametersRequestEnumFormString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            TestEnumParametersRequestEnumFormString::Abc => write!(f, "_abc"),
            TestEnumParametersRequestEnumFormString::Efg => write!(f, "-efg"),
            TestEnumParametersRequestEnumFormString::LeftParenthesisXyzRightParenthesis => write!(f, "(xyz)"),
        }
    }
}

impl std::str::FromStr for TestEnumParametersRequestEnumFormString {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "_abc" => std::result::Result::Ok(TestEnumParametersRequestEnumFormString::Abc),
            "-efg" => std::result::Result::Ok(TestEnumParametersRequestEnumFormString::Efg),
            "(xyz)" => std::result::Result::Ok(TestEnumParametersRequestEnumFormString::LeftParenthesisXyzRightParenthesis),
            _ => std::result::Result::Err(format!("Value not valid: {s}")),
        }
    }
}

// Methods for converting between header::IntoHeaderValue<TestEnumParametersRequestEnumFormString> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<TestEnumParametersRequestEnumFormString>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<TestEnumParametersRequestEnumFormString>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for TestEnumParametersRequestEnumFormString - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<TestEnumParametersRequestEnumFormString> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <TestEnumParametersRequestEnumFormString as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into TestEnumParametersRequestEnumFormString - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<TestEnumParametersRequestEnumFormString>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<TestEnumParametersRequestEnumFormString>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<TestEnumParametersRequestEnumFormString>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<TestEnumParametersRequestEnumFormString> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <TestEnumParametersRequestEnumFormString as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into TestEnumParametersRequestEnumFormString - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl TestEnumParametersRequestEnumFormString {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
#[serde(rename = "User")]
pub struct User {
    #[serde(rename = "id")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub id: Option<i64>,

    #[serde(rename = "username")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub username: Option<String>,

    #[serde(rename = "firstName")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub first_name: Option<String>,

    #[serde(rename = "lastName")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub last_name: Option<String>,

    #[serde(rename = "email")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub email: Option<String>,

    #[serde(rename = "password")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub password: Option<String>,

    #[serde(rename = "phone")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub phone: Option<String>,

    /// User Status
    #[serde(rename = "userStatus")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub user_status: Option<i32>,

}


impl User {
    #[allow(clippy::new_without_default)]
    pub fn new() -> User {
        User {
            id: None,
            username: None,
            first_name: None,
            last_name: None,
            email: None,
            password: None,
            phone: None,
            user_status: None,
        }
    }
}

/// Converts the User value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for User {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            self.id.as_ref().map(|id| {
                [
                    "id".to_string(),
                    id.to_string(),
                ].join(",")
            }),
            self.username.as_ref().map(|username| {
                [
                    "username".to_string(),
                    username.to_string(),
                ].join(",")
            }),
            self.first_name.as_ref().map(|first_name| {
                [
                    "firstName".to_string(),
                    first_name.to_string(),
                ].join(",")
            }),
            self.last_name.as_ref().map(|last_name| {
                [
                    "lastName".to_string(),
                    last_name.to_string(),
                ].join(",")
            }),
            self.email.as_ref().map(|email| {
                [
                    "email".to_string(),
                    email.to_string(),
                ].join(",")
            }),
            self.password.as_ref().map(|password| {
                [
                    "password".to_string(),
                    password.to_string(),
                ].join(",")
            }),
            self.phone.as_ref().map(|phone| {
                [
                    "phone".to_string(),
                    phone.to_string(),
                ].join(",")
            }),
            self.user_status.as_ref().map(|user_status| {
                [
                    "userStatus".to_string(),
                    user_status.to_string(),
                ].join(",")
            }),
        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a User value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for User {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub id: Vec<i64>,
            pub username: Vec<String>,
            pub first_name: Vec<String>,
            pub last_name: Vec<String>,
            pub email: Vec<String>,
            pub password: Vec<String>,
            pub phone: Vec<String>,
            pub user_status: Vec<i32>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing User".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "id" => intermediate_rep.id.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "username" => intermediate_rep.username.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "firstName" => intermediate_rep.first_name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "lastName" => intermediate_rep.last_name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "email" => intermediate_rep.email.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "password" => intermediate_rep.password.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "phone" => intermediate_rep.phone.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "userStatus" => intermediate_rep.user_status.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing User".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(User {
            id: intermediate_rep.id.into_iter().next(),
            username: intermediate_rep.username.into_iter().next(),
            first_name: intermediate_rep.first_name.into_iter().next(),
            last_name: intermediate_rep.last_name.into_iter().next(),
            email: intermediate_rep.email.into_iter().next(),
            password: intermediate_rep.password.into_iter().next(),
            phone: intermediate_rep.phone.into_iter().next(),
            user_status: intermediate_rep.user_status.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<User> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<User>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<User>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for User - value: {hdr_value} is invalid {e}"))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<User> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <User as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{value}' into User - {err}"))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {hdr_value:?} to string: {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Vec<User>>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_values: header::IntoHeaderValue<Vec<User>>) -> std::result::Result<Self, Self::Error> {
        let hdr_values : Vec<String> = hdr_values.0.into_iter().map(|hdr_value| {
            hdr_value.to_string()
        }).collect();

        match hyper::header::HeaderValue::from_str(&hdr_values.join(", ")) {
           std::result::Result::Ok(hdr_value) => std::result::Result::Ok(hdr_value),
           std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to convert {hdr_values:?} into a header - {e}"))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<Vec<User>> {
    type Error = String;

    fn try_from(hdr_values: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_values.to_str() {
            std::result::Result::Ok(hdr_values) => {
                let hdr_values : std::vec::Vec<User> = hdr_values
                .split(',')
                .filter_map(|hdr_value| match hdr_value.trim() {
                    "" => std::option::Option::None,
                    hdr_value => std::option::Option::Some({
                        match <User as std::str::FromStr>::from_str(hdr_value) {
                            std::result::Result::Ok(value) => std::result::Result::Ok(value),
                            std::result::Result::Err(err) => std::result::Result::Err(
                                format!("Unable to convert header value '{hdr_value}' into User - {err}"))
                        }
                    })
                }).collect::<std::result::Result<std::vec::Vec<_>, String>>()?;

                std::result::Result::Ok(header::IntoHeaderValue(hdr_values))
            },
            std::result::Result::Err(e) => std::result::Result::Err(format!("Unable to parse header: {hdr_values:?} as a string - {e}")),
        }
    }
}

impl User {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn as_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}
