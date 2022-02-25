#![allow(unused_qualifications)]

use crate::models;
#[cfg(any(feature = "client", feature = "server"))]
use crate::header;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
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

/// Converts the ANullableContainer value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for ANullableContainer {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref nullable_thing) = self.nullable_thing {
            params.push("NullableThing".to_string());
            params.push(nullable_thing.as_ref().map_or("null".to_string(), |x| x.to_string()));
        }


        params.push("RequiredNullableThing".to_string());
        params.push(self.required_nullable_thing.as_ref().map_or("null".to_string(), |x| x.to_string()));

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ANullableContainer value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ANullableContainer {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub nullable_thing: Vec<String>,
            pub required_nullable_thing: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ANullableContainer".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "NullableThing" => return std::result::Result::Err("Parsing a nullable type in this style is not supported in ANullableContainer".to_string()),
                    "RequiredNullableThing" => return std::result::Result::Err("Parsing a nullable type in this style is not supported in ANullableContainer".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing ANullableContainer".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ANullableContainer {
            nullable_thing: std::result::Result::Err("Nullable types not supported in ANullableContainer".to_string())?,
            required_nullable_thing: std::result::Result::Err("Nullable types not supported in ANullableContainer".to_string())?,
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ANullableContainer> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<ANullableContainer>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ANullableContainer>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ANullableContainer - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<ANullableContainer> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ANullableContainer as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into ANullableContainer - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}


/// An additionalPropertiesObject
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AdditionalPropertiesObject(std::collections::HashMap<String, String>);

impl std::convert::From<std::collections::HashMap<String, String>> for AdditionalPropertiesObject {
    fn from(x: std::collections::HashMap<String, String>) -> Self {
        AdditionalPropertiesObject(x)
    }
}

impl std::convert::From<AdditionalPropertiesObject> for std::collections::HashMap<String, String> {
    fn from(x: AdditionalPropertiesObject) -> Self {
        x.0
    }
}

impl std::ops::Deref for AdditionalPropertiesObject {
    type Target = std::collections::HashMap<String, String>;
    fn deref(&self) -> &std::collections::HashMap<String, String> {
        &self.0
    }
}

impl std::ops::DerefMut for AdditionalPropertiesObject {
    fn deref_mut(&mut self) -> &mut std::collections::HashMap<String, String> {
        &mut self.0
    }
}

/// Converts the AdditionalPropertiesObject value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for AdditionalPropertiesObject {
    fn to_string(&self) -> String {
        // Skipping additionalProperties in query parameter serialization
        "".to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AdditionalPropertiesObject value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for AdditionalPropertiesObject {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Err("Parsing additionalProperties for AdditionalPropertiesObject is not supported")
    }
}

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AllOfObject {
    #[serde(rename = "sampleProperty")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub sample_property: Option<String>,

    #[serde(rename = "sampleBasePropery")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub sample_base_propery: Option<String>,

}

impl AllOfObject {
    pub fn new() -> AllOfObject {
        AllOfObject {
            sample_property: None,
            sample_base_propery: None,
        }
    }
}

/// Converts the AllOfObject value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for AllOfObject {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref sample_property) = self.sample_property {
            params.push("sampleProperty".to_string());
            params.push(sample_property.to_string());
        }


        if let Some(ref sample_base_propery) = self.sample_base_propery {
            params.push("sampleBasePropery".to_string());
            params.push(sample_base_propery.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AllOfObject value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AllOfObject {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub sample_property: Vec<String>,
            pub sample_base_propery: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing AllOfObject".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "sampleProperty" => intermediate_rep.sample_property.push(<String as std::str::FromStr>::from_str(val).map_err(|x| format!("{}", x))?),
                    "sampleBasePropery" => intermediate_rep.sample_base_propery.push(<String as std::str::FromStr>::from_str(val).map_err(|x| format!("{}", x))?),
                    _ => return std::result::Result::Err("Unexpected key while parsing AllOfObject".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(AllOfObject {
            sample_property: intermediate_rep.sample_property.into_iter().next(),
            sample_base_propery: intermediate_rep.sample_base_propery.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<AllOfObject> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<AllOfObject>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AllOfObject>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AllOfObject - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<AllOfObject> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AllOfObject as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into AllOfObject - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}


#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct BaseAllOf {
    #[serde(rename = "sampleBasePropery")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub sample_base_propery: Option<String>,

}

impl BaseAllOf {
    pub fn new() -> BaseAllOf {
        BaseAllOf {
            sample_base_propery: None,
        }
    }
}

/// Converts the BaseAllOf value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for BaseAllOf {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref sample_base_propery) = self.sample_base_propery {
            params.push("sampleBasePropery".to_string());
            params.push(sample_base_propery.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a BaseAllOf value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for BaseAllOf {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub sample_base_propery: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing BaseAllOf".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "sampleBasePropery" => intermediate_rep.sample_base_propery.push(<String as std::str::FromStr>::from_str(val).map_err(|x| format!("{}", x))?),
                    _ => return std::result::Result::Err("Unexpected key while parsing BaseAllOf".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(BaseAllOf {
            sample_base_propery: intermediate_rep.sample_base_propery.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<BaseAllOf> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<BaseAllOf>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<BaseAllOf>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for BaseAllOf - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<BaseAllOf> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <BaseAllOf as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into BaseAllOf - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}


/// structured response
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct GetYamlResponse {
    /// Inner string
    #[serde(rename = "value")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub value: Option<String>,

}

impl GetYamlResponse {
    pub fn new() -> GetYamlResponse {
        GetYamlResponse {
            value: None,
        }
    }
}

/// Converts the GetYamlResponse value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for GetYamlResponse {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref value) = self.value {
            params.push("value".to_string());
            params.push(value.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a GetYamlResponse value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for GetYamlResponse {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub value: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing GetYamlResponse".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "value" => intermediate_rep.value.push(<String as std::str::FromStr>::from_str(val).map_err(|x| format!("{}", x))?),
                    _ => return std::result::Result::Err("Unexpected key while parsing GetYamlResponse".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(GetYamlResponse {
            value: intermediate_rep.value.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<GetYamlResponse> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<GetYamlResponse>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<GetYamlResponse>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for GetYamlResponse - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<GetYamlResponse> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <GetYamlResponse as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into GetYamlResponse - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}


#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
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

/// Converts the InlineObject value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for InlineObject {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        params.push("id".to_string());
        params.push(self.id.to_string());


        if let Some(ref password) = self.password {
            params.push("password".to_string());
            params.push(password.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a InlineObject value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for InlineObject {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub id: Vec<String>,
            pub password: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing InlineObject".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "id" => intermediate_rep.id.push(<String as std::str::FromStr>::from_str(val).map_err(|x| format!("{}", x))?),
                    "password" => intermediate_rep.password.push(<String as std::str::FromStr>::from_str(val).map_err(|x| format!("{}", x))?),
                    _ => return std::result::Result::Err("Unexpected key while parsing InlineObject".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(InlineObject {
            id: intermediate_rep.id.into_iter().next().ok_or("id missing in InlineObject".to_string())?,
            password: intermediate_rep.password.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<InlineObject> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<InlineObject>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<InlineObject>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for InlineObject - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<InlineObject> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <InlineObject as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into InlineObject - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}


/// An object of objects
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
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

/// Converts the ObjectOfObjects value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for ObjectOfObjects {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];
        // Skipping inner in query parameter serialization

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectOfObjects value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ObjectOfObjects {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub inner: Vec<models::ObjectOfObjectsInner>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ObjectOfObjects".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "inner" => intermediate_rep.inner.push(<models::ObjectOfObjectsInner as std::str::FromStr>::from_str(val).map_err(|x| format!("{}", x))?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ObjectOfObjects".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ObjectOfObjects {
            inner: intermediate_rep.inner.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ObjectOfObjects> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectOfObjects>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ObjectOfObjects>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ObjectOfObjects - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<ObjectOfObjects> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ObjectOfObjects as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into ObjectOfObjects - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}


#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ObjectOfObjectsInner {
    #[serde(rename = "required_thing")]
    pub required_thing: String,

    #[serde(rename = "optional_thing")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub optional_thing: Option<isize>,

}

impl ObjectOfObjectsInner {
    pub fn new(required_thing: String, ) -> ObjectOfObjectsInner {
        ObjectOfObjectsInner {
            required_thing: required_thing,
            optional_thing: None,
        }
    }
}

/// Converts the ObjectOfObjectsInner value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for ObjectOfObjectsInner {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        params.push("required_thing".to_string());
        params.push(self.required_thing.to_string());


        if let Some(ref optional_thing) = self.optional_thing {
            params.push("optional_thing".to_string());
            params.push(optional_thing.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectOfObjectsInner value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ObjectOfObjectsInner {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub required_thing: Vec<String>,
            pub optional_thing: Vec<isize>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ObjectOfObjectsInner".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "required_thing" => intermediate_rep.required_thing.push(<String as std::str::FromStr>::from_str(val).map_err(|x| format!("{}", x))?),
                    "optional_thing" => intermediate_rep.optional_thing.push(<isize as std::str::FromStr>::from_str(val).map_err(|x| format!("{}", x))?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ObjectOfObjectsInner".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ObjectOfObjectsInner {
            required_thing: intermediate_rep.required_thing.into_iter().next().ok_or("required_thing missing in ObjectOfObjectsInner".to_string())?,
            optional_thing: intermediate_rep.optional_thing.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ObjectOfObjectsInner> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectOfObjectsInner>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ObjectOfObjectsInner>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ObjectOfObjectsInner - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<ObjectOfObjectsInner> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ObjectOfObjectsInner as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into ObjectOfObjectsInner - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}

