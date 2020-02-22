#![allow(unused_imports, unused_qualifications)]

use serde::ser::Serializer;

use std::collections::HashMap;
use models;
use swagger;
use hyper::header::HeaderValue;
use std::string::ParseError;
use std::str::FromStr;
use header::IntoHeaderValue;



// Methods for converting between IntoHeaderValue<ANullableContainer> and HeaderValue

impl From<IntoHeaderValue<ANullableContainer>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<ANullableContainer>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<ANullableContainer> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(ANullableContainer::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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


/// An additionalPropertiesObject
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct AdditionalPropertiesObject(HashMap<String, String>);

impl ::std::convert::From<HashMap<String, String>> for AdditionalPropertiesObject {
    fn from(x: HashMap<String, String>) -> Self {
        AdditionalPropertiesObject(x)
    }
}


impl ::std::convert::From<AdditionalPropertiesObject> for HashMap<String, String> {
    fn from(x: AdditionalPropertiesObject) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for AdditionalPropertiesObject {
    type Target = HashMap<String, String>;
    fn deref(&self) -> &HashMap<String, String> {
        &self.0
    }
}

impl ::std::ops::DerefMut for AdditionalPropertiesObject {
    fn deref_mut(&mut self) -> &mut HashMap<String, String> {
        &mut self.0
    }
}




// Methods for converting between IntoHeaderValue<InlineObject> and HeaderValue

impl From<IntoHeaderValue<InlineObject>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<InlineObject>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<InlineObject> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(InlineObject::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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


/// An object of objects

// Methods for converting between IntoHeaderValue<ObjectOfObjects> and HeaderValue

impl From<IntoHeaderValue<ObjectOfObjects>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<ObjectOfObjects>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<ObjectOfObjects> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(ObjectOfObjects::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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



// Methods for converting between IntoHeaderValue<ObjectOfObjectsInner> and HeaderValue

impl From<IntoHeaderValue<ObjectOfObjectsInner>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<ObjectOfObjectsInner>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<ObjectOfObjectsInner> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(ObjectOfObjectsInner::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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

