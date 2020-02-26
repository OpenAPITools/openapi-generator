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

/// Converts the ANullableContainer value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for ANullableContainer {
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
impl ::std::str::FromStr for ANullableContainer {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
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
                None => return Err(())
            };

            if let Some(key) = key_result {
                match key {
                    "NullableThing" => return Err(()), // Parsing a nullable type in this style is not supported yet
                    
                    "RequiredNullableThing" => return Err(()), // Parsing a nullable type in this style is not supported yet
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(ANullableContainer {
            nullable_thing: Err(())?,
            required_nullable_thing: Err(())?,
        })
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
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Parsing additionalProperties in this style is not supported yet
        Err(())
    }
}


// Methods for converting between IntoHeaderValue<AllOfObject> and HeaderValue

impl From<IntoHeaderValue<AllOfObject>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<AllOfObject>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<AllOfObject> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(AllOfObject::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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
impl ::std::string::ToString for AllOfObject {
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
impl ::std::str::FromStr for AllOfObject {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
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
                None => return Err(())
            };

            if let Some(key) = key_result {
                match key {
                    
                    "sampleProperty" => intermediate_rep.sample_property.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "sampleBasePropery" => intermediate_rep.sample_base_propery.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(AllOfObject {
            sample_property: intermediate_rep.sample_property.into_iter().next(),
            sample_base_propery: intermediate_rep.sample_base_propery.into_iter().next(),
        })
    }
}



// Methods for converting between IntoHeaderValue<BaseAllOf> and HeaderValue

impl From<IntoHeaderValue<BaseAllOf>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<BaseAllOf>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<BaseAllOf> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(BaseAllOf::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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
impl ::std::string::ToString for BaseAllOf {
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
impl ::std::str::FromStr for BaseAllOf {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
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
                None => return Err(())
            };

            if let Some(key) = key_result {
                match key {
                    
                    "sampleBasePropery" => intermediate_rep.sample_base_propery.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(BaseAllOf {
            sample_base_propery: intermediate_rep.sample_base_propery.into_iter().next(),
        })
    }
}



/// structured response
// Methods for converting between IntoHeaderValue<GetYamlResponse> and HeaderValue

impl From<IntoHeaderValue<GetYamlResponse>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<GetYamlResponse>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<GetYamlResponse> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(GetYamlResponse::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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
impl ::std::string::ToString for GetYamlResponse {
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
impl ::std::str::FromStr for GetYamlResponse {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
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
                None => return Err(())
            };

            if let Some(key) = key_result {
                match key {
                    
                    "value" => intermediate_rep.value.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(GetYamlResponse {
            value: intermediate_rep.value.into_iter().next(),
        })
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

/// Converts the InlineObject value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for InlineObject {
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
impl ::std::str::FromStr for InlineObject {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
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
                None => return Err(())
            };

            if let Some(key) = key_result {
                match key {
                    
                    "id" => intermediate_rep.id.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "password" => intermediate_rep.password.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(InlineObject {
            id: intermediate_rep.id.into_iter().next().ok_or(())?,
            password: intermediate_rep.password.into_iter().next(),
        })
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

/// Converts the ObjectOfObjects value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for ObjectOfObjects {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];
        // Skipping inner in query parameter serialization

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectOfObjects value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for ObjectOfObjects {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
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
                None => return Err(())
            };

            if let Some(key) = key_result {
                match key {
                    
                    "inner" => intermediate_rep.inner.push(models::ObjectOfObjectsInner::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(ObjectOfObjects {
            inner: intermediate_rep.inner.into_iter().next(),
        })
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

/// Converts the ObjectOfObjectsInner value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for ObjectOfObjectsInner {
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
impl ::std::str::FromStr for ObjectOfObjectsInner {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
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
                None => return Err(())
            };

            if let Some(key) = key_result {
                match key {
                    
                    "required_thing" => intermediate_rep.required_thing.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "optional_thing" => intermediate_rep.optional_thing.push(isize::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(ObjectOfObjectsInner {
            required_thing: intermediate_rep.required_thing.into_iter().next().ok_or(())?,
            optional_thing: intermediate_rep.optional_thing.into_iter().next(),
        })
    }
}


