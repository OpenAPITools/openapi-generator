#![allow(unused_imports, unused_qualifications)]

use serde::ser::Serializer;

use std::collections::HashMap;
use models;
use swagger;
use hyper::header::HeaderValue;
use std::string::ParseError;
use std::str::FromStr;
use header::IntoHeaderValue;


// Methods for converting between IntoHeaderValue<MultipartRelatedRequest> and HeaderValue

impl From<IntoHeaderValue<MultipartRelatedRequest>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<MultipartRelatedRequest>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<MultipartRelatedRequest> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(MultipartRelatedRequest::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct MultipartRelatedRequest {
    #[serde(rename = "object_field")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub object_field: Option<models::MultipartRequestObjectField>,

    #[serde(rename = "optional_binary_field")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub optional_binary_field: Option<swagger::ByteArray>,

    #[serde(rename = "required_binary_field")]
    pub required_binary_field: swagger::ByteArray,

}

impl MultipartRelatedRequest {
    pub fn new(required_binary_field: swagger::ByteArray, ) -> MultipartRelatedRequest {
        MultipartRelatedRequest {
            object_field: None,
            optional_binary_field: None,
            required_binary_field: required_binary_field,
        }
    }
}

/// Converts the MultipartRelatedRequest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for MultipartRelatedRequest {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];
        // Skipping object_field in query parameter serialization

        // Skipping optional_binary_field in query parameter serialization
        // Skipping optional_binary_field in query parameter serialization

        // Skipping required_binary_field in query parameter serialization
        // Skipping required_binary_field in query parameter serialization

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MultipartRelatedRequest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for MultipartRelatedRequest {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub object_field: Vec<models::MultipartRequestObjectField>,
            pub optional_binary_field: Vec<swagger::ByteArray>,
            pub required_binary_field: Vec<swagger::ByteArray>,
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
                    
                    "object_field" => intermediate_rep.object_field.push(models::MultipartRequestObjectField::from_str(val).map_err(|x| ())?),
                    
                    "optional_binary_field" => return Err(()), // Parsing binary data in this style is not supported yet
                    
                    "required_binary_field" => return Err(()), // Parsing binary data in this style is not supported yet
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(MultipartRelatedRequest {
            object_field: intermediate_rep.object_field.into_iter().next(),
            optional_binary_field: intermediate_rep.optional_binary_field.into_iter().next(),
            required_binary_field: intermediate_rep.required_binary_field.into_iter().next().ok_or(())?,
        })
    }
}



// Methods for converting between IntoHeaderValue<MultipartRequest> and HeaderValue

impl From<IntoHeaderValue<MultipartRequest>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<MultipartRequest>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<MultipartRequest> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(MultipartRequest::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct MultipartRequest {
    #[serde(rename = "string_field")]
    pub string_field: String,

    #[serde(rename = "optional_string_field")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub optional_string_field: Option<String>,

    #[serde(rename = "object_field")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub object_field: Option<models::MultipartRequestObjectField>,

    #[serde(rename = "binary_field")]
    pub binary_field: swagger::ByteArray,

}

impl MultipartRequest {
    pub fn new(string_field: String, binary_field: swagger::ByteArray, ) -> MultipartRequest {
        MultipartRequest {
            string_field: string_field,
            optional_string_field: None,
            object_field: None,
            binary_field: binary_field,
        }
    }
}

/// Converts the MultipartRequest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for MultipartRequest {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        params.push("string_field".to_string());
        params.push(self.string_field.to_string());


        if let Some(ref optional_string_field) = self.optional_string_field {
            params.push("optional_string_field".to_string());
            params.push(optional_string_field.to_string());
        }

        // Skipping object_field in query parameter serialization

        // Skipping binary_field in query parameter serialization
        // Skipping binary_field in query parameter serialization

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MultipartRequest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for MultipartRequest {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub string_field: Vec<String>,
            pub optional_string_field: Vec<String>,
            pub object_field: Vec<models::MultipartRequestObjectField>,
            pub binary_field: Vec<swagger::ByteArray>,
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
                    
                    "string_field" => intermediate_rep.string_field.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "optional_string_field" => intermediate_rep.optional_string_field.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "object_field" => intermediate_rep.object_field.push(models::MultipartRequestObjectField::from_str(val).map_err(|x| ())?),
                    
                    "binary_field" => return Err(()), // Parsing binary data in this style is not supported yet
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(MultipartRequest {
            string_field: intermediate_rep.string_field.into_iter().next().ok_or(())?,
            optional_string_field: intermediate_rep.optional_string_field.into_iter().next(),
            object_field: intermediate_rep.object_field.into_iter().next(),
            binary_field: intermediate_rep.binary_field.into_iter().next().ok_or(())?,
        })
    }
}



// Methods for converting between IntoHeaderValue<MultipartRequestObjectField> and HeaderValue

impl From<IntoHeaderValue<MultipartRequestObjectField>> for HeaderValue {
    fn from(hdr_value: IntoHeaderValue<MultipartRequestObjectField>) -> Self {
        HeaderValue::from_str(&hdr_value.to_string()).unwrap()
    }
}

impl From<HeaderValue> for IntoHeaderValue<MultipartRequestObjectField> {
    fn from(hdr_value: HeaderValue) -> Self {
        IntoHeaderValue(MultipartRequestObjectField::from_str(hdr_value.to_str().unwrap()).unwrap())
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct MultipartRequestObjectField {
    #[serde(rename = "field_a")]
    pub field_a: String,

    #[serde(rename = "field_b")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub field_b: Option<Vec<String>>,

}

impl MultipartRequestObjectField {
    pub fn new(field_a: String, ) -> MultipartRequestObjectField {
        MultipartRequestObjectField {
            field_a: field_a,
            field_b: None,
        }
    }
}

/// Converts the MultipartRequestObjectField value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for MultipartRequestObjectField {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        params.push("field_a".to_string());
        params.push(self.field_a.to_string());


        if let Some(ref field_b) = self.field_b {
            params.push("field_b".to_string());
            params.push(field_b.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",").to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MultipartRequestObjectField value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for MultipartRequestObjectField {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub field_a: Vec<String>,
            pub field_b: Vec<Vec<String>>,
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
                    
                    "field_a" => intermediate_rep.field_a.push(String::from_str(val).map_err(|x| ())?),
                    
                    "field_b" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(MultipartRequestObjectField {
            field_a: intermediate_rep.field_a.into_iter().next().ok_or(())?,
            field_b: intermediate_rep.field_b.into_iter().next(),
        })
    }
}


