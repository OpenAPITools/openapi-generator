#![allow(unused_qualifications)]

use crate::models;
#[cfg(any(feature = "client", feature = "server"))]
use crate::header;

#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
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
    #[allow(clippy::new_without_default)]
    pub fn new(required_binary_field: swagger::ByteArray, ) -> MultipartRelatedRequest {
        MultipartRelatedRequest {
            object_field: None,
            optional_binary_field: None,
            required_binary_field,
        }
    }
}

/// Converts the MultipartRelatedRequest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for MultipartRelatedRequest {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            // Skipping object_field in query parameter serialization

            // Skipping optional_binary_field in query parameter serialization
            // Skipping optional_binary_field in query parameter serialization

            // Skipping required_binary_field in query parameter serialization
            // Skipping required_binary_field in query parameter serialization

        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MultipartRelatedRequest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for MultipartRelatedRequest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub object_field: Vec<models::MultipartRequestObjectField>,
            pub optional_binary_field: Vec<swagger::ByteArray>,
            pub required_binary_field: Vec<swagger::ByteArray>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing MultipartRelatedRequest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "object_field" => intermediate_rep.object_field.push(<models::MultipartRequestObjectField as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    "optional_binary_field" => return std::result::Result::Err("Parsing binary data in this style is not supported in MultipartRelatedRequest".to_string()),
                    "required_binary_field" => return std::result::Result::Err("Parsing binary data in this style is not supported in MultipartRelatedRequest".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing MultipartRelatedRequest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(MultipartRelatedRequest {
            object_field: intermediate_rep.object_field.into_iter().next(),
            optional_binary_field: intermediate_rep.optional_binary_field.into_iter().next(),
            required_binary_field: intermediate_rep.required_binary_field.into_iter().next().ok_or_else(|| "required_binary_field missing in MultipartRelatedRequest".to_string())?,
        })
    }
}

// Methods for converting between header::IntoHeaderValue<MultipartRelatedRequest> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<MultipartRelatedRequest>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<MultipartRelatedRequest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for MultipartRelatedRequest - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<MultipartRelatedRequest> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <MultipartRelatedRequest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into MultipartRelatedRequest - {}",
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
pub struct MultipartRequestObjectField {
    #[serde(rename = "field_a")]
    pub field_a: String,

    #[serde(rename = "field_b")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub field_b: Option<Vec<String>>,

}

impl MultipartRequestObjectField {
    #[allow(clippy::new_without_default)]
    pub fn new(field_a: String, ) -> MultipartRequestObjectField {
        MultipartRequestObjectField {
            field_a,
            field_b: None,
        }
    }
}

/// Converts the MultipartRequestObjectField value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for MultipartRequestObjectField {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![

            Some("field_a".to_string()),
            Some(self.field_a.to_string()),


            self.field_b.as_ref().map(|field_b| {
                vec![
                    "field_b".to_string(),
                    field_b.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","),
                ].join(",")
            }),

        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MultipartRequestObjectField value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for MultipartRequestObjectField {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub field_a: Vec<String>,
            pub field_b: Vec<Vec<String>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing MultipartRequestObjectField".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "field_a" => intermediate_rep.field_a.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    "field_b" => return std::result::Result::Err("Parsing a container in this style is not supported in MultipartRequestObjectField".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing MultipartRequestObjectField".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(MultipartRequestObjectField {
            field_a: intermediate_rep.field_a.into_iter().next().ok_or_else(|| "field_a missing in MultipartRequestObjectField".to_string())?,
            field_b: intermediate_rep.field_b.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<MultipartRequestObjectField> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<MultipartRequestObjectField>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<MultipartRequestObjectField>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for MultipartRequestObjectField - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<MultipartRequestObjectField> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <MultipartRequestObjectField as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into MultipartRequestObjectField - {}",
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
pub struct MultipleIdenticalMimeTypesPostRequest {
    #[serde(rename = "binary1")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub binary1: Option<swagger::ByteArray>,

    #[serde(rename = "binary2")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub binary2: Option<swagger::ByteArray>,

}

impl MultipleIdenticalMimeTypesPostRequest {
    #[allow(clippy::new_without_default)]
    pub fn new() -> MultipleIdenticalMimeTypesPostRequest {
        MultipleIdenticalMimeTypesPostRequest {
            binary1: None,
            binary2: None,
        }
    }
}

/// Converts the MultipleIdenticalMimeTypesPostRequest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::string::ToString for MultipleIdenticalMimeTypesPostRequest {
    fn to_string(&self) -> String {
        let params: Vec<Option<String>> = vec![
            // Skipping binary1 in query parameter serialization
            // Skipping binary1 in query parameter serialization

            // Skipping binary2 in query parameter serialization
            // Skipping binary2 in query parameter serialization

        ];

        params.into_iter().flatten().collect::<Vec<_>>().join(",")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MultipleIdenticalMimeTypesPostRequest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for MultipleIdenticalMimeTypesPostRequest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub binary1: Vec<swagger::ByteArray>,
            pub binary2: Vec<swagger::ByteArray>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing MultipleIdenticalMimeTypesPostRequest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "binary1" => return std::result::Result::Err("Parsing binary data in this style is not supported in MultipleIdenticalMimeTypesPostRequest".to_string()),
                    "binary2" => return std::result::Result::Err("Parsing binary data in this style is not supported in MultipleIdenticalMimeTypesPostRequest".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing MultipleIdenticalMimeTypesPostRequest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(MultipleIdenticalMimeTypesPostRequest {
            binary1: intermediate_rep.binary1.into_iter().next(),
            binary2: intermediate_rep.binary2.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<MultipleIdenticalMimeTypesPostRequest> and hyper::header::HeaderValue

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<header::IntoHeaderValue<MultipleIdenticalMimeTypesPostRequest>> for hyper::header::HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<MultipleIdenticalMimeTypesPostRequest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match hyper::header::HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for MultipleIdenticalMimeTypesPostRequest - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(any(feature = "client", feature = "server"))]
impl std::convert::TryFrom<hyper::header::HeaderValue> for header::IntoHeaderValue<MultipleIdenticalMimeTypesPostRequest> {
    type Error = String;

    fn try_from(hdr_value: hyper::header::HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <MultipleIdenticalMimeTypesPostRequest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into MultipleIdenticalMimeTypesPostRequest - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}

