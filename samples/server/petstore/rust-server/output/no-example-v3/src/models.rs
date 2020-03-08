#![allow(unused_imports, unused_qualifications)]

use serde::ser::Serializer;

use std::collections::HashMap;
use models;
use swagger;
use hyper::header::HeaderValue;
use std::string::ParseError;
use std::str::FromStr;
use header::IntoHeaderValue;


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
    #[serde(rename = "propery")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub propery: Option<String>,

}

impl InlineObject {
    pub fn new() -> InlineObject {
        InlineObject {
            propery: None,
        }
    }
}

/// Converts the InlineObject value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for InlineObject {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref propery) = self.propery {
            params.push("propery".to_string());
            params.push(propery.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a InlineObject value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for InlineObject {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub propery: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return Err("Missing value while parsing InlineObject".to_string())
            };

            if let Some(key) = key_result {
                match key {
                    "propery" => intermediate_rep.propery.push(String::from_str(val).map_err(|x| format!("{}", x))?),
                    _ => return Err("Unexpected key while parsing InlineObject".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(InlineObject {
            propery: intermediate_rep.propery.into_iter().next(),
        })
    }
}


