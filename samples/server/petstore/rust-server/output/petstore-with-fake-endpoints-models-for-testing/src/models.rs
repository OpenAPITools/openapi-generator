#![allow(unused_imports, unused_qualifications)]

use serde_xml_rs;
use serde::ser::Serializer;

use std::collections::{HashMap, BTreeMap};
use models;
use swagger;
use std::string::ParseError;
use uuid;



#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct AdditionalPropertiesClass {
    #[serde(rename = "map_property")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map_property: Option<HashMap<String, String>>,

    #[serde(rename = "map_of_map_property")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map_of_map_property: Option<HashMap<String, HashMap<String, String>>>,

}

impl AdditionalPropertiesClass {
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
impl ::std::string::ToString for AdditionalPropertiesClass {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];
        // Skipping map_property in query parameter serialization

        // Skipping map_of_map_property in query parameter serialization
        // Skipping map_of_map_property in query parameter serialization

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AdditionalPropertiesClass value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for AdditionalPropertiesClass {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub map_property: Vec<HashMap<String, String>>,
            pub map_of_map_property: Vec<HashMap<String, HashMap<String, String>>>,
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
                    "map_property" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    "map_of_map_property" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(AdditionalPropertiesClass {
            map_property: intermediate_rep.map_property.into_iter().next(),
            map_of_map_property: intermediate_rep.map_of_map_property.into_iter().next(),
        })
    }
}


impl AdditionalPropertiesClass {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct Animal {
    #[serde(rename = "className")]
    pub class_name: String,

    #[serde(rename = "color")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub color: Option<String>,

}

impl Animal {
    pub fn new(class_name: String, ) -> Animal {
        Animal {
            class_name: class_name,
            color: Some("red".to_string()),
        }
    }
}

/// Converts the Animal value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for Animal {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        params.push("className".to_string());
        params.push(self.class_name.to_string());


        if let Some(ref color) = self.color {
            params.push("color".to_string());
            params.push(color.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Animal value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for Animal {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub class_name: Vec<String>,
            pub color: Vec<String>,
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
                    
                    "className" => intermediate_rep.class_name.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "color" => intermediate_rep.color.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(Animal {
            class_name: intermediate_rep.class_name.into_iter().next().ok_or(())?,
            color: intermediate_rep.color.into_iter().next(),
        })
    }
}


impl Animal {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct AnimalFarm(Vec<Animal>);

impl ::std::convert::From<Vec<Animal>> for AnimalFarm {
    fn from(x: Vec<Animal>) -> Self {
        AnimalFarm(x)
    }
}

impl ::std::convert::From<AnimalFarm> for Vec<Animal> {
    fn from(x: AnimalFarm) -> Self {
        x.0
    }
}

impl ::std::iter::FromIterator<Animal> for AnimalFarm {
    fn from_iter<U: IntoIterator<Item=Animal>>(u: U) -> Self {
        AnimalFarm(Vec::<Animal>::from_iter(u))
    }
}

impl ::std::iter::IntoIterator for AnimalFarm {
    type Item = Animal;
    type IntoIter = ::std::vec::IntoIter<Animal>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> ::std::iter::IntoIterator for &'a AnimalFarm {
    type Item = &'a Animal;
    type IntoIter = ::std::slice::Iter<'a, Animal>;

    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

impl<'a> ::std::iter::IntoIterator for &'a mut AnimalFarm {
    type Item = &'a mut Animal;
    type IntoIter = ::std::slice::IterMut<'a, Animal>;

    fn into_iter(self) -> Self::IntoIter {
        (&mut self.0).into_iter()
    }
}

impl ::std::ops::Deref for AnimalFarm {
    type Target = Vec<Animal>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl ::std::ops::DerefMut for AnimalFarm {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Converts the AnimalFarm value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for AnimalFarm {
    fn to_string(&self) -> String {
        self.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnimalFarm value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for AnimalFarm {
    type Err = <Animal as ::std::str::FromStr>::Err;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut items = vec![];
        for item in s.split(',')
        {
            items.push(item.parse()?);
        }
        Ok(AnimalFarm(items))
    }
}


impl AnimalFarm {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct ApiResponse {
    #[serde(rename = "code")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub code: Option<i32>,

    #[serde(rename = "type")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub _type: Option<String>,

    #[serde(rename = "message")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub message: Option<String>,

}

impl ApiResponse {
    pub fn new() -> ApiResponse {
        ApiResponse {
            code: None,
            _type: None,
            message: None,
        }
    }
}

/// Converts the ApiResponse value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for ApiResponse {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref code) = self.code {
            params.push("code".to_string());
            params.push(code.to_string());
        }


        if let Some(ref _type) = self._type {
            params.push("type".to_string());
            params.push(_type.to_string());
        }


        if let Some(ref message) = self.message {
            params.push("message".to_string());
            params.push(message.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ApiResponse value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for ApiResponse {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub code: Vec<i32>,
            pub _type: Vec<String>,
            pub message: Vec<String>,
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
                    
                    "code" => intermediate_rep.code.push(i32::from_str(val).map_err(|x| ())?),
                    
                    
                    "type" => intermediate_rep._type.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "message" => intermediate_rep.message.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(ApiResponse {
            code: intermediate_rep.code.into_iter().next(),
            _type: intermediate_rep._type.into_iter().next(),
            message: intermediate_rep.message.into_iter().next(),
        })
    }
}


impl ApiResponse {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct ArrayOfArrayOfNumberOnly {
    #[serde(rename = "ArrayArrayNumber")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_array_number: Option<Vec<Vec<f64>>>,

}

impl ArrayOfArrayOfNumberOnly {
    pub fn new() -> ArrayOfArrayOfNumberOnly {
        ArrayOfArrayOfNumberOnly {
            array_array_number: None,
        }
    }
}

/// Converts the ArrayOfArrayOfNumberOnly value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for ArrayOfArrayOfNumberOnly {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];
        // Skipping ArrayArrayNumber in query parameter serialization

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ArrayOfArrayOfNumberOnly value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for ArrayOfArrayOfNumberOnly {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub array_array_number: Vec<Vec<Vec<f64>>>,
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
                    "ArrayArrayNumber" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(ArrayOfArrayOfNumberOnly {
            array_array_number: intermediate_rep.array_array_number.into_iter().next(),
        })
    }
}


impl ArrayOfArrayOfNumberOnly {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct ArrayOfNumberOnly {
    #[serde(rename = "ArrayNumber")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_number: Option<Vec<f64>>,

}

impl ArrayOfNumberOnly {
    pub fn new() -> ArrayOfNumberOnly {
        ArrayOfNumberOnly {
            array_number: None,
        }
    }
}

/// Converts the ArrayOfNumberOnly value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for ArrayOfNumberOnly {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref array_number) = self.array_number {
            params.push("ArrayNumber".to_string());
            params.push(array_number.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",").to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ArrayOfNumberOnly value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for ArrayOfNumberOnly {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub array_number: Vec<Vec<f64>>,
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
                    "ArrayNumber" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(ArrayOfNumberOnly {
            array_number: intermediate_rep.array_number.into_iter().next(),
        })
    }
}


impl ArrayOfNumberOnly {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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

    // Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "array_of_enum")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_of_enum: Option<Vec<String>>,

}

impl ArrayTest {
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
impl ::std::string::ToString for ArrayTest {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref array_of_string) = self.array_of_string {
            params.push("array_of_string".to_string());
            params.push(array_of_string.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",").to_string());
        }

        // Skipping array_array_of_integer in query parameter serialization

        // Skipping array_array_of_model in query parameter serialization


        if let Some(ref array_of_enum) = self.array_of_enum {
            params.push("array_of_enum".to_string());
            params.push(array_of_enum.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",").to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ArrayTest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for ArrayTest {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub array_of_string: Vec<Vec<String>>,
            pub array_array_of_integer: Vec<Vec<Vec<i64>>>,
            pub array_array_of_model: Vec<Vec<Vec<models::ReadOnlyFirst>>>,
            pub array_of_enum: Vec<Vec<String>>,
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
                    "array_of_string" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    "array_array_of_integer" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    "array_array_of_model" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    "array_of_enum" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(ArrayTest {
            array_of_string: intermediate_rep.array_of_string.into_iter().next(),
            array_array_of_integer: intermediate_rep.array_array_of_integer.into_iter().next(),
            array_array_of_model: intermediate_rep.array_array_of_model.into_iter().next(),
            array_of_enum: intermediate_rep.array_of_enum.into_iter().next(),
        })
    }
}


impl ArrayTest {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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
impl ::std::string::ToString for Capitalization {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref small_camel) = self.small_camel {
            params.push("smallCamel".to_string());
            params.push(small_camel.to_string());
        }


        if let Some(ref capital_camel) = self.capital_camel {
            params.push("CapitalCamel".to_string());
            params.push(capital_camel.to_string());
        }


        if let Some(ref small_snake) = self.small_snake {
            params.push("small_Snake".to_string());
            params.push(small_snake.to_string());
        }


        if let Some(ref capital_snake) = self.capital_snake {
            params.push("Capital_Snake".to_string());
            params.push(capital_snake.to_string());
        }


        if let Some(ref sca_eth_flow_points) = self.sca_eth_flow_points {
            params.push("SCA_ETH_Flow_Points".to_string());
            params.push(sca_eth_flow_points.to_string());
        }


        if let Some(ref att_name) = self.att_name {
            params.push("ATT_NAME".to_string());
            params.push(att_name.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Capitalization value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for Capitalization {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
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
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return Err(())
            };

            if let Some(key) = key_result {
                match key {
                    
                    "smallCamel" => intermediate_rep.small_camel.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "CapitalCamel" => intermediate_rep.capital_camel.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "small_Snake" => intermediate_rep.small_snake.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "Capital_Snake" => intermediate_rep.capital_snake.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "SCA_ETH_Flow_Points" => intermediate_rep.sca_eth_flow_points.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "ATT_NAME" => intermediate_rep.att_name.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(Capitalization {
            small_camel: intermediate_rep.small_camel.into_iter().next(),
            capital_camel: intermediate_rep.capital_camel.into_iter().next(),
            small_snake: intermediate_rep.small_snake.into_iter().next(),
            capital_snake: intermediate_rep.capital_snake.into_iter().next(),
            sca_eth_flow_points: intermediate_rep.sca_eth_flow_points.into_iter().next(),
            att_name: intermediate_rep.att_name.into_iter().next(),
        })
    }
}


impl Capitalization {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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
    pub fn new(class_name: String, ) -> Cat {
        Cat {
            class_name: class_name,
            color: Some("red".to_string()),
            declawed: None,
        }
    }
}

/// Converts the Cat value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for Cat {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        params.push("className".to_string());
        params.push(self.class_name.to_string());


        if let Some(ref color) = self.color {
            params.push("color".to_string());
            params.push(color.to_string());
        }


        if let Some(ref declawed) = self.declawed {
            params.push("declawed".to_string());
            params.push(declawed.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Cat value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for Cat {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub class_name: Vec<String>,
            pub color: Vec<String>,
            pub declawed: Vec<bool>,
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
                    
                    "className" => intermediate_rep.class_name.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "color" => intermediate_rep.color.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "declawed" => intermediate_rep.declawed.push(bool::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(Cat {
            class_name: intermediate_rep.class_name.into_iter().next().ok_or(())?,
            color: intermediate_rep.color.into_iter().next(),
            declawed: intermediate_rep.declawed.into_iter().next(),
        })
    }
}


impl Cat {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct CatAllOf {
    #[serde(rename = "declawed")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub declawed: Option<bool>,

}

impl CatAllOf {
    pub fn new() -> CatAllOf {
        CatAllOf {
            declawed: None,
        }
    }
}

/// Converts the CatAllOf value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for CatAllOf {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref declawed) = self.declawed {
            params.push("declawed".to_string());
            params.push(declawed.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a CatAllOf value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for CatAllOf {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub declawed: Vec<bool>,
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
                    
                    "declawed" => intermediate_rep.declawed.push(bool::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(CatAllOf {
            declawed: intermediate_rep.declawed.into_iter().next(),
        })
    }
}


impl CatAllOf {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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
impl ::std::string::ToString for Category {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref id) = self.id {
            params.push("id".to_string());
            params.push(id.to_string());
        }


        if let Some(ref name) = self.name {
            params.push("name".to_string());
            params.push(name.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Category value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for Category {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub id: Vec<i64>,
            pub name: Vec<String>,
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
                    
                    "id" => intermediate_rep.id.push(i64::from_str(val).map_err(|x| ())?),
                    
                    
                    "name" => intermediate_rep.name.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(Category {
            id: intermediate_rep.id.into_iter().next(),
            name: intermediate_rep.name.into_iter().next(),
        })
    }
}


impl Category {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Model for testing model with \"_class\" property

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct ClassModel {
    #[serde(rename = "_class")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub _class: Option<String>,

}

impl ClassModel {
    pub fn new() -> ClassModel {
        ClassModel {
            _class: None,
        }
    }
}

/// Converts the ClassModel value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for ClassModel {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref _class) = self._class {
            params.push("_class".to_string());
            params.push(_class.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ClassModel value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for ClassModel {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub _class: Vec<String>,
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
                    
                    "_class" => intermediate_rep._class.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(ClassModel {
            _class: intermediate_rep._class.into_iter().next(),
        })
    }
}


impl ClassModel {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct Client {
    #[serde(rename = "client")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub client: Option<String>,

}

impl Client {
    pub fn new() -> Client {
        Client {
            client: None,
        }
    }
}

/// Converts the Client value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for Client {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref client) = self.client {
            params.push("client".to_string());
            params.push(client.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Client value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for Client {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub client: Vec<String>,
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
                    
                    "client" => intermediate_rep.client.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(Client {
            client: intermediate_rep.client.into_iter().next(),
        })
    }
}


impl Client {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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
    pub fn new(class_name: String, ) -> Dog {
        Dog {
            class_name: class_name,
            color: Some("red".to_string()),
            breed: None,
        }
    }
}

/// Converts the Dog value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for Dog {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        params.push("className".to_string());
        params.push(self.class_name.to_string());


        if let Some(ref color) = self.color {
            params.push("color".to_string());
            params.push(color.to_string());
        }


        if let Some(ref breed) = self.breed {
            params.push("breed".to_string());
            params.push(breed.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Dog value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for Dog {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub class_name: Vec<String>,
            pub color: Vec<String>,
            pub breed: Vec<String>,
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
                    
                    "className" => intermediate_rep.class_name.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "color" => intermediate_rep.color.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "breed" => intermediate_rep.breed.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(Dog {
            class_name: intermediate_rep.class_name.into_iter().next().ok_or(())?,
            color: intermediate_rep.color.into_iter().next(),
            breed: intermediate_rep.breed.into_iter().next(),
        })
    }
}


impl Dog {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct DogAllOf {
    #[serde(rename = "breed")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub breed: Option<String>,

}

impl DogAllOf {
    pub fn new() -> DogAllOf {
        DogAllOf {
            breed: None,
        }
    }
}

/// Converts the DogAllOf value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for DogAllOf {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref breed) = self.breed {
            params.push("breed".to_string());
            params.push(breed.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a DogAllOf value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for DogAllOf {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub breed: Vec<String>,
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
                    
                    "breed" => intermediate_rep.breed.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(DogAllOf {
            breed: intermediate_rep.breed.into_iter().next(),
        })
    }
}


impl DogAllOf {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct EnumArrays {
    // Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "just_symbol")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub just_symbol: Option<String>,

    // Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "array_enum")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_enum: Option<Vec<String>>,

    // Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "array_array_enum")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_array_enum: Option<Vec<Vec<String>>>,

}

impl EnumArrays {
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
impl ::std::string::ToString for EnumArrays {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref just_symbol) = self.just_symbol {
            params.push("just_symbol".to_string());
            params.push(just_symbol.to_string());
        }


        if let Some(ref array_enum) = self.array_enum {
            params.push("array_enum".to_string());
            params.push(array_enum.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",").to_string());
        }

        // Skipping array_array_enum in query parameter serialization

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a EnumArrays value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for EnumArrays {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub just_symbol: Vec<String>,
            pub array_enum: Vec<Vec<String>>,
            pub array_array_enum: Vec<Vec<Vec<String>>>,
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
                    
                    "just_symbol" => intermediate_rep.just_symbol.push(String::from_str(val).map_err(|x| ())?),
                    
                    "array_enum" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    "array_array_enum" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(EnumArrays {
            just_symbol: intermediate_rep.just_symbol.into_iter().next(),
            array_enum: intermediate_rep.array_enum.into_iter().next(),
            array_array_enum: intermediate_rep.array_array_enum.into_iter().next(),
        })
    }
}


impl EnumArrays {
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
pub enum EnumClass { 
    #[serde(rename = "_abc")]
    _ABC,
    #[serde(rename = "-efg")]
    _EFG,
    #[serde(rename = "(xyz)")]
    _XYZ_,
}

impl ::std::fmt::Display for EnumClass {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self { 
            EnumClass::_ABC => write!(f, "{}", "_abc"),
            EnumClass::_EFG => write!(f, "{}", "-efg"),
            EnumClass::_XYZ_ => write!(f, "{}", "(xyz)"),
        }
    }
}

impl ::std::str::FromStr for EnumClass {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "_abc" => Ok(EnumClass::_ABC),
            "-efg" => Ok(EnumClass::_EFG),
            "(xyz)" => Ok(EnumClass::_XYZ_),
            _ => Err(()),
        }
    }
}

impl EnumClass {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct EnumTest {
    // Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "enum_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub enum_string: Option<String>,

    // Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "enum_string_required")]
    pub enum_string_required: String,

    // Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "enum_integer")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub enum_integer: Option<i32>,

    // Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "enum_number")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub enum_number: Option<f64>,

    #[serde(rename = "outerEnum")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub outer_enum: Option<models::OuterEnum>,

}

impl EnumTest {
    pub fn new(enum_string_required: String, ) -> EnumTest {
        EnumTest {
            enum_string: None,
            enum_string_required: enum_string_required,
            enum_integer: None,
            enum_number: None,
            outer_enum: None,
        }
    }
}

/// Converts the EnumTest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for EnumTest {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref enum_string) = self.enum_string {
            params.push("enum_string".to_string());
            params.push(enum_string.to_string());
        }


        params.push("enum_string_required".to_string());
        params.push(self.enum_string_required.to_string());


        if let Some(ref enum_integer) = self.enum_integer {
            params.push("enum_integer".to_string());
            params.push(enum_integer.to_string());
        }


        if let Some(ref enum_number) = self.enum_number {
            params.push("enum_number".to_string());
            params.push(enum_number.to_string());
        }

        // Skipping outerEnum in query parameter serialization

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a EnumTest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for EnumTest {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub enum_string: Vec<String>,
            pub enum_string_required: Vec<String>,
            pub enum_integer: Vec<i32>,
            pub enum_number: Vec<f64>,
            pub outer_enum: Vec<models::OuterEnum>,
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
                    
                    "enum_string" => intermediate_rep.enum_string.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "enum_string_required" => intermediate_rep.enum_string_required.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "enum_integer" => intermediate_rep.enum_integer.push(i32::from_str(val).map_err(|x| ())?),
                    
                    
                    "enum_number" => intermediate_rep.enum_number.push(f64::from_str(val).map_err(|x| ())?),
                    
                    
                    "outerEnum" => intermediate_rep.outer_enum.push(models::OuterEnum::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(EnumTest {
            enum_string: intermediate_rep.enum_string.into_iter().next(),
            enum_string_required: intermediate_rep.enum_string_required.into_iter().next().ok_or(())?,
            enum_integer: intermediate_rep.enum_integer.into_iter().next(),
            enum_number: intermediate_rep.enum_number.into_iter().next(),
            outer_enum: intermediate_rep.outer_enum.into_iter().next(),
        })
    }
}


impl EnumTest {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct FormatTest {
    #[serde(rename = "integer")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub integer: Option<u8>,

    #[serde(rename = "int32")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub int32: Option<u32>,

    #[serde(rename = "int64")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub int64: Option<i64>,

    #[serde(rename = "number")]
    pub number: f64,

    #[serde(rename = "float")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub float: Option<f32>,

    #[serde(rename = "double")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub double: Option<f64>,

    #[serde(rename = "string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub string: Option<String>,

    #[serde(rename = "byte")]
    pub byte: swagger::ByteArray,

    #[serde(rename = "binary")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub binary: Option<swagger::ByteArray>,

    #[serde(rename = "date")]
    pub date: chrono::DateTime::<chrono::Utc>,

    #[serde(rename = "dateTime")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub date_time: Option<chrono::DateTime::<chrono::Utc>>,

    #[serde(rename = "uuid")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub uuid: Option<uuid::Uuid>,

    #[serde(rename = "password")]
    pub password: String,

}

impl FormatTest {
    pub fn new(number: f64, byte: swagger::ByteArray, date: chrono::DateTime::<chrono::Utc>, password: String, ) -> FormatTest {
        FormatTest {
            integer: None,
            int32: None,
            int64: None,
            number: number,
            float: None,
            double: None,
            string: None,
            byte: byte,
            binary: None,
            date: date,
            date_time: None,
            uuid: None,
            password: password,
        }
    }
}

/// Converts the FormatTest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for FormatTest {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref integer) = self.integer {
            params.push("integer".to_string());
            params.push(integer.to_string());
        }


        if let Some(ref int32) = self.int32 {
            params.push("int32".to_string());
            params.push(int32.to_string());
        }


        if let Some(ref int64) = self.int64 {
            params.push("int64".to_string());
            params.push(int64.to_string());
        }


        params.push("number".to_string());
        params.push(self.number.to_string());


        if let Some(ref float) = self.float {
            params.push("float".to_string());
            params.push(float.to_string());
        }


        if let Some(ref double) = self.double {
            params.push("double".to_string());
            params.push(double.to_string());
        }


        if let Some(ref string) = self.string {
            params.push("string".to_string());
            params.push(string.to_string());
        }

        // Skipping byte in query parameter serialization
        // Skipping byte in query parameter serialization

        // Skipping binary in query parameter serialization
        // Skipping binary in query parameter serialization

        // Skipping date in query parameter serialization

        // Skipping dateTime in query parameter serialization

        // Skipping uuid in query parameter serialization


        params.push("password".to_string());
        params.push(self.password.to_string());

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a FormatTest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for FormatTest {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
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
            pub date: Vec<chrono::DateTime::<chrono::Utc>>,
            pub date_time: Vec<chrono::DateTime::<chrono::Utc>>,
            pub uuid: Vec<uuid::Uuid>,
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
                    
                    "integer" => intermediate_rep.integer.push(u8::from_str(val).map_err(|x| ())?),
                    
                    
                    "int32" => intermediate_rep.int32.push(u32::from_str(val).map_err(|x| ())?),
                    
                    
                    "int64" => intermediate_rep.int64.push(i64::from_str(val).map_err(|x| ())?),
                    
                    
                    "number" => intermediate_rep.number.push(f64::from_str(val).map_err(|x| ())?),
                    
                    
                    "float" => intermediate_rep.float.push(f32::from_str(val).map_err(|x| ())?),
                    
                    
                    "double" => intermediate_rep.double.push(f64::from_str(val).map_err(|x| ())?),
                    
                    
                    "string" => intermediate_rep.string.push(String::from_str(val).map_err(|x| ())?),
                    
                    "byte" => return Err(()), // Parsing binary data in this style is not supported yet
                    
                    "binary" => return Err(()), // Parsing binary data in this style is not supported yet
                    
                    
                    "date" => intermediate_rep.date.push(chrono::DateTime::<chrono::Utc>::from_str(val).map_err(|x| ())?),
                    
                    
                    "dateTime" => intermediate_rep.date_time.push(chrono::DateTime::<chrono::Utc>::from_str(val).map_err(|x| ())?),
                    
                    
                    "uuid" => intermediate_rep.uuid.push(uuid::Uuid::from_str(val).map_err(|x| ())?),
                    
                    
                    "password" => intermediate_rep.password.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(FormatTest {
            integer: intermediate_rep.integer.into_iter().next(),
            int32: intermediate_rep.int32.into_iter().next(),
            int64: intermediate_rep.int64.into_iter().next(),
            number: intermediate_rep.number.into_iter().next().ok_or(())?,
            float: intermediate_rep.float.into_iter().next(),
            double: intermediate_rep.double.into_iter().next(),
            string: intermediate_rep.string.into_iter().next(),
            byte: intermediate_rep.byte.into_iter().next().ok_or(())?,
            binary: intermediate_rep.binary.into_iter().next(),
            date: intermediate_rep.date.into_iter().next().ok_or(())?,
            date_time: intermediate_rep.date_time.into_iter().next(),
            uuid: intermediate_rep.uuid.into_iter().next(),
            password: intermediate_rep.password.into_iter().next().ok_or(())?,
        })
    }
}


impl FormatTest {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct HasOnlyReadOnly {
    #[serde(rename = "bar")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub bar: Option<String>,

    #[serde(rename = "foo")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub foo: Option<String>,

}

impl HasOnlyReadOnly {
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
impl ::std::string::ToString for HasOnlyReadOnly {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref bar) = self.bar {
            params.push("bar".to_string());
            params.push(bar.to_string());
        }


        if let Some(ref foo) = self.foo {
            params.push("foo".to_string());
            params.push(foo.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a HasOnlyReadOnly value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for HasOnlyReadOnly {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub bar: Vec<String>,
            pub foo: Vec<String>,
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
                    
                    "bar" => intermediate_rep.bar.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "foo" => intermediate_rep.foo.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(HasOnlyReadOnly {
            bar: intermediate_rep.bar.into_iter().next(),
            foo: intermediate_rep.foo.into_iter().next(),
        })
    }
}


impl HasOnlyReadOnly {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct List {
    #[serde(rename = "123-list")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub _123_list: Option<String>,

}

impl List {
    pub fn new() -> List {
        List {
            _123_list: None,
        }
    }
}

/// Converts the List value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for List {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref _123_list) = self._123_list {
            params.push("123-list".to_string());
            params.push(_123_list.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a List value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for List {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub _123_list: Vec<String>,
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
                    
                    "123-list" => intermediate_rep._123_list.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(List {
            _123_list: intermediate_rep._123_list.into_iter().next(),
        })
    }
}


impl List {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct MapTest {
    #[serde(rename = "map_map_of_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map_map_of_string: Option<HashMap<String, HashMap<String, String>>>,

    // Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "map_map_of_enum")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map_map_of_enum: Option<HashMap<String, HashMap<String, String>>>,

    // Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "map_of_enum_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map_of_enum_string: Option<HashMap<String, String>>,

}

impl MapTest {
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
impl ::std::string::ToString for MapTest {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];
        // Skipping map_map_of_string in query parameter serialization
        // Skipping map_map_of_string in query parameter serialization

        // Skipping map_map_of_enum in query parameter serialization
        // Skipping map_map_of_enum in query parameter serialization

        // Skipping map_of_enum_string in query parameter serialization

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MapTest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for MapTest {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub map_map_of_string: Vec<HashMap<String, HashMap<String, String>>>,
            pub map_map_of_enum: Vec<HashMap<String, HashMap<String, String>>>,
            pub map_of_enum_string: Vec<HashMap<String, String>>,
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
                    "map_map_of_string" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    "map_map_of_enum" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    "map_of_enum_string" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(MapTest {
            map_map_of_string: intermediate_rep.map_map_of_string.into_iter().next(),
            map_map_of_enum: intermediate_rep.map_map_of_enum.into_iter().next(),
            map_of_enum_string: intermediate_rep.map_of_enum_string.into_iter().next(),
        })
    }
}


impl MapTest {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct MixedPropertiesAndAdditionalPropertiesClass {
    #[serde(rename = "uuid")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub uuid: Option<uuid::Uuid>,

    #[serde(rename = "dateTime")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub date_time: Option<chrono::DateTime::<chrono::Utc>>,

    #[serde(rename = "map")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map: Option<HashMap<String, models::Animal>>,

}

impl MixedPropertiesAndAdditionalPropertiesClass {
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
impl ::std::string::ToString for MixedPropertiesAndAdditionalPropertiesClass {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];
        // Skipping uuid in query parameter serialization

        // Skipping dateTime in query parameter serialization

        // Skipping map in query parameter serialization
        // Skipping map in query parameter serialization

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MixedPropertiesAndAdditionalPropertiesClass value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for MixedPropertiesAndAdditionalPropertiesClass {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub uuid: Vec<uuid::Uuid>,
            pub date_time: Vec<chrono::DateTime::<chrono::Utc>>,
            pub map: Vec<HashMap<String, models::Animal>>,
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
                    
                    "uuid" => intermediate_rep.uuid.push(uuid::Uuid::from_str(val).map_err(|x| ())?),
                    
                    
                    "dateTime" => intermediate_rep.date_time.push(chrono::DateTime::<chrono::Utc>::from_str(val).map_err(|x| ())?),
                    
                    "map" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(MixedPropertiesAndAdditionalPropertiesClass {
            uuid: intermediate_rep.uuid.into_iter().next(),
            date_time: intermediate_rep.date_time.into_iter().next(),
            map: intermediate_rep.map.into_iter().next(),
        })
    }
}


impl MixedPropertiesAndAdditionalPropertiesClass {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Model for testing model name starting with number

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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
impl ::std::string::ToString for Model200Response {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref name) = self.name {
            params.push("name".to_string());
            params.push(name.to_string());
        }


        if let Some(ref class) = self.class {
            params.push("class".to_string());
            params.push(class.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Model200Response value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for Model200Response {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub name: Vec<i32>,
            pub class: Vec<String>,
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
                    
                    "name" => intermediate_rep.name.push(i32::from_str(val).map_err(|x| ())?),
                    
                    
                    "class" => intermediate_rep.class.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(Model200Response {
            name: intermediate_rep.name.into_iter().next(),
            class: intermediate_rep.class.into_iter().next(),
        })
    }
}


impl Model200Response {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Model for testing reserved words

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
#[serde(rename = "Return")]
pub struct ModelReturn {
    #[serde(rename = "return")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub _return: Option<i32>,

}

impl ModelReturn {
    pub fn new() -> ModelReturn {
        ModelReturn {
            _return: None,
        }
    }
}

/// Converts the ModelReturn value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for ModelReturn {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref _return) = self._return {
            params.push("return".to_string());
            params.push(_return.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ModelReturn value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for ModelReturn {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub _return: Vec<i32>,
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
                    
                    "return" => intermediate_rep._return.push(i32::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(ModelReturn {
            _return: intermediate_rep._return.into_iter().next(),
        })
    }
}


impl ModelReturn {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

/// Model for testing model name same as property name

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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
    pub _123_number: Option<isize>,

}

impl Name {
    pub fn new(name: i32, ) -> Name {
        Name {
            name: name,
            snake_case: None,
            property: None,
            _123_number: None,
        }
    }
}

/// Converts the Name value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for Name {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        params.push("name".to_string());
        params.push(self.name.to_string());


        if let Some(ref snake_case) = self.snake_case {
            params.push("snake_case".to_string());
            params.push(snake_case.to_string());
        }


        if let Some(ref property) = self.property {
            params.push("property".to_string());
            params.push(property.to_string());
        }


        if let Some(ref _123_number) = self._123_number {
            params.push("123Number".to_string());
            params.push(_123_number.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Name value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for Name {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub name: Vec<i32>,
            pub snake_case: Vec<i32>,
            pub property: Vec<String>,
            pub _123_number: Vec<isize>,
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
                    
                    "name" => intermediate_rep.name.push(i32::from_str(val).map_err(|x| ())?),
                    
                    
                    "snake_case" => intermediate_rep.snake_case.push(i32::from_str(val).map_err(|x| ())?),
                    
                    
                    "property" => intermediate_rep.property.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "123Number" => intermediate_rep._123_number.push(isize::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(Name {
            name: intermediate_rep.name.into_iter().next().ok_or(())?,
            snake_case: intermediate_rep.snake_case.into_iter().next(),
            property: intermediate_rep.property.into_iter().next(),
            _123_number: intermediate_rep._123_number.into_iter().next(),
        })
    }
}


impl Name {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct NumberOnly {
    #[serde(rename = "JustNumber")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub just_number: Option<f64>,

}

impl NumberOnly {
    pub fn new() -> NumberOnly {
        NumberOnly {
            just_number: None,
        }
    }
}

/// Converts the NumberOnly value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for NumberOnly {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref just_number) = self.just_number {
            params.push("JustNumber".to_string());
            params.push(just_number.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a NumberOnly value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for NumberOnly {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub just_number: Vec<f64>,
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
                    
                    "JustNumber" => intermediate_rep.just_number.push(f64::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(NumberOnly {
            just_number: intermediate_rep.just_number.into_iter().next(),
        })
    }
}


impl NumberOnly {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct ObjectContainingObjectWithOnlyAdditionalProperties {
    #[serde(rename = "inner")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub inner: Option<models::ObjectWithOnlyAdditionalProperties>,

}

impl ObjectContainingObjectWithOnlyAdditionalProperties {
    pub fn new() -> ObjectContainingObjectWithOnlyAdditionalProperties {
        ObjectContainingObjectWithOnlyAdditionalProperties {
            inner: None,
        }
    }
}

/// Converts the ObjectContainingObjectWithOnlyAdditionalProperties value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for ObjectContainingObjectWithOnlyAdditionalProperties {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];
        // Skipping inner in query parameter serialization

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectContainingObjectWithOnlyAdditionalProperties value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for ObjectContainingObjectWithOnlyAdditionalProperties {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub inner: Vec<models::ObjectWithOnlyAdditionalProperties>,
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
                    
                    "inner" => intermediate_rep.inner.push(models::ObjectWithOnlyAdditionalProperties::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(ObjectContainingObjectWithOnlyAdditionalProperties {
            inner: intermediate_rep.inner.into_iter().next(),
        })
    }
}


impl ObjectContainingObjectWithOnlyAdditionalProperties {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct ObjectWithOnlyAdditionalProperties(HashMap<String, String>);

impl ::std::convert::From<HashMap<String, String>> for ObjectWithOnlyAdditionalProperties {
    fn from(x: HashMap<String, String>) -> Self {
        ObjectWithOnlyAdditionalProperties(x)
    }
}


impl ::std::convert::From<ObjectWithOnlyAdditionalProperties> for HashMap<String, String> {
    fn from(x: ObjectWithOnlyAdditionalProperties) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for ObjectWithOnlyAdditionalProperties {
    type Target = HashMap<String, String>;
    fn deref(&self) -> &HashMap<String, String> {
        &self.0
    }
}

impl ::std::ops::DerefMut for ObjectWithOnlyAdditionalProperties {
    fn deref_mut(&mut self) -> &mut HashMap<String, String> {
        &mut self.0
    }
}

/// Converts the ObjectWithOnlyAdditionalProperties value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for ObjectWithOnlyAdditionalProperties {
    fn to_string(&self) -> String {
        // Skipping additionalProperties in query parameter serialization
        "".to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectWithOnlyAdditionalProperties value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for ObjectWithOnlyAdditionalProperties {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Parsing additionalProperties in this style is not supported yet
        Err(())
    }
}

impl ObjectWithOnlyAdditionalProperties {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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

    /// Order Status
    // Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "status")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub status: Option<String>,

    #[serde(rename = "complete")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub complete: Option<bool>,

}

impl Order {
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
impl ::std::string::ToString for Order {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref id) = self.id {
            params.push("id".to_string());
            params.push(id.to_string());
        }


        if let Some(ref pet_id) = self.pet_id {
            params.push("petId".to_string());
            params.push(pet_id.to_string());
        }


        if let Some(ref quantity) = self.quantity {
            params.push("quantity".to_string());
            params.push(quantity.to_string());
        }

        // Skipping shipDate in query parameter serialization


        if let Some(ref status) = self.status {
            params.push("status".to_string());
            params.push(status.to_string());
        }


        if let Some(ref complete) = self.complete {
            params.push("complete".to_string());
            params.push(complete.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Order value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for Order {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub id: Vec<i64>,
            pub pet_id: Vec<i64>,
            pub quantity: Vec<i32>,
            pub ship_date: Vec<chrono::DateTime::<chrono::Utc>>,
            pub status: Vec<String>,
            pub complete: Vec<bool>,
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
                    
                    "id" => intermediate_rep.id.push(i64::from_str(val).map_err(|x| ())?),
                    
                    
                    "petId" => intermediate_rep.pet_id.push(i64::from_str(val).map_err(|x| ())?),
                    
                    
                    "quantity" => intermediate_rep.quantity.push(i32::from_str(val).map_err(|x| ())?),
                    
                    
                    "shipDate" => intermediate_rep.ship_date.push(chrono::DateTime::<chrono::Utc>::from_str(val).map_err(|x| ())?),
                    
                    
                    "status" => intermediate_rep.status.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "complete" => intermediate_rep.complete.push(bool::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(Order {
            id: intermediate_rep.id.into_iter().next(),
            pet_id: intermediate_rep.pet_id.into_iter().next(),
            quantity: intermediate_rep.quantity.into_iter().next(),
            ship_date: intermediate_rep.ship_date.into_iter().next(),
            status: intermediate_rep.status.into_iter().next(),
            complete: intermediate_rep.complete.into_iter().next(),
        })
    }
}


impl Order {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct OuterBoolean(bool);

impl ::std::convert::From<bool> for OuterBoolean {
    fn from(x: bool) -> Self {
        OuterBoolean(x)
    }
}


impl ::std::convert::From<OuterBoolean> for bool {
    fn from(x: OuterBoolean) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for OuterBoolean {
    type Target = bool;
    fn deref(&self) -> &bool {
        &self.0
    }
}

impl ::std::ops::DerefMut for OuterBoolean {
    fn deref_mut(&mut self) -> &mut bool {
        &mut self.0
    }
}


impl OuterBoolean {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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
impl ::std::string::ToString for OuterComposite {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref my_number) = self.my_number {
            params.push("my_number".to_string());
            params.push(my_number.to_string());
        }


        if let Some(ref my_string) = self.my_string {
            params.push("my_string".to_string());
            params.push(my_string.to_string());
        }


        if let Some(ref my_boolean) = self.my_boolean {
            params.push("my_boolean".to_string());
            params.push(my_boolean.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a OuterComposite value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for OuterComposite {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub my_number: Vec<f64>,
            pub my_string: Vec<String>,
            pub my_boolean: Vec<bool>,
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
                    
                    "my_number" => intermediate_rep.my_number.push(f64::from_str(val).map_err(|x| ())?),
                    
                    
                    "my_string" => intermediate_rep.my_string.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "my_boolean" => intermediate_rep.my_boolean.push(bool::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(OuterComposite {
            my_number: intermediate_rep.my_number.into_iter().next(),
            my_string: intermediate_rep.my_string.into_iter().next(),
            my_boolean: intermediate_rep.my_boolean.into_iter().next(),
        })
    }
}


impl OuterComposite {
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
pub enum OuterEnum { 
    #[serde(rename = "placed")]
    PLACED,
    #[serde(rename = "approved")]
    APPROVED,
    #[serde(rename = "delivered")]
    DELIVERED,
}

impl ::std::fmt::Display for OuterEnum {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self { 
            OuterEnum::PLACED => write!(f, "{}", "placed"),
            OuterEnum::APPROVED => write!(f, "{}", "approved"),
            OuterEnum::DELIVERED => write!(f, "{}", "delivered"),
        }
    }
}

impl ::std::str::FromStr for OuterEnum {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "placed" => Ok(OuterEnum::PLACED),
            "approved" => Ok(OuterEnum::APPROVED),
            "delivered" => Ok(OuterEnum::DELIVERED),
            _ => Err(()),
        }
    }
}

impl OuterEnum {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct OuterNumber(f64);

impl ::std::convert::From<f64> for OuterNumber {
    fn from(x: f64) -> Self {
        OuterNumber(x)
    }
}


impl ::std::convert::From<OuterNumber> for f64 {
    fn from(x: OuterNumber) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for OuterNumber {
    type Target = f64;
    fn deref(&self) -> &f64 {
        &self.0
    }
}

impl ::std::ops::DerefMut for OuterNumber {
    fn deref_mut(&mut self) -> &mut f64 {
        &mut self.0
    }
}


impl OuterNumber {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct OuterString(String);

impl ::std::convert::From<String> for OuterString {
    fn from(x: String) -> Self {
        OuterString(x)
    }
}

impl std::str::FromStr for OuterString {
    type Err = ParseError;
    fn from_str(x: &str) -> Result<Self, Self::Err> {
        Ok(OuterString(x.to_string()))
    }
}

impl ::std::convert::From<OuterString> for String {
    fn from(x: OuterString) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for OuterString {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl ::std::ops::DerefMut for OuterString {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}


impl OuterString {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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

    /// pet status in the store
    // Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "status")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub status: Option<String>,

}

impl Pet {
    pub fn new(name: String, photo_urls: Vec<String>, ) -> Pet {
        Pet {
            id: None,
            category: None,
            name: name,
            photo_urls: photo_urls,
            tags: None,
            status: None,
        }
    }
}

/// Converts the Pet value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for Pet {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref id) = self.id {
            params.push("id".to_string());
            params.push(id.to_string());
        }

        // Skipping category in query parameter serialization


        params.push("name".to_string());
        params.push(self.name.to_string());


        params.push("photoUrls".to_string());
        params.push(self.photo_urls.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",").to_string());

        // Skipping tags in query parameter serialization


        if let Some(ref status) = self.status {
            params.push("status".to_string());
            params.push(status.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Pet value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for Pet {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub id: Vec<i64>,
            pub category: Vec<models::Category>,
            pub name: Vec<String>,
            pub photo_urls: Vec<Vec<String>>,
            pub tags: Vec<Vec<models::Tag>>,
            pub status: Vec<String>,
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
                    
                    "id" => intermediate_rep.id.push(i64::from_str(val).map_err(|x| ())?),
                    
                    
                    "category" => intermediate_rep.category.push(models::Category::from_str(val).map_err(|x| ())?),
                    
                    
                    "name" => intermediate_rep.name.push(String::from_str(val).map_err(|x| ())?),
                    
                    "photoUrls" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    "tags" => return Err(()), // Parsing a container in this style is not supported yet
                    
                    
                    "status" => intermediate_rep.status.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(Pet {
            id: intermediate_rep.id.into_iter().next(),
            category: intermediate_rep.category.into_iter().next(),
            name: intermediate_rep.name.into_iter().next().ok_or(())?,
            photo_urls: intermediate_rep.photo_urls.into_iter().next().ok_or(())?,
            tags: intermediate_rep.tags.into_iter().next(),
            status: intermediate_rep.status.into_iter().next(),
        })
    }
}


impl Pet {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
pub struct ReadOnlyFirst {
    #[serde(rename = "bar")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub bar: Option<String>,

    #[serde(rename = "baz")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub baz: Option<String>,

}

impl ReadOnlyFirst {
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
impl ::std::string::ToString for ReadOnlyFirst {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref bar) = self.bar {
            params.push("bar".to_string());
            params.push(bar.to_string());
        }


        if let Some(ref baz) = self.baz {
            params.push("baz".to_string());
            params.push(baz.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ReadOnlyFirst value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for ReadOnlyFirst {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub bar: Vec<String>,
            pub baz: Vec<String>,
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
                    
                    "bar" => intermediate_rep.bar.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "baz" => intermediate_rep.baz.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(ReadOnlyFirst {
            bar: intermediate_rep.bar.into_iter().next(),
            baz: intermediate_rep.baz.into_iter().next(),
        })
    }
}


impl ReadOnlyFirst {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
#[serde(rename = "$special[model.name]")]
pub struct SpecialModelName {
    #[serde(rename = "$special[property.name]")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub special_property_name: Option<i64>,

}

impl SpecialModelName {
    pub fn new() -> SpecialModelName {
        SpecialModelName {
            special_property_name: None,
        }
    }
}

/// Converts the SpecialModelName value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl ::std::string::ToString for SpecialModelName {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref special_property_name) = self.special_property_name {
            params.push("$special[property.name]".to_string());
            params.push(special_property_name.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a SpecialModelName value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for SpecialModelName {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub special_property_name: Vec<i64>,
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
                    
                    "$special[property.name]" => intermediate_rep.special_property_name.push(i64::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(SpecialModelName {
            special_property_name: intermediate_rep.special_property_name.into_iter().next(),
        })
    }
}


impl SpecialModelName {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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
impl ::std::string::ToString for Tag {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref id) = self.id {
            params.push("id".to_string());
            params.push(id.to_string());
        }


        if let Some(ref name) = self.name {
            params.push("name".to_string());
            params.push(name.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Tag value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for Tag {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
        struct IntermediateRep {
            pub id: Vec<i64>,
            pub name: Vec<String>,
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
                    
                    "id" => intermediate_rep.id.push(i64::from_str(val).map_err(|x| ())?),
                    
                    
                    "name" => intermediate_rep.name.push(String::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(Tag {
            id: intermediate_rep.id.into_iter().next(),
            name: intermediate_rep.name.into_iter().next(),
        })
    }
}


impl Tag {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[cfg_attr(feature = "conversion", derive(LabelledGeneric))]
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
impl ::std::string::ToString for User {
    fn to_string(&self) -> String {
        let mut params: Vec<String> = vec![];

        if let Some(ref id) = self.id {
            params.push("id".to_string());
            params.push(id.to_string());
        }


        if let Some(ref username) = self.username {
            params.push("username".to_string());
            params.push(username.to_string());
        }


        if let Some(ref first_name) = self.first_name {
            params.push("firstName".to_string());
            params.push(first_name.to_string());
        }


        if let Some(ref last_name) = self.last_name {
            params.push("lastName".to_string());
            params.push(last_name.to_string());
        }


        if let Some(ref email) = self.email {
            params.push("email".to_string());
            params.push(email.to_string());
        }


        if let Some(ref password) = self.password {
            params.push("password".to_string());
            params.push(password.to_string());
        }


        if let Some(ref phone) = self.phone {
            params.push("phone".to_string());
            params.push(phone.to_string());
        }


        if let Some(ref user_status) = self.user_status {
            params.push("userStatus".to_string());
            params.push(user_status.to_string());
        }

        params.join(",").to_string()
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a User value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for User {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        #[derive(Default)]
        // An intermediate representation of the struct to use for parsing.
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
        let mut string_iter = s.split(',').into_iter();
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return Err(())
            };

            if let Some(key) = key_result {
                match key {
                    
                    "id" => intermediate_rep.id.push(i64::from_str(val).map_err(|x| ())?),
                    
                    
                    "username" => intermediate_rep.username.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "firstName" => intermediate_rep.first_name.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "lastName" => intermediate_rep.last_name.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "email" => intermediate_rep.email.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "password" => intermediate_rep.password.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "phone" => intermediate_rep.phone.push(String::from_str(val).map_err(|x| ())?),
                    
                    
                    "userStatus" => intermediate_rep.user_status.push(i32::from_str(val).map_err(|x| ())?),
                    
                    _ => return Err(()) // Parse error - unexpected key
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        Ok(User {
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


impl User {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}
