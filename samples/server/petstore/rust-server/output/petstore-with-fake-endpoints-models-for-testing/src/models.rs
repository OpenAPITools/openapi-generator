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
    pub date: chrono::DateTime<chrono::Utc>,

    #[serde(rename = "dateTime")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub date_time: Option<chrono::DateTime<chrono::Utc>>,

    #[serde(rename = "uuid")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub uuid: Option<uuid::Uuid>,

    #[serde(rename = "password")]
    pub password: String,

}

impl FormatTest {
    pub fn new(number: f64, byte: swagger::ByteArray, date: chrono::DateTime<chrono::Utc>, password: String, ) -> FormatTest {
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
    pub date_time: Option<chrono::DateTime<chrono::Utc>>,

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
    pub ship_date: Option<chrono::DateTime<chrono::Utc>>,

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

impl User {
    /// Helper function to allow us to convert this model to an XML string.
    /// Will panic if serialisation fails.
    #[allow(dead_code)]
    pub(crate) fn to_xml(&self) -> String {
        serde_xml_rs::to_string(&self).expect("impossible to fail to serialize")
    }
}
