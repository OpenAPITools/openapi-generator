#![allow(unused_imports, unused_qualifications, unused_extern_crates)]
extern crate chrono;
extern crate uuid;

use serde_xml_rs;
use serde::ser::Serializer;

use std::collections::HashMap;
use models;
use swagger;


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct AdditionalPropertiesClass(object);

impl ::std::convert::From<object> for AdditionalPropertiesClass {
    fn from(x: object) -> Self {
        AdditionalPropertiesClass(x)
    }
}

impl ::std::convert::From<AdditionalPropertiesClass> for object {
    fn from(x: AdditionalPropertiesClass) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for AdditionalPropertiesClass {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for AdditionalPropertiesClass {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct Animal(object);

impl ::std::convert::From<object> for Animal {
    fn from(x: object) -> Self {
        Animal(x)
    }
}

impl ::std::convert::From<Animal> for object {
    fn from(x: Animal) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for Animal {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for Animal {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct ApiResponse(object);

impl ::std::convert::From<object> for ApiResponse {
    fn from(x: object) -> Self {
        ApiResponse(x)
    }
}

impl ::std::convert::From<ApiResponse> for object {
    fn from(x: ApiResponse) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for ApiResponse {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for ApiResponse {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct ArrayOfArrayOfNumberOnly(object);

impl ::std::convert::From<object> for ArrayOfArrayOfNumberOnly {
    fn from(x: object) -> Self {
        ArrayOfArrayOfNumberOnly(x)
    }
}

impl ::std::convert::From<ArrayOfArrayOfNumberOnly> for object {
    fn from(x: ArrayOfArrayOfNumberOnly) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for ArrayOfArrayOfNumberOnly {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for ArrayOfArrayOfNumberOnly {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct ArrayOfNumberOnly(object);

impl ::std::convert::From<object> for ArrayOfNumberOnly {
    fn from(x: object) -> Self {
        ArrayOfNumberOnly(x)
    }
}

impl ::std::convert::From<ArrayOfNumberOnly> for object {
    fn from(x: ArrayOfNumberOnly) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for ArrayOfNumberOnly {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for ArrayOfNumberOnly {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct ArrayTest(object);

impl ::std::convert::From<object> for ArrayTest {
    fn from(x: object) -> Self {
        ArrayTest(x)
    }
}

impl ::std::convert::From<ArrayTest> for object {
    fn from(x: ArrayTest) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for ArrayTest {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for ArrayTest {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct Capitalization(object);

impl ::std::convert::From<object> for Capitalization {
    fn from(x: object) -> Self {
        Capitalization(x)
    }
}

impl ::std::convert::From<Capitalization> for object {
    fn from(x: Capitalization) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for Capitalization {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for Capitalization {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename = "Category")]
pub struct Category(object);

impl ::std::convert::From<object> for Category {
    fn from(x: object) -> Self {
        Category(x)
    }
}

impl ::std::convert::From<Category> for object {
    fn from(x: Category) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for Category {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for Category {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


/// Model for testing model with \"_class\" property
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct ClassModel(object);

impl ::std::convert::From<object> for ClassModel {
    fn from(x: object) -> Self {
        ClassModel(x)
    }
}

impl ::std::convert::From<ClassModel> for object {
    fn from(x: ClassModel) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for ClassModel {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for ClassModel {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct Client(object);

impl ::std::convert::From<object> for Client {
    fn from(x: object) -> Self {
        Client(x)
    }
}

impl ::std::convert::From<Client> for object {
    fn from(x: Client) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for Client {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for Client {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct EnumArrays(object);

impl ::std::convert::From<object> for EnumArrays {
    fn from(x: object) -> Self {
        EnumArrays(x)
    }
}

impl ::std::convert::From<EnumArrays> for object {
    fn from(x: EnumArrays) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for EnumArrays {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for EnumArrays {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize, Eq, Ord)]
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

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct EnumTest(object);

impl ::std::convert::From<object> for EnumTest {
    fn from(x: object) -> Self {
        EnumTest(x)
    }
}

impl ::std::convert::From<EnumTest> for object {
    fn from(x: EnumTest) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for EnumTest {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for EnumTest {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct FormatTest(object);

impl ::std::convert::From<object> for FormatTest {
    fn from(x: object) -> Self {
        FormatTest(x)
    }
}

impl ::std::convert::From<FormatTest> for object {
    fn from(x: FormatTest) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for FormatTest {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for FormatTest {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct HasOnlyReadOnly(object);

impl ::std::convert::From<object> for HasOnlyReadOnly {
    fn from(x: object) -> Self {
        HasOnlyReadOnly(x)
    }
}

impl ::std::convert::From<HasOnlyReadOnly> for object {
    fn from(x: HasOnlyReadOnly) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for HasOnlyReadOnly {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for HasOnlyReadOnly {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct List(object);

impl ::std::convert::From<object> for List {
    fn from(x: object) -> Self {
        List(x)
    }
}

impl ::std::convert::From<List> for object {
    fn from(x: List) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for List {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for List {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct MapTest(object);

impl ::std::convert::From<object> for MapTest {
    fn from(x: object) -> Self {
        MapTest(x)
    }
}

impl ::std::convert::From<MapTest> for object {
    fn from(x: MapTest) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for MapTest {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for MapTest {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct MixedPropertiesAndAdditionalPropertiesClass(object);

impl ::std::convert::From<object> for MixedPropertiesAndAdditionalPropertiesClass {
    fn from(x: object) -> Self {
        MixedPropertiesAndAdditionalPropertiesClass(x)
    }
}

impl ::std::convert::From<MixedPropertiesAndAdditionalPropertiesClass> for object {
    fn from(x: MixedPropertiesAndAdditionalPropertiesClass) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for MixedPropertiesAndAdditionalPropertiesClass {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for MixedPropertiesAndAdditionalPropertiesClass {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


/// Model for testing model name starting with number
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename = "Name")]
pub struct Model200Response(object);

impl ::std::convert::From<object> for Model200Response {
    fn from(x: object) -> Self {
        Model200Response(x)
    }
}

impl ::std::convert::From<Model200Response> for object {
    fn from(x: Model200Response) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for Model200Response {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for Model200Response {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


/// Model for testing reserved words
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename = "Return")]
pub struct ModelReturn(object);

impl ::std::convert::From<object> for ModelReturn {
    fn from(x: object) -> Self {
        ModelReturn(x)
    }
}

impl ::std::convert::From<ModelReturn> for object {
    fn from(x: ModelReturn) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for ModelReturn {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for ModelReturn {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


/// Model for testing model name same as property name
#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename = "Name")]
pub struct Name(object);

impl ::std::convert::From<object> for Name {
    fn from(x: object) -> Self {
        Name(x)
    }
}

impl ::std::convert::From<Name> for object {
    fn from(x: Name) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for Name {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for Name {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct NumberOnly(object);

impl ::std::convert::From<object> for NumberOnly {
    fn from(x: object) -> Self {
        NumberOnly(x)
    }
}

impl ::std::convert::From<NumberOnly> for object {
    fn from(x: NumberOnly) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for NumberOnly {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for NumberOnly {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename = "Order")]
pub struct Order(object);

impl ::std::convert::From<object> for Order {
    fn from(x: object) -> Self {
        Order(x)
    }
}

impl ::std::convert::From<Order> for object {
    fn from(x: Order) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for Order {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for Order {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct OuterComposite(object);

impl ::std::convert::From<object> for OuterComposite {
    fn from(x: object) -> Self {
        OuterComposite(x)
    }
}

impl ::std::convert::From<OuterComposite> for object {
    fn from(x: OuterComposite) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for OuterComposite {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for OuterComposite {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Serialize, Deserialize, Eq, Ord)]
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

#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename = "Pet")]
pub struct Pet(object);

impl ::std::convert::From<object> for Pet {
    fn from(x: object) -> Self {
        Pet(x)
    }
}

impl ::std::convert::From<Pet> for object {
    fn from(x: Pet) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for Pet {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for Pet {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]

pub struct ReadOnlyFirst(object);

impl ::std::convert::From<object> for ReadOnlyFirst {
    fn from(x: object) -> Self {
        ReadOnlyFirst(x)
    }
}

impl ::std::convert::From<ReadOnlyFirst> for object {
    fn from(x: ReadOnlyFirst) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for ReadOnlyFirst {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for ReadOnlyFirst {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename = "$special[model.name]")]
pub struct SpecialModelName(object);

impl ::std::convert::From<object> for SpecialModelName {
    fn from(x: object) -> Self {
        SpecialModelName(x)
    }
}

impl ::std::convert::From<SpecialModelName> for object {
    fn from(x: SpecialModelName) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for SpecialModelName {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for SpecialModelName {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename = "Tag")]
pub struct Tag(object);

impl ::std::convert::From<object> for Tag {
    fn from(x: object) -> Self {
        Tag(x)
    }
}

impl ::std::convert::From<Tag> for object {
    fn from(x: Tag) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for Tag {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for Tag {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, Serialize, Deserialize)]
#[serde(rename = "User")]
pub struct User(object);

impl ::std::convert::From<object> for User {
    fn from(x: object) -> Self {
        User(x)
    }
}

impl ::std::convert::From<User> for object {
    fn from(x: User) -> Self {
        x.0
    }
}

impl ::std::ops::Deref for User {
    type Target = object;
    fn deref(&self) -> &object {
        &self.0
    }
}

impl ::std::ops::DerefMut for User {
    fn deref_mut(&mut self) -> &mut object {
        &mut self.0
    }
}

