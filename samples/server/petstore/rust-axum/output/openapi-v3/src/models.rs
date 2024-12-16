#![allow(unused_qualifications)]

use http::HeaderValue;
use validator::Validate;

#[cfg(feature = "server")]
use crate::header;
use crate::{models, types::*};

      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct AnyOfGetQueryParams {
            /// list of any of objects
                #[serde(rename = "any-of")]
                #[validate(
                        length(min = 1),
                    )]
                #[serde(skip_serializing_if="Option::is_none")]
                pub any_of: Option<Vec<models::AnyOfObject>>,
    }

      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct CallbackWithHeaderPostQueryParams {
                #[serde(rename = "url")]
                pub url: String,
    }

      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct ComplexQueryParamGetQueryParams {
                #[serde(rename = "list-of-strings")]
                #[serde(skip_serializing_if="Option::is_none")]
                pub list_of_strings: Option<Vec<models::StringObject>>,
    }

      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct EnumInPathPathParamGetPathParams {
                pub path_param: models::StringEnum,
    }


      
      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct GetWithBooleanParameterQueryParams {
            /// Let's check apostrophes get encoded properly!
                #[serde(rename = "iambool")]
                pub iambool: bool,
    }

      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct JsonComplexQueryParamGetQueryParams {
                #[serde(rename = "list-of-strings")]
                #[serde(skip_serializing_if="Option::is_none")]
                pub list_of_strings: Option<Vec<models::StringObject>>,
    }

      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
    pub struct MandatoryRequestHeaderGetHeaderParams {
        pub x_header: String,
    }

            
      
      
      
      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct MultiplePathParamsWithVeryLongPathToTestFormattingPathParamAPathParamBGetPathParams {
                pub path_param_a: String,
                pub path_param_b: String,
    }


      
      
      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct ParamgetGetQueryParams {
            /// The stuff to get
                #[serde(rename = "uuid")]
                #[serde(skip_serializing_if="Option::is_none")]
                pub uuid: Option<uuid::Uuid>,
            /// Some object to pass as query parameter
                #[serde(rename = "someObject")]
                #[serde(skip_serializing_if="Option::is_none")]
                pub some_object: Option<crate::types::Object>,
            /// Some list to pass as query parameter
                #[serde(rename = "someList")]
                #[serde(skip_serializing_if="Option::is_none")]
                pub some_list: Option<Vec<models::MyId>>,
    }

      
      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct RegisterCallbackPostQueryParams {
                #[serde(rename = "url")]
                pub url: String,
    }

      
      
      
      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
    pub struct TwoFirstLetterHeadersHeaderParams {
        pub x_header_one: Option<bool>,
        pub x_header_two: Option<bool>,
    }

            
      
      
      
      
      
      
      
      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct GetRepoInfoPathParams {
                pub repo_id: String,
    }


      


/// Check that an object with only additional properties that references another object (e.g. an anyOf object) isn't treated as freeForm
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AdditionalPropertiesReferencedAnyOfObject(std::collections::HashMap<String, models::AnyOfProperty>);

impl validator::Validate for AdditionalPropertiesReferencedAnyOfObject {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<std::collections::HashMap<String, models::AnyOfProperty>> for AdditionalPropertiesReferencedAnyOfObject {
    fn from(x: std::collections::HashMap<String, models::AnyOfProperty>) -> Self {
        AdditionalPropertiesReferencedAnyOfObject(x)
    }
}

impl std::convert::From<AdditionalPropertiesReferencedAnyOfObject> for std::collections::HashMap<String, models::AnyOfProperty> {
    fn from(x: AdditionalPropertiesReferencedAnyOfObject) -> Self {
        x.0
    }
}

impl std::ops::Deref for AdditionalPropertiesReferencedAnyOfObject {
    type Target = std::collections::HashMap<String, models::AnyOfProperty>;
    fn deref(&self) -> &std::collections::HashMap<String, models::AnyOfProperty> {
        &self.0
    }
}

impl std::ops::DerefMut for AdditionalPropertiesReferencedAnyOfObject {
    fn deref_mut(&mut self) -> &mut std::collections::HashMap<String, models::AnyOfProperty> {
        &mut self.0
    }
}

/// Converts the AdditionalPropertiesReferencedAnyOfObject value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for AdditionalPropertiesReferencedAnyOfObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Skipping additionalProperties in query parameter serialization
        write!(f, "")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AdditionalPropertiesReferencedAnyOfObject value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for AdditionalPropertiesReferencedAnyOfObject {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Err("Parsing additionalProperties for AdditionalPropertiesReferencedAnyOfObject is not supported")
    }
}


#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AdditionalPropertiesWithList(std::collections::HashMap<String, Vec<String>>);

impl validator::Validate for AdditionalPropertiesWithList {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<std::collections::HashMap<String, Vec<String>>> for AdditionalPropertiesWithList {
    fn from(x: std::collections::HashMap<String, Vec<String>>) -> Self {
        AdditionalPropertiesWithList(x)
    }
}

impl std::convert::From<AdditionalPropertiesWithList> for std::collections::HashMap<String, Vec<String>> {
    fn from(x: AdditionalPropertiesWithList) -> Self {
        x.0
    }
}

impl std::ops::Deref for AdditionalPropertiesWithList {
    type Target = std::collections::HashMap<String, Vec<String>>;
    fn deref(&self) -> &std::collections::HashMap<String, Vec<String>> {
        &self.0
    }
}

impl std::ops::DerefMut for AdditionalPropertiesWithList {
    fn deref_mut(&mut self) -> &mut std::collections::HashMap<String, Vec<String>> {
        &mut self.0
    }
}

/// Converts the AdditionalPropertiesWithList value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for AdditionalPropertiesWithList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Skipping additionalProperties in query parameter serialization
        write!(f, "")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AdditionalPropertiesWithList value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for AdditionalPropertiesWithList {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Err("Parsing additionalProperties for AdditionalPropertiesWithList is not supported")
    }
}


#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AdditionalPropertiesWithNullable {
    #[serde(rename = "nullableString")]
    #[serde(deserialize_with = "deserialize_optional_nullable")]
    #[serde(default = "default_optional_nullable")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub nullable_string: Option<Nullable<String>>,

    #[serde(rename = "nullableMap")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub nullable_map: Option<std::collections::HashMap<String, models::NullableObject>>,

}

impl AdditionalPropertiesWithNullable {
    fn __name_for_nullable_string() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='nullableString', complexType='string', getter='getNullableString', setter='setNullableString', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='nullable_string', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.nullableString;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "nullable" : true,
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=true, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='nullableString', nameInPascalCase='NullableString', nameInSnakeCase='NULLABLE_STRING', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_nullable_string<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_nullable_string())
        }
}
impl AdditionalPropertiesWithNullable {
    fn __name_for_nullable_map() -> String {
        String::from("CodegenProperty{openApiType='object', baseName='nullableMap', complexType='string', getter='getNullableMap', setter='setNullableMap', description='null', dataType='std::collections::HashMap<String, models::NullableObject>', datatypeWithEnum='std::collections::HashMap<String, models::NullableObject>', dataFormat='null', name='nullable_map', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.nullableMap;', baseType='map', containerType='map', containerTypeMapped='std::collections::HashMap', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "additionalProperties" : {
    "$ref" : "#/components/schemas/NullableObject"
  },
  "type" : "object"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=true, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "nullable" : true,
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=true, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=CodegenProperty{openApiType='string', baseName='additional_properties', complexType='string', getter='getAdditionalProperties', setter='setAdditionalProperties', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='additional_properties', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.additional_properties;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "nullable" : true,
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=true, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='additionalProperties', nameInPascalCase='AdditionalProperties', nameInSnakeCase='ADDITIONAL_PROPERTIES', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "nullable" : true,
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=true, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='nullableMap', nameInPascalCase='NullableMap', nameInSnakeCase='NULLABLE_MAP', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=true, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_nullable_map<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_nullable_map())
        }
}


impl AdditionalPropertiesWithNullable {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> AdditionalPropertiesWithNullable {
        AdditionalPropertiesWithNullable {
            nullable_string: None,
            nullable_map: None,
        }
    }
}

/// Converts the AdditionalPropertiesWithNullable value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for AdditionalPropertiesWithNullable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.nullable_string.as_ref().map(|nullable_string| {
                [
                    "nullableString".to_string(),
                    nullable_string.as_ref().map_or("null".to_string(), |x| x.to_string()),
                ].join(",")
            }),

            // Skipping nullableMap in query parameter serialization

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AdditionalPropertiesWithNullable value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AdditionalPropertiesWithNullable {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub nullable_string: Vec<String>,
            pub nullable_map: Vec<std::collections::HashMap<String, models::NullableObject>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing AdditionalPropertiesWithNullable".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "nullableString" => return std::result::Result::Err("Parsing a nullable type in this style is not supported in AdditionalPropertiesWithNullable".to_string()),
                    "nullableMap" => return std::result::Result::Err("Parsing a container in this style is not supported in AdditionalPropertiesWithNullable".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing AdditionalPropertiesWithNullable".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(AdditionalPropertiesWithNullable {
            nullable_string: std::result::Result::Err("Nullable types not supported in AdditionalPropertiesWithNullable".to_string())?,
            nullable_map: intermediate_rep.nullable_map.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<AdditionalPropertiesWithNullable> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<AdditionalPropertiesWithNullable>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AdditionalPropertiesWithNullable>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AdditionalPropertiesWithNullable - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<AdditionalPropertiesWithNullable> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AdditionalPropertiesWithNullable as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into AdditionalPropertiesWithNullable - {}",
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
pub struct AnotherXmlArray(Vec<String>);

impl validator::Validate for AnotherXmlArray {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<Vec<String>> for AnotherXmlArray {
    fn from(x: Vec<String>) -> Self {
        AnotherXmlArray(x)
    }
}

impl std::convert::From<AnotherXmlArray> for Vec<String> {
    fn from(x: AnotherXmlArray) -> Self {
        x.0
    }
}

impl std::iter::FromIterator<String> for AnotherXmlArray {
    fn from_iter<U: IntoIterator<Item=String>>(u: U) -> Self {
        AnotherXmlArray(Vec::<String>::from_iter(u))
    }
}

impl std::iter::IntoIterator for AnotherXmlArray {
    type Item = String;
    type IntoIter = std::vec::IntoIter<String>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> std::iter::IntoIterator for &'a AnotherXmlArray {
    type Item = &'a String;
    type IntoIter = std::slice::Iter<'a, String>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a> std::iter::IntoIterator for &'a mut AnotherXmlArray {
    type Item = &'a mut String;
    type IntoIter = std::slice::IterMut<'a, String>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl std::ops::Deref for AnotherXmlArray {
    type Target = Vec<String>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for AnotherXmlArray {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Converts the AnotherXmlArray value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for AnotherXmlArray {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{}", self.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnotherXmlArray value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AnotherXmlArray {
    type Err = <String as std::str::FromStr>::Err;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut items = vec![];
        for item in s.split(',')
        {
            items.push(item.parse()?);
        }
        std::result::Result::Ok(AnotherXmlArray(items))
    }
}


// Methods for converting between header::IntoHeaderValue<AnotherXmlArray> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<AnotherXmlArray>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AnotherXmlArray>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AnotherXmlArray - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<AnotherXmlArray> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AnotherXmlArray as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into AnotherXmlArray - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AnotherXmlInner(String);

impl validator::Validate for AnotherXmlInner {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<String> for AnotherXmlInner {
    fn from(x: String) -> Self {
        AnotherXmlInner(x)
    }
}

impl std::fmt::Display for AnotherXmlInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{:?}", self.0)
    }
}

impl std::str::FromStr for AnotherXmlInner {
    type Err = std::string::ParseError;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(AnotherXmlInner(x.to_string()))
    }
}

impl std::convert::From<AnotherXmlInner> for String {
    fn from(x: AnotherXmlInner) -> Self {
        x.0
    }
}

impl std::ops::Deref for AnotherXmlInner {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for AnotherXmlInner {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}



/// An XML object
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AnotherXmlObject {
    #[serde(rename = "inner_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub inner_string: Option<String>,

}

impl AnotherXmlObject {
    fn __name_for_inner_string() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='inner_string', complexType='string', getter='getInnerString', setter='setInnerString', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner_string', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.inner_string;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='innerString', nameInPascalCase='InnerString', nameInSnakeCase='INNER_STRING', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_inner_string<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_inner_string())
        }
}


impl AnotherXmlObject {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> AnotherXmlObject {
        AnotherXmlObject {
            inner_string: None,
        }
    }
}

/// Converts the AnotherXmlObject value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for AnotherXmlObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.inner_string.as_ref().map(|inner_string| {
                [
                    "inner_string".to_string(),
                    inner_string.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnotherXmlObject value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AnotherXmlObject {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub inner_string: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing AnotherXmlObject".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "inner_string" => intermediate_rep.inner_string.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing AnotherXmlObject".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(AnotherXmlObject {
            inner_string: intermediate_rep.inner_string.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<AnotherXmlObject> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<AnotherXmlObject>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AnotherXmlObject>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AnotherXmlObject - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<AnotherXmlObject> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AnotherXmlObject as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into AnotherXmlObject - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




/// Any of:
/// - String
/// - uuid::Uuid
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnyOfGet202Response(Box<serde_json::value::RawValue>);

impl validator::Validate for AnyOfGet202Response
{
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnyOfGet202Response value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AnyOfGet202Response {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

impl PartialEq for AnyOfGet202Response {
    fn eq(&self, other: &Self) -> bool {
        self.0.get() == other.0.get()
    }
}





/// Test a model containing an anyOf of a hash map
/// Any of:
/// - String
/// - std::collections::HashMap<String, String>
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnyOfHashMapObject(Box<serde_json::value::RawValue>);

impl validator::Validate for AnyOfHashMapObject
{
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnyOfHashMapObject value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AnyOfHashMapObject {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

impl PartialEq for AnyOfHashMapObject {
    fn eq(&self, other: &Self) -> bool {
        self.0.get() == other.0.get()
    }
}





/// Test a model containing an anyOf
/// Any of:
/// - String
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AnyOfObject(Box<serde_json::value::RawValue>);

impl validator::Validate for AnyOfObject
{
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnyOfObject value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AnyOfObject {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

impl PartialEq for AnyOfObject {
    fn eq(&self, other: &Self) -> bool {
        self.0.get() == other.0.get()
    }
}





/// Test containing an anyOf object
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AnyOfProperty {
    #[serde(rename = "requiredAnyOf")]
    pub required_any_of: models::AnyOfObject,

    #[serde(rename = "optionalAnyOf")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub optional_any_of: Option<models::Model12345AnyOfObject>,

}

impl AnyOfProperty {
    fn __name_for_required_any_of() -> String {
        String::from("CodegenProperty{openApiType='AnyOfObject', baseName='requiredAnyOf', complexType='AnyOfObject', getter='getRequiredAnyOf', setter='setRequiredAnyOf', description='null', dataType='models::AnyOfObject', datatypeWithEnum='models::AnyOfObject', dataFormat='null', name='required_any_of', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.requiredAnyOf;', baseType='AnyOfObject', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "$ref" : "#/components/schemas/AnyOfObject"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=true, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='requiredAnyOf', nameInPascalCase='RequiredAnyOf', nameInSnakeCase='REQUIRED_ANY_OF', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=#/components/schemas/AnyOfObject, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_required_any_of<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_required_any_of())
        }
}
impl AnyOfProperty {
    fn __name_for_optional_any_of() -> String {
        String::from("CodegenProperty{openApiType='12345AnyOfObject', baseName='optionalAnyOf', complexType='12345AnyOfObject', getter='getOptionalAnyOf', setter='setOptionalAnyOf', description='null', dataType='models::Model12345AnyOfObject', datatypeWithEnum='models::Model12345AnyOfObject', dataFormat='null', name='optional_any_of', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.optionalAnyOf;', baseType='12345AnyOfObject', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "$ref" : "#/components/schemas/12345AnyOfObject"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=true, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='optionalAnyOf', nameInPascalCase='OptionalAnyOf', nameInSnakeCase='OPTIONAL_ANY_OF', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=#/components/schemas/12345AnyOfObject, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_optional_any_of<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_optional_any_of())
        }
}


impl AnyOfProperty {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(required_any_of: models::AnyOfObject, ) -> AnyOfProperty {
        AnyOfProperty {
            required_any_of,
            optional_any_of: None,
        }
    }
}

/// Converts the AnyOfProperty value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for AnyOfProperty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping requiredAnyOf in query parameter serialization

            // Skipping optionalAnyOf in query parameter serialization

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnyOfProperty value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AnyOfProperty {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub required_any_of: Vec<models::AnyOfObject>,
            pub optional_any_of: Vec<models::Model12345AnyOfObject>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing AnyOfProperty".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "requiredAnyOf" => intermediate_rep.required_any_of.push(<models::AnyOfObject as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "optionalAnyOf" => intermediate_rep.optional_any_of.push(<models::Model12345AnyOfObject as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing AnyOfProperty".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(AnyOfProperty {
            required_any_of: intermediate_rep.required_any_of.into_iter().next().ok_or_else(|| "requiredAnyOf missing in AnyOfProperty".to_string())?,
            optional_any_of: intermediate_rep.optional_any_of.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<AnyOfProperty> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<AnyOfProperty>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AnyOfProperty>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AnyOfProperty - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<AnyOfProperty> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AnyOfProperty as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into AnyOfProperty - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




/// An XML object
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct DuplicateXmlObject {
    #[serde(rename = "inner_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub inner_string: Option<String>,

    #[serde(rename = "inner_array")]
    pub inner_array: models::XmlArray,

}

impl DuplicateXmlObject {
    fn __name_for_inner_string() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='inner_string', complexType='string', getter='getInnerString', setter='setInnerString', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner_string', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.inner_string;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='innerString', nameInPascalCase='InnerString', nameInSnakeCase='INNER_STRING', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_inner_string<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_inner_string())
        }
}
impl DuplicateXmlObject {
    fn __name_for_inner_array() -> String {
        String::from("CodegenProperty{openApiType='xml_array', baseName='inner_array', complexType='xml_array', getter='getInnerArray', setter='setInnerArray', description='null', dataType='models::XmlArray', datatypeWithEnum='models::XmlArray', dataFormat='null', name='inner_array', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner_array;', baseType='xml_array', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "$ref" : "#/components/schemas/xml_array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='innerArray', nameInPascalCase='InnerArray', nameInSnakeCase='INNER_ARRAY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='CamelXmlArray', xmlNamespace='null', isXmlWrapped=true, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=#/components/schemas/xml_array, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_inner_array<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_inner_array())
        }
}


impl DuplicateXmlObject {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(inner_array: models::XmlArray, ) -> DuplicateXmlObject {
        DuplicateXmlObject {
            inner_string: None,
            inner_array,
        }
    }
}

/// Converts the DuplicateXmlObject value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for DuplicateXmlObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.inner_string.as_ref().map(|inner_string| {
                [
                    "inner_string".to_string(),
                    inner_string.to_string(),
                ].join(",")
            }),

            // Skipping inner_array in query parameter serialization

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a DuplicateXmlObject value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for DuplicateXmlObject {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub inner_string: Vec<String>,
            pub inner_array: Vec<models::XmlArray>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing DuplicateXmlObject".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "inner_string" => intermediate_rep.inner_string.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "inner_array" => intermediate_rep.inner_array.push(<models::XmlArray as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing DuplicateXmlObject".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(DuplicateXmlObject {
            inner_string: intermediate_rep.inner_string.into_iter().next(),
            inner_array: intermediate_rep.inner_array.into_iter().next().ok_or_else(|| "inner_array missing in DuplicateXmlObject".to_string())?,
        })
    }
}

// Methods for converting between header::IntoHeaderValue<DuplicateXmlObject> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<DuplicateXmlObject>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<DuplicateXmlObject>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for DuplicateXmlObject - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<DuplicateXmlObject> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <DuplicateXmlObject as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into DuplicateXmlObject - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




/// Test a model containing a special character in the enum
/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk_enum_derive::LabelledGenericEnum))]
pub enum EnumWithStarObject {
    #[serde(rename = "FOO")]
    Foo,
    #[serde(rename = "BAR")]
    Bar,
    #[serde(rename = "*")]
    Star,
}

impl std::fmt::Display for EnumWithStarObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            EnumWithStarObject::Foo => write!(f, "FOO"),
            EnumWithStarObject::Bar => write!(f, "BAR"),
            EnumWithStarObject::Star => write!(f, "*"),
        }
    }
}

impl std::str::FromStr for EnumWithStarObject {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "FOO" => std::result::Result::Ok(EnumWithStarObject::Foo),
            "BAR" => std::result::Result::Ok(EnumWithStarObject::Bar),
            "*" => std::result::Result::Ok(EnumWithStarObject::Star),
            _ => std::result::Result::Err(format!("Value not valid: {}", s)),
        }
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Err(String);

impl validator::Validate for Err {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<String> for Err {
    fn from(x: String) -> Self {
        Err(x)
    }
}

impl std::fmt::Display for Err {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{:?}", self.0)
    }
}

impl std::str::FromStr for Err {
    type Err = std::string::ParseError;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(Err(x.to_string()))
    }
}

impl std::convert::From<Err> for String {
    fn from(x: Err) -> Self {
        x.0
    }
}

impl std::ops::Deref for Err {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for Err {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}



#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Error(String);

impl validator::Validate for Error {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<String> for Error {
    fn from(x: String) -> Self {
        Error(x)
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{:?}", self.0)
    }
}

impl std::str::FromStr for Error {
    type Err = std::string::ParseError;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(Error(x.to_string()))
    }
}

impl std::convert::From<Error> for String {
    fn from(x: Error) -> Self {
        x.0
    }
}

impl std::ops::Deref for Error {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for Error {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}



#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct FormTestRequest {
    #[serde(rename = "requiredArray")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub required_array: Option<Vec<String>>,

}

impl FormTestRequest {
    fn __name_for_required_array() -> String {
        String::from("CodegenProperty{openApiType='array', baseName='requiredArray', complexType='string', getter='getRequiredArray', setter='setRequiredArray', description='null', dataType='Vec<String>', datatypeWithEnum='Vec<String>', dataFormat='null', name='required_array', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.requiredArray;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "string"
  },
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='string', baseName='required_array', complexType='string', getter='getRequiredArray', setter='setRequiredArray', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='required_array', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.required_array;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='requiredArray', nameInPascalCase='RequiredArray', nameInSnakeCase='REQUIRED_ARRAY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='required_array', complexType='string', getter='getRequiredArray', setter='setRequiredArray', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='required_array', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.required_array;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='requiredArray', nameInPascalCase='RequiredArray', nameInSnakeCase='REQUIRED_ARRAY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='requiredArray', nameInPascalCase='RequiredArray', nameInSnakeCase='REQUIRED_ARRAY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_required_array<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_required_array())
        }
}


impl FormTestRequest {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> FormTestRequest {
        FormTestRequest {
            required_array: None,
        }
    }
}

/// Converts the FormTestRequest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for FormTestRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.required_array.as_ref().map(|required_array| {
                [
                    "requiredArray".to_string(),
                    required_array.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a FormTestRequest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for FormTestRequest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub required_array: Vec<Vec<String>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing FormTestRequest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "requiredArray" => return std::result::Result::Err("Parsing a container in this style is not supported in FormTestRequest".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing FormTestRequest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(FormTestRequest {
            required_array: intermediate_rep.required_array.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<FormTestRequest> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<FormTestRequest>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<FormTestRequest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for FormTestRequest - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<FormTestRequest> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <FormTestRequest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into FormTestRequest - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




/// Test a model containing an anyOf that starts with a number
/// Any of:
/// - String
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Model12345AnyOfObject(Box<serde_json::value::RawValue>);

impl validator::Validate for Model12345AnyOfObject
{
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Model12345AnyOfObject value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Model12345AnyOfObject {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}

impl PartialEq for Model12345AnyOfObject {
    fn eq(&self, other: &Self) -> bool {
        self.0.get() == other.0.get()
    }
}





#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct MultigetGet201Response {
    #[serde(rename = "foo")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub foo: Option<String>,

}

impl MultigetGet201Response {
    fn __name_for_foo() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='foo', complexType='string', getter='getFoo', setter='setFoo', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='foo', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.foo;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='foo', nameInPascalCase='Foo', nameInSnakeCase='FOO', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_foo<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_foo())
        }
}


impl MultigetGet201Response {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> MultigetGet201Response {
        MultigetGet201Response {
            foo: None,
        }
    }
}

/// Converts the MultigetGet201Response value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for MultigetGet201Response {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.foo.as_ref().map(|foo| {
                [
                    "foo".to_string(),
                    foo.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MultigetGet201Response value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for MultigetGet201Response {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub foo: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing MultigetGet201Response".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "foo" => intermediate_rep.foo.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing MultigetGet201Response".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(MultigetGet201Response {
            foo: intermediate_rep.foo.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<MultigetGet201Response> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<MultigetGet201Response>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<MultigetGet201Response>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for MultigetGet201Response - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<MultigetGet201Response> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <MultigetGet201Response as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into MultigetGet201Response - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct MyId(i32);

impl validator::Validate for MyId {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<i32> for MyId {
    fn from(x: i32) -> Self {
        MyId(x)
    }
}

impl std::convert::From<MyId> for i32 {
    fn from(x: MyId) -> Self {
        x.0
    }
}

impl std::ops::Deref for MyId {
    type Target = i32;
    fn deref(&self) -> &i32 {
        &self.0
    }
}

impl std::ops::DerefMut for MyId {
    fn deref_mut(&mut self) -> &mut i32 {
        &mut self.0
    }
}



#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct MyIdList(Vec<i32>);

impl validator::Validate for MyIdList {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<Vec<i32>> for MyIdList {
    fn from(x: Vec<i32>) -> Self {
        MyIdList(x)
    }
}

impl std::convert::From<MyIdList> for Vec<i32> {
    fn from(x: MyIdList) -> Self {
        x.0
    }
}

impl std::iter::FromIterator<i32> for MyIdList {
    fn from_iter<U: IntoIterator<Item=i32>>(u: U) -> Self {
        MyIdList(Vec::<i32>::from_iter(u))
    }
}

impl std::iter::IntoIterator for MyIdList {
    type Item = i32;
    type IntoIter = std::vec::IntoIter<i32>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> std::iter::IntoIterator for &'a MyIdList {
    type Item = &'a i32;
    type IntoIter = std::slice::Iter<'a, i32>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a> std::iter::IntoIterator for &'a mut MyIdList {
    type Item = &'a mut i32;
    type IntoIter = std::slice::IterMut<'a, i32>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl std::ops::Deref for MyIdList {
    type Target = Vec<i32>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for MyIdList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Converts the MyIdList value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for MyIdList {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{}", self.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MyIdList value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for MyIdList {
    type Err = <i32 as std::str::FromStr>::Err;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut items = vec![];
        for item in s.split(',')
        {
            items.push(item.parse()?);
        }
        std::result::Result::Ok(MyIdList(items))
    }
}


// Methods for converting between header::IntoHeaderValue<MyIdList> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<MyIdList>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<MyIdList>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for MyIdList - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<MyIdList> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <MyIdList as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into MyIdList - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct NullableObject(String);

impl validator::Validate for NullableObject {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<String> for NullableObject {
    fn from(x: String) -> Self {
        NullableObject(x)
    }
}

impl std::fmt::Display for NullableObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{:?}", self.0)
    }
}

impl std::str::FromStr for NullableObject {
    type Err = std::string::ParseError;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(NullableObject(x.to_string()))
    }
}

impl std::convert::From<NullableObject> for String {
    fn from(x: NullableObject) -> Self {
        x.0
    }
}

impl std::ops::Deref for NullableObject {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for NullableObject {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}



#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct NullableTest {
    #[serde(rename = "nullable")]
    pub nullable: Nullable<String>,

    #[serde(rename = "nullableWithNullDefault")]
    #[serde(deserialize_with = "deserialize_optional_nullable")]
    #[serde(default = "default_optional_nullable")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub nullable_with_null_default: Option<Nullable<String>>,

    #[serde(rename = "nullableWithPresentDefault")]
    #[serde(deserialize_with = "deserialize_optional_nullable")]
    #[serde(default = "default_optional_nullable")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub nullable_with_present_default: Option<Nullable<String>>,

    #[serde(rename = "nullableWithNoDefault")]
    #[serde(deserialize_with = "deserialize_optional_nullable")]
    #[serde(default = "default_optional_nullable")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub nullable_with_no_default: Option<Nullable<String>>,

    #[serde(rename = "nullableArray")]
    #[serde(deserialize_with = "deserialize_optional_nullable")]
    #[serde(default = "default_optional_nullable")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub nullable_array: Option<Nullable<Vec<String>>>,

    #[serde(rename = "min_item_test")]
    #[validate(
            length(min = 1),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub min_item_test: Option<Vec<i32>>,

    #[serde(rename = "max_item_test")]
    #[validate(
            length(max = 2),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub max_item_test: Option<Vec<i32>>,

    #[serde(rename = "min_max_item_test")]
    #[validate(
            length(min = 1, max = 3),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub min_max_item_test: Option<Vec<i32>>,

}

impl NullableTest {
    fn __name_for_nullable() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='nullable', complexType='string', getter='getNullable', setter='setNullable', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='nullable', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.nullable;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "nullable" : true,
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=true, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='nullable', nameInPascalCase='Nullable', nameInSnakeCase='NULLABLE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_nullable<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_nullable())
        }
}
impl NullableTest {
    fn __name_for_nullable_with_null_default() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='nullableWithNullDefault', complexType='string', getter='getNullableWithNullDefault', setter='setNullableWithNullDefault', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='nullable_with_null_default', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.nullableWithNullDefault;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "nullable" : true,
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=true, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='nullableWithNullDefault', nameInPascalCase='NullableWithNullDefault', nameInSnakeCase='NULLABLE_WITH_NULL_DEFAULT', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_nullable_with_null_default<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_nullable_with_null_default())
        }
}
impl NullableTest {
    fn __name_for_nullable_with_present_default() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='nullableWithPresentDefault', complexType='string', getter='getNullableWithPresentDefault', setter='setNullableWithPresentDefault', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='nullable_with_present_default', min='null', max='null', defaultValue='Some(Nullable::Present(r#"default"#.to_string()))', defaultValueWithParam=' = data.nullableWithPresentDefault;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "default" : "default",
  "nullable" : true,
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=true, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='nullableWithPresentDefault', nameInPascalCase='NullableWithPresentDefault', nameInSnakeCase='NULLABLE_WITH_PRESENT_DEFAULT', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_nullable_with_present_default<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_nullable_with_present_default())
        }
}
impl NullableTest {
    fn __name_for_nullable_with_no_default() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='nullableWithNoDefault', complexType='string', getter='getNullableWithNoDefault', setter='setNullableWithNoDefault', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='nullable_with_no_default', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.nullableWithNoDefault;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "nullable" : true,
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=true, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='nullableWithNoDefault', nameInPascalCase='NullableWithNoDefault', nameInSnakeCase='NULLABLE_WITH_NO_DEFAULT', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_nullable_with_no_default<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_nullable_with_no_default())
        }
}
impl NullableTest {
    fn __name_for_nullable_array() -> String {
        String::from("CodegenProperty{openApiType='array', baseName='nullableArray', complexType='string', getter='getNullableArray', setter='setNullableArray', description='null', dataType='Vec<String>', datatypeWithEnum='Vec<String>', dataFormat='null', name='nullable_array', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.nullableArray;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "string"
  },
  "nullable" : true,
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=true, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='string', baseName='nullable_array', complexType='string', getter='getNullableArray', setter='setNullableArray', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='nullable_array', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.nullable_array;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='nullableArray', nameInPascalCase='NullableArray', nameInSnakeCase='NULLABLE_ARRAY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='nullable_array', complexType='string', getter='getNullableArray', setter='setNullableArray', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='nullable_array', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.nullable_array;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='nullableArray', nameInPascalCase='NullableArray', nameInSnakeCase='NULLABLE_ARRAY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='nullableArray', nameInPascalCase='NullableArray', nameInSnakeCase='NULLABLE_ARRAY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_nullable_array<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_nullable_array())
        }
}
impl NullableTest {
    fn __name_for_min_item_test() -> String {
        String::from("CodegenProperty{openApiType='array', baseName='min_item_test', complexType='integer', getter='getMinItemTest', setter='setMinItemTest', description='null', dataType='Vec<i32>', datatypeWithEnum='Vec<i32>', dataFormat='null', name='min_item_test', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.min_item_test;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "integer"
  },
  "minItems" : 1,
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='integer', baseName='min_item_test', complexType='integer', getter='getMinItemTest', setter='setMinItemTest', description='null', dataType='i32', datatypeWithEnum='i32', dataFormat='null', name='min_item_test', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.min_item_test;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=false, isLong=false, isUnboundedInteger=true, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='minItemTest', nameInPascalCase='MinItemTest', nameInSnakeCase='MIN_ITEM_TEST', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='integer', baseName='min_item_test', complexType='integer', getter='getMinItemTest', setter='setMinItemTest', description='null', dataType='i32', datatypeWithEnum='i32', dataFormat='null', name='min_item_test', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.min_item_test;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=false, isLong=false, isUnboundedInteger=true, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='minItemTest', nameInPascalCase='MinItemTest', nameInSnakeCase='MIN_ITEM_TEST', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='minItemTest', nameInPascalCase='MinItemTest', nameInSnakeCase='MIN_ITEM_TEST', enumName='null', maxItems=null, minItems=1, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_min_item_test<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_min_item_test())
        }
}
impl NullableTest {
    fn __name_for_max_item_test() -> String {
        String::from("CodegenProperty{openApiType='array', baseName='max_item_test', complexType='integer', getter='getMaxItemTest', setter='setMaxItemTest', description='null', dataType='Vec<i32>', datatypeWithEnum='Vec<i32>', dataFormat='null', name='max_item_test', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.max_item_test;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "integer"
  },
  "maxItems" : 2,
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='integer', baseName='max_item_test', complexType='integer', getter='getMaxItemTest', setter='setMaxItemTest', description='null', dataType='i32', datatypeWithEnum='i32', dataFormat='null', name='max_item_test', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.max_item_test;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=false, isLong=false, isUnboundedInteger=true, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='maxItemTest', nameInPascalCase='MaxItemTest', nameInSnakeCase='MAX_ITEM_TEST', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='integer', baseName='max_item_test', complexType='integer', getter='getMaxItemTest', setter='setMaxItemTest', description='null', dataType='i32', datatypeWithEnum='i32', dataFormat='null', name='max_item_test', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.max_item_test;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=false, isLong=false, isUnboundedInteger=true, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='maxItemTest', nameInPascalCase='MaxItemTest', nameInSnakeCase='MAX_ITEM_TEST', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='maxItemTest', nameInPascalCase='MaxItemTest', nameInSnakeCase='MAX_ITEM_TEST', enumName='null', maxItems=2, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_max_item_test<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_max_item_test())
        }
}
impl NullableTest {
    fn __name_for_min_max_item_test() -> String {
        String::from("CodegenProperty{openApiType='array', baseName='min_max_item_test', complexType='integer', getter='getMinMaxItemTest', setter='setMinMaxItemTest', description='null', dataType='Vec<i32>', datatypeWithEnum='Vec<i32>', dataFormat='null', name='min_max_item_test', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.min_max_item_test;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "integer"
  },
  "maxItems" : 3,
  "minItems" : 1,
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='integer', baseName='min_max_item_test', complexType='integer', getter='getMinMaxItemTest', setter='setMinMaxItemTest', description='null', dataType='i32', datatypeWithEnum='i32', dataFormat='null', name='min_max_item_test', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.min_max_item_test;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=false, isLong=false, isUnboundedInteger=true, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='minMaxItemTest', nameInPascalCase='MinMaxItemTest', nameInSnakeCase='MIN_MAX_ITEM_TEST', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='integer', baseName='min_max_item_test', complexType='integer', getter='getMinMaxItemTest', setter='setMinMaxItemTest', description='null', dataType='i32', datatypeWithEnum='i32', dataFormat='null', name='min_max_item_test', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.min_max_item_test;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=false, isLong=false, isUnboundedInteger=true, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='minMaxItemTest', nameInPascalCase='MinMaxItemTest', nameInSnakeCase='MIN_MAX_ITEM_TEST', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='minMaxItemTest', nameInPascalCase='MinMaxItemTest', nameInSnakeCase='MIN_MAX_ITEM_TEST', enumName='null', maxItems=3, minItems=1, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_min_max_item_test<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_min_max_item_test())
        }
}


impl NullableTest {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(nullable: Nullable<String>, ) -> NullableTest {
        NullableTest {
            nullable,
            nullable_with_null_default: None,
            nullable_with_present_default: Some(Nullable::Present(r#"default"#.to_string())),
            nullable_with_no_default: None,
            nullable_array: None,
            min_item_test: None,
            max_item_test: None,
            min_max_item_test: None,
        }
    }
}

/// Converts the NullableTest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for NullableTest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            Some("nullable".to_string()),
            Some(self.nullable.as_ref().map_or("null".to_string(), |x| x.to_string())),


            self.nullable_with_null_default.as_ref().map(|nullable_with_null_default| {
                [
                    "nullableWithNullDefault".to_string(),
                    nullable_with_null_default.as_ref().map_or("null".to_string(), |x| x.to_string()),
                ].join(",")
            }),


            self.nullable_with_present_default.as_ref().map(|nullable_with_present_default| {
                [
                    "nullableWithPresentDefault".to_string(),
                    nullable_with_present_default.as_ref().map_or("null".to_string(), |x| x.to_string()),
                ].join(",")
            }),


            self.nullable_with_no_default.as_ref().map(|nullable_with_no_default| {
                [
                    "nullableWithNoDefault".to_string(),
                    nullable_with_no_default.as_ref().map_or("null".to_string(), |x| x.to_string()),
                ].join(",")
            }),


            self.nullable_array.as_ref().map(|nullable_array| {
                [
                    "nullableArray".to_string(),
                    nullable_array.as_ref().map_or("null".to_string(), |x| x.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",")),
                ].join(",")
            }),


            self.min_item_test.as_ref().map(|min_item_test| {
                [
                    "min_item_test".to_string(),
                    min_item_test.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","),
                ].join(",")
            }),


            self.max_item_test.as_ref().map(|max_item_test| {
                [
                    "max_item_test".to_string(),
                    max_item_test.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","),
                ].join(",")
            }),


            self.min_max_item_test.as_ref().map(|min_max_item_test| {
                [
                    "min_max_item_test".to_string(),
                    min_max_item_test.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a NullableTest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for NullableTest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub nullable: Vec<String>,
            pub nullable_with_null_default: Vec<String>,
            pub nullable_with_present_default: Vec<String>,
            pub nullable_with_no_default: Vec<String>,
            pub nullable_array: Vec<Vec<String>>,
            pub min_item_test: Vec<Vec<i32>>,
            pub max_item_test: Vec<Vec<i32>>,
            pub min_max_item_test: Vec<Vec<i32>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing NullableTest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "nullable" => return std::result::Result::Err("Parsing a nullable type in this style is not supported in NullableTest".to_string()),
                    "nullableWithNullDefault" => return std::result::Result::Err("Parsing a nullable type in this style is not supported in NullableTest".to_string()),
                    "nullableWithPresentDefault" => return std::result::Result::Err("Parsing a nullable type in this style is not supported in NullableTest".to_string()),
                    "nullableWithNoDefault" => return std::result::Result::Err("Parsing a nullable type in this style is not supported in NullableTest".to_string()),
                    "nullableArray" => return std::result::Result::Err("Parsing a container in this style is not supported in NullableTest".to_string()),
                    "min_item_test" => return std::result::Result::Err("Parsing a container in this style is not supported in NullableTest".to_string()),
                    "max_item_test" => return std::result::Result::Err("Parsing a container in this style is not supported in NullableTest".to_string()),
                    "min_max_item_test" => return std::result::Result::Err("Parsing a container in this style is not supported in NullableTest".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing NullableTest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(NullableTest {
            nullable: std::result::Result::Err("Nullable types not supported in NullableTest".to_string())?,
            nullable_with_null_default: std::result::Result::Err("Nullable types not supported in NullableTest".to_string())?,
            nullable_with_present_default: std::result::Result::Err("Nullable types not supported in NullableTest".to_string())?,
            nullable_with_no_default: std::result::Result::Err("Nullable types not supported in NullableTest".to_string())?,
            nullable_array: std::result::Result::Err("Nullable types not supported in NullableTest".to_string())?,
            min_item_test: intermediate_rep.min_item_test.into_iter().next(),
            max_item_test: intermediate_rep.max_item_test.into_iter().next(),
            min_max_item_test: intermediate_rep.min_max_item_test.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<NullableTest> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<NullableTest>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<NullableTest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for NullableTest - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<NullableTest> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <NullableTest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into NullableTest - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ObjectHeader {
    #[serde(rename = "requiredObjectHeader")]
    pub required_object_header: bool,

    #[serde(rename = "optionalObjectHeader")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub optional_object_header: Option<i32>,

}

impl ObjectHeader {
    fn __name_for_required_object_header() -> String {
        String::from("CodegenProperty{openApiType='boolean', baseName='requiredObjectHeader', complexType='boolean', getter='getRequiredObjectHeader', setter='setRequiredObjectHeader', description='null', dataType='bool', datatypeWithEnum='bool', dataFormat='null', name='required_object_header', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.requiredObjectHeader;', baseType='boolean', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "boolean"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=true, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='requiredObjectHeader', nameInPascalCase='RequiredObjectHeader', nameInSnakeCase='REQUIRED_OBJECT_HEADER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_required_object_header<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_required_object_header())
        }
}
impl ObjectHeader {
    fn __name_for_optional_object_header() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='optionalObjectHeader', complexType='integer', getter='getOptionalObjectHeader', setter='setOptionalObjectHeader', description='null', dataType='i32', datatypeWithEnum='i32', dataFormat='null', name='optional_object_header', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.optionalObjectHeader;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=false, isLong=false, isUnboundedInteger=true, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='optionalObjectHeader', nameInPascalCase='OptionalObjectHeader', nameInSnakeCase='OPTIONAL_OBJECT_HEADER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_optional_object_header<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_optional_object_header())
        }
}


impl ObjectHeader {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(required_object_header: bool, ) -> ObjectHeader {
        ObjectHeader {
            required_object_header,
            optional_object_header: None,
        }
    }
}

/// Converts the ObjectHeader value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for ObjectHeader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            Some("requiredObjectHeader".to_string()),
            Some(self.required_object_header.to_string()),


            self.optional_object_header.as_ref().map(|optional_object_header| {
                [
                    "optionalObjectHeader".to_string(),
                    optional_object_header.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectHeader value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ObjectHeader {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub required_object_header: Vec<bool>,
            pub optional_object_header: Vec<i32>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ObjectHeader".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "requiredObjectHeader" => intermediate_rep.required_object_header.push(<bool as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "optionalObjectHeader" => intermediate_rep.optional_object_header.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ObjectHeader".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ObjectHeader {
            required_object_header: intermediate_rep.required_object_header.into_iter().next().ok_or_else(|| "requiredObjectHeader missing in ObjectHeader".to_string())?,
            optional_object_header: intermediate_rep.optional_object_header.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ObjectHeader> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectHeader>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ObjectHeader>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ObjectHeader - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<ObjectHeader> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ObjectHeader as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into ObjectHeader - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ObjectParam {
    #[serde(rename = "requiredParam")]
    pub required_param: bool,

    #[serde(rename = "optionalParam")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub optional_param: Option<i32>,

}

impl ObjectParam {
    fn __name_for_required_param() -> String {
        String::from("CodegenProperty{openApiType='boolean', baseName='requiredParam', complexType='boolean', getter='getRequiredParam', setter='setRequiredParam', description='null', dataType='bool', datatypeWithEnum='bool', dataFormat='null', name='required_param', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.requiredParam;', baseType='boolean', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "boolean"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=true, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='requiredParam', nameInPascalCase='RequiredParam', nameInSnakeCase='REQUIRED_PARAM', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_required_param<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_required_param())
        }
}
impl ObjectParam {
    fn __name_for_optional_param() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='optionalParam', complexType='integer', getter='getOptionalParam', setter='setOptionalParam', description='null', dataType='i32', datatypeWithEnum='i32', dataFormat='null', name='optional_param', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.optionalParam;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=false, isLong=false, isUnboundedInteger=true, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='optionalParam', nameInPascalCase='OptionalParam', nameInSnakeCase='OPTIONAL_PARAM', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_optional_param<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_optional_param())
        }
}


impl ObjectParam {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(required_param: bool, ) -> ObjectParam {
        ObjectParam {
            required_param,
            optional_param: None,
        }
    }
}

/// Converts the ObjectParam value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for ObjectParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            Some("requiredParam".to_string()),
            Some(self.required_param.to_string()),


            self.optional_param.as_ref().map(|optional_param| {
                [
                    "optionalParam".to_string(),
                    optional_param.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectParam value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ObjectParam {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub required_param: Vec<bool>,
            pub optional_param: Vec<i32>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ObjectParam".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "requiredParam" => intermediate_rep.required_param.push(<bool as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "optionalParam" => intermediate_rep.optional_param.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ObjectParam".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ObjectParam {
            required_param: intermediate_rep.required_param.into_iter().next().ok_or_else(|| "requiredParam missing in ObjectParam".to_string())?,
            optional_param: intermediate_rep.optional_param.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ObjectParam> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectParam>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ObjectParam>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ObjectParam - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<ObjectParam> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ObjectParam as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into ObjectParam - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ObjectUntypedProps {
    #[serde(rename = "required_untyped")]
    pub required_untyped: crate::types::Object,

    #[serde(rename = "required_untyped_nullable")]
    pub required_untyped_nullable: Nullable<crate::types::Object>,

    #[serde(rename = "not_required_untyped")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub not_required_untyped: Option<crate::types::Object>,

    #[serde(rename = "not_required_untyped_nullable")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub not_required_untyped_nullable: Option<crate::types::Object>,

}

impl ObjectUntypedProps {
    fn __name_for_required_untyped() -> String {
        String::from("CodegenProperty{openApiType='AnyType', baseName='required_untyped', complexType='null', getter='getRequiredUntyped', setter='setRequiredUntyped', description='null', dataType='crate::types::Object', datatypeWithEnum='crate::types::Object', dataFormat='null', name='required_untyped', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.required_untyped;', baseType='AnyType', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "nullable" : false
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=true, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='requiredUntyped', nameInPascalCase='RequiredUntyped', nameInSnakeCase='REQUIRED_UNTYPED', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_required_untyped<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_required_untyped())
        }
}
impl ObjectUntypedProps {
    fn __name_for_required_untyped_nullable() -> String {
        String::from("CodegenProperty{openApiType='AnyType', baseName='required_untyped_nullable', complexType='null', getter='getRequiredUntypedNullable', setter='setRequiredUntypedNullable', description='null', dataType='crate::types::Object', datatypeWithEnum='crate::types::Object', dataFormat='null', name='required_untyped_nullable', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.required_untyped_nullable;', baseType='AnyType', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "nullable" : true
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=true, isReadOnly=false, isWriteOnly=false, isNullable=true, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='requiredUntypedNullable', nameInPascalCase='RequiredUntypedNullable', nameInSnakeCase='REQUIRED_UNTYPED_NULLABLE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_required_untyped_nullable<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_required_untyped_nullable())
        }
}
impl ObjectUntypedProps {
    fn __name_for_not_required_untyped() -> String {
        String::from("CodegenProperty{openApiType='AnyType', baseName='not_required_untyped', complexType='null', getter='getNotRequiredUntyped', setter='setNotRequiredUntyped', description='null', dataType='crate::types::Object', datatypeWithEnum='crate::types::Object', dataFormat='null', name='not_required_untyped', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.not_required_untyped;', baseType='AnyType', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "nullable" : false
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=true, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='notRequiredUntyped', nameInPascalCase='NotRequiredUntyped', nameInSnakeCase='NOT_REQUIRED_UNTYPED', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_not_required_untyped<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_not_required_untyped())
        }
}
impl ObjectUntypedProps {
    fn __name_for_not_required_untyped_nullable() -> String {
        String::from("CodegenProperty{openApiType='AnyType', baseName='not_required_untyped_nullable', complexType='null', getter='getNotRequiredUntypedNullable', setter='setNotRequiredUntypedNullable', description='null', dataType='crate::types::Object', datatypeWithEnum='crate::types::Object', dataFormat='null', name='not_required_untyped_nullable', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.not_required_untyped_nullable;', baseType='AnyType', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "nullable" : false
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=true, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='notRequiredUntypedNullable', nameInPascalCase='NotRequiredUntypedNullable', nameInSnakeCase='NOT_REQUIRED_UNTYPED_NULLABLE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_not_required_untyped_nullable<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_not_required_untyped_nullable())
        }
}


impl ObjectUntypedProps {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(required_untyped: crate::types::Object, required_untyped_nullable: Nullable<crate::types::Object>, ) -> ObjectUntypedProps {
        ObjectUntypedProps {
            required_untyped,
            required_untyped_nullable,
            not_required_untyped: None,
            not_required_untyped_nullable: None,
        }
    }
}

/// Converts the ObjectUntypedProps value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for ObjectUntypedProps {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping required_untyped in query parameter serialization

            // Skipping required_untyped_nullable in query parameter serialization

            // Skipping not_required_untyped in query parameter serialization

            // Skipping not_required_untyped_nullable in query parameter serialization

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectUntypedProps value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ObjectUntypedProps {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub required_untyped: Vec<crate::types::Object>,
            pub required_untyped_nullable: Vec<crate::types::Object>,
            pub not_required_untyped: Vec<crate::types::Object>,
            pub not_required_untyped_nullable: Vec<crate::types::Object>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ObjectUntypedProps".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "required_untyped" => intermediate_rep.required_untyped.push(<crate::types::Object as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    "required_untyped_nullable" => return std::result::Result::Err("Parsing a nullable type in this style is not supported in ObjectUntypedProps".to_string()),
                    #[allow(clippy::redundant_clone)]
                    "not_required_untyped" => intermediate_rep.not_required_untyped.push(<crate::types::Object as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "not_required_untyped_nullable" => intermediate_rep.not_required_untyped_nullable.push(<crate::types::Object as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ObjectUntypedProps".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ObjectUntypedProps {
            required_untyped: intermediate_rep.required_untyped.into_iter().next().ok_or_else(|| "required_untyped missing in ObjectUntypedProps".to_string())?,
            required_untyped_nullable: std::result::Result::Err("Nullable types not supported in ObjectUntypedProps".to_string())?,
            not_required_untyped: intermediate_rep.not_required_untyped.into_iter().next(),
            not_required_untyped_nullable: intermediate_rep.not_required_untyped_nullable.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ObjectUntypedProps> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectUntypedProps>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ObjectUntypedProps>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ObjectUntypedProps - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<ObjectUntypedProps> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ObjectUntypedProps as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into ObjectUntypedProps - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ObjectWithArrayOfObjects {
    #[serde(rename = "objectArray")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub object_array: Option<Vec<models::StringObject>>,

}

impl ObjectWithArrayOfObjects {
    fn __name_for_object_array() -> String {
        String::from("CodegenProperty{openApiType='array', baseName='objectArray', complexType='string', getter='getObjectArray', setter='setObjectArray', description='null', dataType='Vec<models::StringObject>', datatypeWithEnum='Vec<models::StringObject>', dataFormat='null', name='object_array', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.objectArray;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "$ref" : "#/components/schemas/StringObject"
  },
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='string', baseName='object_array', complexType='string', getter='getObjectArray', setter='setObjectArray', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='object_array', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.object_array;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='objectArray', nameInPascalCase='ObjectArray', nameInSnakeCase='OBJECT_ARRAY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='object_array', complexType='string', getter='getObjectArray', setter='setObjectArray', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='object_array', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.object_array;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='objectArray', nameInPascalCase='ObjectArray', nameInSnakeCase='OBJECT_ARRAY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='objectArray', nameInPascalCase='ObjectArray', nameInSnakeCase='OBJECT_ARRAY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_object_array<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_object_array())
        }
}


impl ObjectWithArrayOfObjects {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> ObjectWithArrayOfObjects {
        ObjectWithArrayOfObjects {
            object_array: None,
        }
    }
}

/// Converts the ObjectWithArrayOfObjects value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for ObjectWithArrayOfObjects {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.object_array.as_ref().map(|object_array| {
                [
                    "objectArray".to_string(),
                    object_array.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectWithArrayOfObjects value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ObjectWithArrayOfObjects {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub object_array: Vec<Vec<models::StringObject>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ObjectWithArrayOfObjects".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "objectArray" => return std::result::Result::Err("Parsing a container in this style is not supported in ObjectWithArrayOfObjects".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing ObjectWithArrayOfObjects".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ObjectWithArrayOfObjects {
            object_array: intermediate_rep.object_array.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ObjectWithArrayOfObjects> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectWithArrayOfObjects>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ObjectWithArrayOfObjects>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ObjectWithArrayOfObjects - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<ObjectWithArrayOfObjects> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ObjectWithArrayOfObjects as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into ObjectWithArrayOfObjects - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Ok(String);

impl validator::Validate for Ok {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<String> for Ok {
    fn from(x: String) -> Self {
        Ok(x)
    }
}

impl std::fmt::Display for Ok {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{:?}", self.0)
    }
}

impl std::str::FromStr for Ok {
    type Err = std::string::ParseError;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(Ok(x.to_string()))
    }
}

impl std::convert::From<Ok> for String {
    fn from(x: Ok) -> Self {
        x.0
    }
}

impl std::ops::Deref for Ok {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for Ok {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}



#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[serde(untagged)]
#[allow(non_camel_case_types)]
pub enum OneOfGet200Response {
    I32(Box<i32>),
    VecOfString(Box<Vec<String>>),
}

impl validator::Validate for OneOfGet200Response
{
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        match self {
            Self::I32(x) => x.validate(),
            Self::VecOfString(x) => x.validate(),
        }
    }
}




impl From<i32> for OneOfGet200Response {
    fn from(value: i32) -> Self {
        Self::I32(Box::new(value))
    }
}
impl From<Vec<String>> for OneOfGet200Response {
    fn from(value: Vec<String>) -> Self {
        Self::VecOfString(Box::new(value))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a OneOfGet200Response value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for OneOfGet200Response {
    type Err = serde_json::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        serde_json::from_str(s)
    }
}





#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct OptionalObjectHeader(i32);

impl validator::Validate for OptionalObjectHeader {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<i32> for OptionalObjectHeader {
    fn from(x: i32) -> Self {
        OptionalObjectHeader(x)
    }
}

impl std::convert::From<OptionalObjectHeader> for i32 {
    fn from(x: OptionalObjectHeader) -> Self {
        x.0
    }
}

impl std::ops::Deref for OptionalObjectHeader {
    type Target = i32;
    fn deref(&self) -> &i32 {
        &self.0
    }
}

impl std::ops::DerefMut for OptionalObjectHeader {
    fn deref_mut(&mut self) -> &mut i32 {
        &mut self.0
    }
}



#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct RequiredObjectHeader(bool);

impl validator::Validate for RequiredObjectHeader {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<bool> for RequiredObjectHeader {
    fn from(x: bool) -> Self {
        RequiredObjectHeader(x)
    }
}

impl std::convert::From<RequiredObjectHeader> for bool {
    fn from(x: RequiredObjectHeader) -> Self {
        x.0
    }
}

impl std::ops::Deref for RequiredObjectHeader {
    type Target = bool;
    fn deref(&self) -> &bool {
        &self.0
    }
}

impl std::ops::DerefMut for RequiredObjectHeader {
    fn deref_mut(&mut self) -> &mut bool {
        &mut self.0
    }
}



#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Result(String);

impl validator::Validate for Result {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<String> for Result {
    fn from(x: String) -> Self {
        Result(x)
    }
}

impl std::fmt::Display for Result {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{:?}", self.0)
    }
}

impl std::str::FromStr for Result {
    type Err = std::string::ParseError;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(Result(x.to_string()))
    }
}

impl std::convert::From<Result> for String {
    fn from(x: Result) -> Self {
        x.0
    }
}

impl std::ops::Deref for Result {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for Result {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}



/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk_enum_derive::LabelledGenericEnum))]
pub enum StringEnum {
    #[serde(rename = "FOO")]
    Foo,
    #[serde(rename = "BAR")]
    Bar,
}

impl std::fmt::Display for StringEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            StringEnum::Foo => write!(f, "FOO"),
            StringEnum::Bar => write!(f, "BAR"),
        }
    }
}

impl std::str::FromStr for StringEnum {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "FOO" => std::result::Result::Ok(StringEnum::Foo),
            "BAR" => std::result::Result::Ok(StringEnum::Bar),
            _ => std::result::Result::Err(format!("Value not valid: {}", s)),
        }
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct StringObject(String);

impl validator::Validate for StringObject {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<String> for StringObject {
    fn from(x: String) -> Self {
        StringObject(x)
    }
}

impl std::fmt::Display for StringObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{:?}", self.0)
    }
}

impl std::str::FromStr for StringObject {
    type Err = std::string::ParseError;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(StringObject(x.to_string()))
    }
}

impl std::convert::From<StringObject> for String {
    fn from(x: StringObject) -> Self {
        x.0
    }
}

impl std::ops::Deref for StringObject {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for StringObject {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}



/// Test a model containing a UUID
#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct UuidObject(uuid::Uuid);

impl validator::Validate for UuidObject {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<uuid::Uuid> for UuidObject {
    fn from(x: uuid::Uuid) -> Self {
        UuidObject(x)
    }
}

impl std::convert::From<UuidObject> for uuid::Uuid {
    fn from(x: UuidObject) -> Self {
        x.0
    }
}

impl std::ops::Deref for UuidObject {
    type Target = uuid::Uuid;
    fn deref(&self) -> &uuid::Uuid {
        &self.0
    }
}

impl std::ops::DerefMut for UuidObject {
    fn deref_mut(&mut self) -> &mut uuid::Uuid {
        &mut self.0
    }
}



#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct XmlArray(Vec<String>);

impl validator::Validate for XmlArray {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<Vec<String>> for XmlArray {
    fn from(x: Vec<String>) -> Self {
        XmlArray(x)
    }
}

impl std::convert::From<XmlArray> for Vec<String> {
    fn from(x: XmlArray) -> Self {
        x.0
    }
}

impl std::iter::FromIterator<String> for XmlArray {
    fn from_iter<U: IntoIterator<Item=String>>(u: U) -> Self {
        XmlArray(Vec::<String>::from_iter(u))
    }
}

impl std::iter::IntoIterator for XmlArray {
    type Item = String;
    type IntoIter = std::vec::IntoIter<String>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> std::iter::IntoIterator for &'a XmlArray {
    type Item = &'a String;
    type IntoIter = std::slice::Iter<'a, String>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a> std::iter::IntoIterator for &'a mut XmlArray {
    type Item = &'a mut String;
    type IntoIter = std::slice::IterMut<'a, String>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl std::ops::Deref for XmlArray {
    type Target = Vec<String>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for XmlArray {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Converts the XmlArray value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for XmlArray {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{}", self.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a XmlArray value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for XmlArray {
    type Err = <String as std::str::FromStr>::Err;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut items = vec![];
        for item in s.split(',')
        {
            items.push(item.parse()?);
        }
        std::result::Result::Ok(XmlArray(items))
    }
}


// Methods for converting between header::IntoHeaderValue<XmlArray> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<XmlArray>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<XmlArray>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for XmlArray - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<XmlArray> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <XmlArray as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into XmlArray - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct XmlInner(String);

impl validator::Validate for XmlInner {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<String> for XmlInner {
    fn from(x: String) -> Self {
        XmlInner(x)
    }
}

impl std::fmt::Display for XmlInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{:?}", self.0)
    }
}

impl std::str::FromStr for XmlInner {
    type Err = std::string::ParseError;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(XmlInner(x.to_string()))
    }
}

impl std::convert::From<XmlInner> for String {
    fn from(x: XmlInner) -> Self {
        x.0
    }
}

impl std::ops::Deref for XmlInner {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for XmlInner {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}



/// An XML object
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct XmlObject {
    #[serde(rename = "innerString")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub inner_string: Option<String>,

    #[serde(rename = "other_inner_rename")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub other_inner_rename: Option<i32>,

}

impl XmlObject {
    fn __name_for_inner_string() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='innerString', complexType='string', getter='getInnerString', setter='setInnerString', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner_string', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.innerString;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='innerString', nameInPascalCase='InnerString', nameInSnakeCase='INNER_STRING', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_inner_string<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_inner_string())
        }
}
impl XmlObject {
    fn __name_for_other_inner_rename() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='other_inner_rename', complexType='integer', getter='getOtherInnerRename', setter='setOtherInnerRename', description='null', dataType='i32', datatypeWithEnum='i32', dataFormat='null', name='other_inner_rename', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.other_inner_rename;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=false, isLong=false, isUnboundedInteger=true, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='otherInnerRename', nameInPascalCase='OtherInnerRename', nameInSnakeCase='OTHER_INNER_RENAME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_other_inner_rename<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_other_inner_rename())
        }
}


impl XmlObject {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> XmlObject {
        XmlObject {
            inner_string: None,
            other_inner_rename: None,
        }
    }
}

/// Converts the XmlObject value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for XmlObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.inner_string.as_ref().map(|inner_string| {
                [
                    "innerString".to_string(),
                    inner_string.to_string(),
                ].join(",")
            }),


            self.other_inner_rename.as_ref().map(|other_inner_rename| {
                [
                    "other_inner_rename".to_string(),
                    other_inner_rename.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a XmlObject value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for XmlObject {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub inner_string: Vec<String>,
            pub other_inner_rename: Vec<i32>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing XmlObject".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "innerString" => intermediate_rep.inner_string.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "other_inner_rename" => intermediate_rep.other_inner_rename.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing XmlObject".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(XmlObject {
            inner_string: intermediate_rep.inner_string.into_iter().next(),
            other_inner_rename: intermediate_rep.other_inner_rename.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<XmlObject> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<XmlObject>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<XmlObject>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for XmlObject - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<XmlObject> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <XmlObject as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into XmlObject - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}



