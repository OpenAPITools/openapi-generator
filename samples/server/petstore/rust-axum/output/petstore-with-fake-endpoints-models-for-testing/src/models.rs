#![allow(unused_qualifications)]

use http::HeaderValue;
use validator::Validate;

#[cfg(feature = "server")]
use crate::header;
use crate::{models, types::*};

      
      
      
      
      
      
      
      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct HyphenParamPathParams {
            /// Parameter with hyphen in name
                pub hyphen_param: String,
    }


      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct TestBodyWithQueryParamsQueryParams {
                #[serde(rename = "query")]
                pub query: String,
    }

      
      
      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
    pub struct TestEnumParametersHeaderParams {
        pub enum_header_string_array: Option<Vec<String>>,
        pub enum_header_string: Option<String>,
    }

            
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct TestEnumParametersQueryParams {
            /// Query parameter enum test (string array)
            /// Note: inline enums are not fully supported by openapi-generator
                #[serde(rename = "enum_query_string_array")]
                #[serde(skip_serializing_if="Option::is_none")]
                pub enum_query_string_array: Option<Vec<String>>,
            /// Query parameter enum test (string)
            /// Note: inline enums are not fully supported by openapi-generator
                #[serde(rename = "enum_query_string")]
                #[serde(skip_serializing_if="Option::is_none")]
                pub enum_query_string: Option<String>,
            /// Query parameter enum test (double)
            /// Note: inline enums are not fully supported by openapi-generator
                #[serde(rename = "enum_query_integer")]
                #[serde(skip_serializing_if="Option::is_none")]
                pub enum_query_integer: Option<i32>,
            /// Query parameter enum test (double)
            /// Note: inline enums are not fully supported by openapi-generator
                #[serde(rename = "enum_query_double")]
                #[serde(skip_serializing_if="Option::is_none")]
                pub enum_query_double: Option<f64>,
    }

      
      
      
      
      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
    pub struct DeletePetHeaderParams {
        pub api_key: Option<String>,
    }

            
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct DeletePetPathParams {
            /// Pet id to delete
                pub pet_id: i64,
    }


      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct FindPetsByStatusQueryParams {
            /// Status values that need to be considered for filter
            /// Note: inline enums are not fully supported by openapi-generator
                #[serde(rename = "status")]
                pub status: Vec<String>,
    }

      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct FindPetsByTagsQueryParams {
            /// Tags to filter by
                #[serde(rename = "tags")]
                pub tags: Vec<String>,
    }

      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct GetPetByIdPathParams {
            /// ID of pet to return
                pub pet_id: i64,
    }


      
      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct UpdatePetWithFormPathParams {
            /// ID of pet that needs to be updated
                pub pet_id: i64,
    }


      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct UploadFilePathParams {
            /// ID of pet to update
                pub pet_id: i64,
    }


      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct DeleteOrderPathParams {
            /// ID of the order that needs to be deleted
                pub order_id: String,
    }


      
      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct GetOrderByIdPathParams {
            /// ID of pet that needs to be fetched
                #[validate(
                        range(min = 1i64, max = 5i64),
                    )]
                pub order_id: i64,
    }


      
      
      
      
      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct DeleteUserPathParams {
            /// The name that needs to be deleted
                pub username: String,
    }


      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct GetUserByNamePathParams {
            /// The name that needs to be fetched. Use user1 for testing.
                pub username: String,
    }


      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct LoginUserQueryParams {
            /// The user name for login
                #[serde(rename = "username")]
                pub username: String,
            /// The password for login in clear text
                #[serde(rename = "password")]
                pub password: String,
    }

      
      
    #[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
    #[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))] 
    pub struct UpdateUserPathParams {
            /// name that need to be deleted
                pub username: String,
    }




#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct AdditionalPropertiesClass {
    #[serde(rename = "map_property")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map_property: Option<std::collections::HashMap<String, String>>,

    #[serde(rename = "map_of_map_property")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map_of_map_property: Option<std::collections::HashMap<String, std::collections::HashMap<String, String>>>,

}

impl AdditionalPropertiesClass {
    fn __name_for_map_property() -> String {
        String::from("CodegenProperty{openApiType='object', baseName='map_property', complexType='string', getter='getMapProperty', setter='setMapProperty', description='null', dataType='std::collections::HashMap<String, String>', datatypeWithEnum='std::collections::HashMap<String, String>', dataFormat='null', name='map_property', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.map_property;', baseType='map', containerType='map', containerTypeMapped='std::collections::HashMap', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "additionalProperties" : {
    "type" : "string"
  },
  "type" : "object"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=true, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=CodegenProperty{openApiType='string', baseName='additional_properties', complexType='string', getter='getAdditionalProperties', setter='setAdditionalProperties', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='additional_properties', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.additional_properties;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='additionalProperties', nameInPascalCase='AdditionalProperties', nameInSnakeCase='ADDITIONAL_PROPERTIES', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='mapProperty', nameInPascalCase='MapProperty', nameInSnakeCase='MAP_PROPERTY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=true, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_map_property<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_map_property())
        }
}
impl AdditionalPropertiesClass {
    fn __name_for_map_of_map_property() -> String {
        String::from("CodegenProperty{openApiType='object', baseName='map_of_map_property', complexType='map', getter='getMapOfMapProperty', setter='setMapOfMapProperty', description='null', dataType='std::collections::HashMap<String, std::collections::HashMap<String, String>>', datatypeWithEnum='std::collections::HashMap<String, std::collections::HashMap<String, String>>', dataFormat='null', name='map_of_map_property', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.map_of_map_property;', baseType='map', containerType='map', containerTypeMapped='std::collections::HashMap', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "additionalProperties" : {
    "type" : "object",
    "additionalProperties" : {
      "type" : "string"
    }
  },
  "type" : "object"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=true, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='object', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='std::collections::HashMap<String, String>', datatypeWithEnum='std::collections::HashMap<String, String>', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='map', containerType='map', containerTypeMapped='std::collections::HashMap', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "additionalProperties" : {
    "type" : "string"
  },
  "type" : "object"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=true, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=CodegenProperty{openApiType='string', baseName='additional_properties', complexType='string', getter='getAdditionalProperties', setter='setAdditionalProperties', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='additional_properties', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.additional_properties;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='additionalProperties', nameInPascalCase='AdditionalProperties', nameInSnakeCase='ADDITIONAL_PROPERTIES', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=true, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=CodegenProperty{openApiType='object', baseName='additional_properties', complexType='string', getter='getAdditionalProperties', setter='setAdditionalProperties', description='null', dataType='std::collections::HashMap<String, String>', datatypeWithEnum='std::collections::HashMap<String, String>', dataFormat='null', name='additional_properties', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.additional_properties;', baseType='map', containerType='map', containerTypeMapped='std::collections::HashMap', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "additionalProperties" : {
    "type" : "string"
  },
  "type" : "object"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=true, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=CodegenProperty{openApiType='string', baseName='additional_properties', complexType='string', getter='getAdditionalProperties', setter='setAdditionalProperties', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='additional_properties', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.additional_properties;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='additionalProperties', nameInPascalCase='AdditionalProperties', nameInSnakeCase='ADDITIONAL_PROPERTIES', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='additionalProperties', nameInPascalCase='AdditionalProperties', nameInSnakeCase='ADDITIONAL_PROPERTIES', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=true, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='mapOfMapProperty', nameInPascalCase='MapOfMapProperty', nameInSnakeCase='MAP_OF_MAP_PROPERTY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=true, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_map_of_map_property<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_map_of_map_property())
        }
}


impl AdditionalPropertiesClass {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
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
impl std::fmt::Display for AdditionalPropertiesClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping map_property in query parameter serialization

            // Skipping map_of_map_property in query parameter serialization
            // Skipping map_of_map_property in query parameter serialization

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AdditionalPropertiesClass value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AdditionalPropertiesClass {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub map_property: Vec<std::collections::HashMap<String, String>>,
            pub map_of_map_property: Vec<std::collections::HashMap<String, std::collections::HashMap<String, String>>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing AdditionalPropertiesClass".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "map_property" => return std::result::Result::Err("Parsing a container in this style is not supported in AdditionalPropertiesClass".to_string()),
                    "map_of_map_property" => return std::result::Result::Err("Parsing a container in this style is not supported in AdditionalPropertiesClass".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing AdditionalPropertiesClass".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(AdditionalPropertiesClass {
            map_property: intermediate_rep.map_property.into_iter().next(),
            map_of_map_property: intermediate_rep.map_of_map_property.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<AdditionalPropertiesClass> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<AdditionalPropertiesClass>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AdditionalPropertiesClass>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AdditionalPropertiesClass - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<AdditionalPropertiesClass> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AdditionalPropertiesClass as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into AdditionalPropertiesClass - {}",
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
pub struct Animal {
    #[serde(default = "Animal::__name_for_class_name")]
    #[serde(serialize_with = "Animal::__serialize_class_name")]
    #[serde(rename = "className")]
    pub class_name: String,

    #[serde(rename = "color")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub color: Option<String>,

}

impl Animal {
    fn __name_for_class_name() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='className', complexType='string', getter='getClassName', setter='setClassName', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='class_name', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.className;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=true, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='className', nameInPascalCase='ClassName', nameInSnakeCase='CLASS_NAME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_class_name<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_class_name())
        }
}
impl Animal {
    fn __name_for_color() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='color', complexType='string', getter='getColor', setter='setColor', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='color', min='null', max='null', defaultValue='Some(r#"red"#.to_string())', defaultValueWithParam=' = data.color;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "default" : "red",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='color', nameInPascalCase='Color', nameInSnakeCase='COLOR', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_color<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_color())
        }
}


impl Animal {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(class_name: String, ) -> Animal {
        Animal {
            class_name,
            color: Some(r#"red"#.to_string()),
        }
    }
}

/// Converts the Animal value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Animal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            Some("className".to_string()),
            Some(self.class_name.to_string()),


            self.color.as_ref().map(|color| {
                [
                    "color".to_string(),
                    color.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Animal value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Animal {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub class_name: Vec<String>,
            pub color: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Animal".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "className" => intermediate_rep.class_name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "color" => intermediate_rep.color.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Animal".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Animal {
            class_name: intermediate_rep.class_name.into_iter().next().ok_or_else(|| "className missing in Animal".to_string())?,
            color: intermediate_rep.color.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Animal> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Animal>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Animal>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Animal - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Animal> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Animal as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into Animal - {}",
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
pub struct AnimalFarm(Vec<Animal>);

impl validator::Validate for AnimalFarm {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<Vec<Animal>> for AnimalFarm {
    fn from(x: Vec<Animal>) -> Self {
        AnimalFarm(x)
    }
}

impl std::convert::From<AnimalFarm> for Vec<Animal> {
    fn from(x: AnimalFarm) -> Self {
        x.0
    }
}

impl std::iter::FromIterator<Animal> for AnimalFarm {
    fn from_iter<U: IntoIterator<Item=Animal>>(u: U) -> Self {
        AnimalFarm(Vec::<Animal>::from_iter(u))
    }
}

impl std::iter::IntoIterator for AnimalFarm {
    type Item = Animal;
    type IntoIter = std::vec::IntoIter<Animal>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> std::iter::IntoIterator for &'a AnimalFarm {
    type Item = &'a Animal;
    type IntoIter = std::slice::Iter<'a, Animal>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<'a> std::iter::IntoIterator for &'a mut AnimalFarm {
    type Item = &'a mut Animal;
    type IntoIter = std::slice::IterMut<'a, Animal>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter_mut()
    }
}

impl std::ops::Deref for AnimalFarm {
    type Target = Vec<Animal>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl std::ops::DerefMut for AnimalFarm {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Converts the AnimalFarm value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for AnimalFarm {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{}", self.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a AnimalFarm value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for AnimalFarm {
    type Err = <Animal as std::str::FromStr>::Err;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut items = vec![];
        for item in s.split(',')
        {
            items.push(item.parse()?);
        }
        std::result::Result::Ok(AnimalFarm(items))
    }
}


// Methods for converting between header::IntoHeaderValue<AnimalFarm> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<AnimalFarm>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<AnimalFarm>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for AnimalFarm - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<AnimalFarm> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <AnimalFarm as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into AnimalFarm - {}",
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
pub struct ApiResponse {
    #[serde(rename = "code")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub code: Option<i32>,

    #[serde(rename = "type")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub r#type: Option<String>,

    #[serde(rename = "message")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub message: Option<String>,

}

impl ApiResponse {
    fn __name_for_code() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='code', complexType='integer', getter='getCode', setter='setCode', description='null', dataType='i32', datatypeWithEnum='i32', dataFormat='int32', name='code', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.code;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int32",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=true, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='code', nameInPascalCase='Code', nameInSnakeCase='CODE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int32, dependentRequired=null, contains=null}")
    }

    fn __serialize_code<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_code())
        }
}
impl ApiResponse {
    fn __name_for_r#type() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='type', complexType='string', getter='getR#type', setter='setR#type', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='r#type', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.type;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='r#type', nameInPascalCase='R#type', nameInSnakeCase='R#TYPE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_r#type<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_r#type())
        }
}
impl ApiResponse {
    fn __name_for_message() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='message', complexType='string', getter='getMessage', setter='setMessage', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='message', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.message;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='message', nameInPascalCase='Message', nameInSnakeCase='MESSAGE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_message<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_message())
        }
}


impl ApiResponse {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> ApiResponse {
        ApiResponse {
            code: None,
            r#type: None,
            message: None,
        }
    }
}

/// Converts the ApiResponse value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for ApiResponse {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.code.as_ref().map(|code| {
                [
                    "code".to_string(),
                    code.to_string(),
                ].join(",")
            }),


            self.r#type.as_ref().map(|r#type| {
                [
                    "type".to_string(),
                    r#type.to_string(),
                ].join(",")
            }),


            self.message.as_ref().map(|message| {
                [
                    "message".to_string(),
                    message.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ApiResponse value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ApiResponse {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub code: Vec<i32>,
            pub r#type: Vec<String>,
            pub message: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ApiResponse".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "code" => intermediate_rep.code.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "type" => intermediate_rep.r#type.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "message" => intermediate_rep.message.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ApiResponse".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ApiResponse {
            code: intermediate_rep.code.into_iter().next(),
            r#type: intermediate_rep.r#type.into_iter().next(),
            message: intermediate_rep.message.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ApiResponse> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<ApiResponse>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ApiResponse>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ApiResponse - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<ApiResponse> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ApiResponse as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into ApiResponse - {}",
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
pub struct ArrayOfArrayOfNumberOnly {
    #[serde(rename = "ArrayArrayNumber")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_array_number: Option<Vec<Vec<f64>>>,

}

impl ArrayOfArrayOfNumberOnly {
    fn __name_for_array_array_number() -> String {
        String::from("CodegenProperty{openApiType='array', baseName='ArrayArrayNumber', complexType='array', getter='getArrayArrayNumber', setter='setArrayArrayNumber', description='null', dataType='Vec<Vec<f64>>', datatypeWithEnum='Vec<Vec<f64>>', dataFormat='null', name='array_array_number', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.ArrayArrayNumber;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "array",
    "items" : {
      "type" : "number"
    }
  },
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='array', baseName='array_array_number', complexType='number', getter='getArrayArrayNumber', setter='setArrayArrayNumber', description='null', dataType='Vec<f64>', datatypeWithEnum='Vec<f64>', dataFormat='null', name='array_array_number', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_array_number;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "number"
  },
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='number', baseName='array_array_number', complexType='number', getter='getArrayArrayNumber', setter='setArrayArrayNumber', description='null', dataType='f64', datatypeWithEnum='f64', dataFormat='null', name='array_array_number', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_array_number;', baseType='number', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "number"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=true, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayNumber', nameInPascalCase='ArrayArrayNumber', nameInSnakeCase='ARRAY_ARRAY_NUMBER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='number', baseName='array_array_number', complexType='number', getter='getArrayArrayNumber', setter='setArrayArrayNumber', description='null', dataType='f64', datatypeWithEnum='f64', dataFormat='null', name='array_array_number', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_array_number;', baseType='number', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "number"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=true, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayNumber', nameInPascalCase='ArrayArrayNumber', nameInSnakeCase='ARRAY_ARRAY_NUMBER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayNumber', nameInPascalCase='ArrayArrayNumber', nameInSnakeCase='ARRAY_ARRAY_NUMBER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='number', baseName='array_array_number', complexType='number', getter='getArrayArrayNumber', setter='setArrayArrayNumber', description='null', dataType='f64', datatypeWithEnum='f64', dataFormat='null', name='array_array_number', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_array_number;', baseType='number', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "number"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=true, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayNumber', nameInPascalCase='ArrayArrayNumber', nameInSnakeCase='ARRAY_ARRAY_NUMBER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayNumber', nameInPascalCase='ArrayArrayNumber', nameInSnakeCase='ARRAY_ARRAY_NUMBER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_array_array_number<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_array_array_number())
        }
}


impl ArrayOfArrayOfNumberOnly {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> ArrayOfArrayOfNumberOnly {
        ArrayOfArrayOfNumberOnly {
            array_array_number: None,
        }
    }
}

/// Converts the ArrayOfArrayOfNumberOnly value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for ArrayOfArrayOfNumberOnly {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping ArrayArrayNumber in query parameter serialization

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ArrayOfArrayOfNumberOnly value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ArrayOfArrayOfNumberOnly {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub array_array_number: Vec<Vec<Vec<f64>>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ArrayOfArrayOfNumberOnly".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "ArrayArrayNumber" => return std::result::Result::Err("Parsing a container in this style is not supported in ArrayOfArrayOfNumberOnly".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing ArrayOfArrayOfNumberOnly".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ArrayOfArrayOfNumberOnly {
            array_array_number: intermediate_rep.array_array_number.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ArrayOfArrayOfNumberOnly> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<ArrayOfArrayOfNumberOnly>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ArrayOfArrayOfNumberOnly>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ArrayOfArrayOfNumberOnly - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<ArrayOfArrayOfNumberOnly> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ArrayOfArrayOfNumberOnly as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into ArrayOfArrayOfNumberOnly - {}",
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
pub struct ArrayOfNumberOnly {
    #[serde(rename = "ArrayNumber")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_number: Option<Vec<f64>>,

}

impl ArrayOfNumberOnly {
    fn __name_for_array_number() -> String {
        String::from("CodegenProperty{openApiType='array', baseName='ArrayNumber', complexType='number', getter='getArrayNumber', setter='setArrayNumber', description='null', dataType='Vec<f64>', datatypeWithEnum='Vec<f64>', dataFormat='null', name='array_number', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.ArrayNumber;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "number"
  },
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='number', baseName='array_number', complexType='number', getter='getArrayNumber', setter='setArrayNumber', description='null', dataType='f64', datatypeWithEnum='f64', dataFormat='null', name='array_number', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_number;', baseType='number', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "number"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=true, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayNumber', nameInPascalCase='ArrayNumber', nameInSnakeCase='ARRAY_NUMBER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='number', baseName='array_number', complexType='number', getter='getArrayNumber', setter='setArrayNumber', description='null', dataType='f64', datatypeWithEnum='f64', dataFormat='null', name='array_number', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_number;', baseType='number', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "number"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=true, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayNumber', nameInPascalCase='ArrayNumber', nameInSnakeCase='ARRAY_NUMBER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayNumber', nameInPascalCase='ArrayNumber', nameInSnakeCase='ARRAY_NUMBER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_array_number<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_array_number())
        }
}


impl ArrayOfNumberOnly {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> ArrayOfNumberOnly {
        ArrayOfNumberOnly {
            array_number: None,
        }
    }
}

/// Converts the ArrayOfNumberOnly value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for ArrayOfNumberOnly {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.array_number.as_ref().map(|array_number| {
                [
                    "ArrayNumber".to_string(),
                    array_number.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ArrayOfNumberOnly value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ArrayOfNumberOnly {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub array_number: Vec<Vec<f64>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ArrayOfNumberOnly".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "ArrayNumber" => return std::result::Result::Err("Parsing a container in this style is not supported in ArrayOfNumberOnly".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing ArrayOfNumberOnly".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ArrayOfNumberOnly {
            array_number: intermediate_rep.array_number.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ArrayOfNumberOnly> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<ArrayOfNumberOnly>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ArrayOfNumberOnly>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ArrayOfNumberOnly - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<ArrayOfNumberOnly> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ArrayOfNumberOnly as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into ArrayOfNumberOnly - {}",
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

        /// Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "array_of_enum")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_of_enum: Option<Vec<String>>,

}

impl ArrayTest {
    fn __name_for_array_of_string() -> String {
        String::from("CodegenProperty{openApiType='array', baseName='array_of_string', complexType='string', getter='getArrayOfString', setter='setArrayOfString', description='null', dataType='Vec<String>', datatypeWithEnum='Vec<String>', dataFormat='null', name='array_of_string', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.array_of_string;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "string"
  },
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='string', baseName='array_of_string', complexType='string', getter='getArrayOfString', setter='setArrayOfString', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='array_of_string', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_of_string;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayOfString', nameInPascalCase='ArrayOfString', nameInSnakeCase='ARRAY_OF_STRING', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='array_of_string', complexType='string', getter='getArrayOfString', setter='setArrayOfString', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='array_of_string', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_of_string;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayOfString', nameInPascalCase='ArrayOfString', nameInSnakeCase='ARRAY_OF_STRING', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayOfString', nameInPascalCase='ArrayOfString', nameInSnakeCase='ARRAY_OF_STRING', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_array_of_string<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_array_of_string())
        }
}
impl ArrayTest {
    fn __name_for_array_array_of_integer() -> String {
        String::from("CodegenProperty{openApiType='array', baseName='array_array_of_integer', complexType='array', getter='getArrayArrayOfInteger', setter='setArrayArrayOfInteger', description='null', dataType='Vec<Vec<i64>>', datatypeWithEnum='Vec<Vec<i64>>', dataFormat='int64', name='array_array_of_integer', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.array_array_of_integer;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "array",
    "items" : {
      "type" : "integer",
      "format" : "int64"
    }
  },
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='array', baseName='array_array_of_integer', complexType='long', getter='getArrayArrayOfInteger', setter='setArrayArrayOfInteger', description='null', dataType='Vec<i64>', datatypeWithEnum='Vec<i64>', dataFormat='int64', name='array_array_of_integer', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_array_of_integer;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "integer",
    "format" : "int64"
  },
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='integer', baseName='array_array_of_integer', complexType='long', getter='getArrayArrayOfInteger', setter='setArrayArrayOfInteger', description='null', dataType='i64', datatypeWithEnum='i64', dataFormat='int64', name='array_array_of_integer', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_array_of_integer;', baseType='long', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int64",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=true, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayOfInteger', nameInPascalCase='ArrayArrayOfInteger', nameInSnakeCase='ARRAY_ARRAY_OF_INTEGER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int64, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='integer', baseName='array_array_of_integer', complexType='long', getter='getArrayArrayOfInteger', setter='setArrayArrayOfInteger', description='null', dataType='i64', datatypeWithEnum='i64', dataFormat='int64', name='array_array_of_integer', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_array_of_integer;', baseType='long', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int64",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=true, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayOfInteger', nameInPascalCase='ArrayArrayOfInteger', nameInSnakeCase='ARRAY_ARRAY_OF_INTEGER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int64, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayOfInteger', nameInPascalCase='ArrayArrayOfInteger', nameInSnakeCase='ARRAY_ARRAY_OF_INTEGER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='integer', baseName='array_array_of_integer', complexType='long', getter='getArrayArrayOfInteger', setter='setArrayArrayOfInteger', description='null', dataType='i64', datatypeWithEnum='i64', dataFormat='int64', name='array_array_of_integer', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_array_of_integer;', baseType='long', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int64",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=true, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayOfInteger', nameInPascalCase='ArrayArrayOfInteger', nameInSnakeCase='ARRAY_ARRAY_OF_INTEGER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int64, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayOfInteger', nameInPascalCase='ArrayArrayOfInteger', nameInSnakeCase='ARRAY_ARRAY_OF_INTEGER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_array_array_of_integer<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_array_array_of_integer())
        }
}
impl ArrayTest {
    fn __name_for_array_array_of_model() -> String {
        String::from("CodegenProperty{openApiType='array', baseName='array_array_of_model', complexType='array', getter='getArrayArrayOfModel', setter='setArrayArrayOfModel', description='null', dataType='Vec<Vec<models::ReadOnlyFirst>>', datatypeWithEnum='Vec<Vec<models::ReadOnlyFirst>>', dataFormat='null', name='array_array_of_model', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.array_array_of_model;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "array",
    "items" : {
      "$ref" : "#/components/schemas/ReadOnlyFirst"
    }
  },
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='array', baseName='array_array_of_model', complexType='ReadOnlyFirst', getter='getArrayArrayOfModel', setter='setArrayArrayOfModel', description='null', dataType='Vec<models::ReadOnlyFirst>', datatypeWithEnum='Vec<models::ReadOnlyFirst>', dataFormat='null', name='array_array_of_model', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_array_of_model;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "$ref" : "#/components/schemas/ReadOnlyFirst"
  },
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='ReadOnlyFirst', baseName='array_array_of_model', complexType='ReadOnlyFirst', getter='getArrayArrayOfModel', setter='setArrayArrayOfModel', description='null', dataType='models::ReadOnlyFirst', datatypeWithEnum='models::ReadOnlyFirst', dataFormat='null', name='array_array_of_model', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_array_of_model;', baseType='ReadOnlyFirst', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "$ref" : "#/components/schemas/ReadOnlyFirst"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=true, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayOfModel', nameInPascalCase='ArrayArrayOfModel', nameInSnakeCase='ARRAY_ARRAY_OF_MODEL', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=#/components/schemas/ReadOnlyFirst, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='ReadOnlyFirst', baseName='array_array_of_model', complexType='ReadOnlyFirst', getter='getArrayArrayOfModel', setter='setArrayArrayOfModel', description='null', dataType='models::ReadOnlyFirst', datatypeWithEnum='models::ReadOnlyFirst', dataFormat='null', name='array_array_of_model', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_array_of_model;', baseType='ReadOnlyFirst', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "$ref" : "#/components/schemas/ReadOnlyFirst"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=true, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayOfModel', nameInPascalCase='ArrayArrayOfModel', nameInSnakeCase='ARRAY_ARRAY_OF_MODEL', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=#/components/schemas/ReadOnlyFirst, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayOfModel', nameInPascalCase='ArrayArrayOfModel', nameInSnakeCase='ARRAY_ARRAY_OF_MODEL', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='ReadOnlyFirst', baseName='array_array_of_model', complexType='ReadOnlyFirst', getter='getArrayArrayOfModel', setter='setArrayArrayOfModel', description='null', dataType='models::ReadOnlyFirst', datatypeWithEnum='models::ReadOnlyFirst', dataFormat='null', name='array_array_of_model', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_array_of_model;', baseType='ReadOnlyFirst', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "$ref" : "#/components/schemas/ReadOnlyFirst"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=true, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayOfModel', nameInPascalCase='ArrayArrayOfModel', nameInSnakeCase='ARRAY_ARRAY_OF_MODEL', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=#/components/schemas/ReadOnlyFirst, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayOfModel', nameInPascalCase='ArrayArrayOfModel', nameInSnakeCase='ARRAY_ARRAY_OF_MODEL', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_array_array_of_model<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_array_array_of_model())
        }
}
impl ArrayTest {
    fn __name_for_array_of_enum() -> String {
        String::from("CodegenProperty{openApiType='array', baseName='array_of_enum', complexType='string', getter='getArrayOfEnum', setter='setArrayOfEnum', description='null', dataType='Vec<String>', datatypeWithEnum='Vec<String>', dataFormat='null', name='array_of_enum', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.array_of_enum;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "string",
    "enum" : [ "UPPER", "lower" ]
  },
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues={enumVars=[{name=Upper, isString=true, value="UPPER"}, {name=Lower, isString=true, value="lower"}], values=[UPPER, lower]}, items=CodegenProperty{openApiType='string', baseName='array_of_enum', complexType='string', getter='getArrayOfEnum', setter='setArrayOfEnum', description='null', dataType='String', datatypeWithEnum='ArrayOfEnum', dataFormat='null', name='array_of_enum', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_of_enum;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "UPPER", "lower" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[UPPER, lower], allowableValues={values=[UPPER, lower], enumVars=[{name=Upper, isString=true, value="UPPER"}, {name=Lower, isString=true, value="lower"}]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayOfEnum', nameInPascalCase='ArrayOfEnum', nameInSnakeCase='ARRAY_OF_ENUM', enumName='ArrayOfEnum', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='array_of_enum', complexType='string', getter='getArrayOfEnum', setter='setArrayOfEnum', description='null', dataType='String', datatypeWithEnum='ArrayOfEnum', dataFormat='null', name='array_of_enum', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_of_enum;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "UPPER", "lower" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[UPPER, lower], allowableValues={values=[UPPER, lower], enumVars=[{name=Upper, isString=true, value="UPPER"}, {name=Lower, isString=true, value="lower"}]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayOfEnum', nameInPascalCase='ArrayOfEnum', nameInSnakeCase='ARRAY_OF_ENUM', enumName='ArrayOfEnum', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayOfEnum', nameInPascalCase='ArrayOfEnum', nameInSnakeCase='ARRAY_OF_ENUM', enumName='ArrayOfEnum', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_array_of_enum<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_array_of_enum())
        }
}


impl ArrayTest {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
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
impl std::fmt::Display for ArrayTest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.array_of_string.as_ref().map(|array_of_string| {
                [
                    "array_of_string".to_string(),
                    array_of_string.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","),
                ].join(",")
            }),

            // Skipping array_array_of_integer in query parameter serialization

            // Skipping array_array_of_model in query parameter serialization


            self.array_of_enum.as_ref().map(|array_of_enum| {
                [
                    "array_of_enum".to_string(),
                    array_of_enum.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ArrayTest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ArrayTest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub array_of_string: Vec<Vec<String>>,
            pub array_array_of_integer: Vec<Vec<Vec<i64>>>,
            pub array_array_of_model: Vec<Vec<Vec<models::ReadOnlyFirst>>>,
            pub array_of_enum: Vec<Vec<String>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ArrayTest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "array_of_string" => return std::result::Result::Err("Parsing a container in this style is not supported in ArrayTest".to_string()),
                    "array_array_of_integer" => return std::result::Result::Err("Parsing a container in this style is not supported in ArrayTest".to_string()),
                    "array_array_of_model" => return std::result::Result::Err("Parsing a container in this style is not supported in ArrayTest".to_string()),
                    "array_of_enum" => return std::result::Result::Err("Parsing a container in this style is not supported in ArrayTest".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing ArrayTest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ArrayTest {
            array_of_string: intermediate_rep.array_of_string.into_iter().next(),
            array_array_of_integer: intermediate_rep.array_array_of_integer.into_iter().next(),
            array_array_of_model: intermediate_rep.array_array_of_model.into_iter().next(),
            array_of_enum: intermediate_rep.array_of_enum.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ArrayTest> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<ArrayTest>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ArrayTest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ArrayTest - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<ArrayTest> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ArrayTest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into ArrayTest - {}",
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
    fn __name_for_small_camel() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='smallCamel', complexType='string', getter='getSmallCamel', setter='setSmallCamel', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='small_camel', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.smallCamel;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='smallCamel', nameInPascalCase='SmallCamel', nameInSnakeCase='SMALL_CAMEL', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_small_camel<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_small_camel())
        }
}
impl Capitalization {
    fn __name_for_capital_camel() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='CapitalCamel', complexType='string', getter='getCapitalCamel', setter='setCapitalCamel', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='capital_camel', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.CapitalCamel;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='capitalCamel', nameInPascalCase='CapitalCamel', nameInSnakeCase='CAPITAL_CAMEL', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_capital_camel<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_capital_camel())
        }
}
impl Capitalization {
    fn __name_for_small_snake() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='small_Snake', complexType='string', getter='getSmallSnake', setter='setSmallSnake', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='small_snake', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.small_Snake;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='smallSnake', nameInPascalCase='SmallSnake', nameInSnakeCase='SMALL_SNAKE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_small_snake<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_small_snake())
        }
}
impl Capitalization {
    fn __name_for_capital_snake() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='Capital_Snake', complexType='string', getter='getCapitalSnake', setter='setCapitalSnake', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='capital_snake', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.Capital_Snake;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='capitalSnake', nameInPascalCase='CapitalSnake', nameInSnakeCase='CAPITAL_SNAKE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_capital_snake<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_capital_snake())
        }
}
impl Capitalization {
    fn __name_for_sca_eth_flow_points() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='SCA_ETH_Flow_Points', complexType='string', getter='getScaEthFlowPoints', setter='setScaEthFlowPoints', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='sca_eth_flow_points', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.SCA_ETH_Flow_Points;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='scaEthFlowPoints', nameInPascalCase='ScaEthFlowPoints', nameInSnakeCase='SCA_ETH_FLOW_POINTS', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_sca_eth_flow_points<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_sca_eth_flow_points())
        }
}
impl Capitalization {
    fn __name_for_att_name() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='ATT_NAME', complexType='string', getter='getAttName', setter='setAttName', description='Name of the pet ', dataType='String', datatypeWithEnum='String', dataFormat='null', name='att_name', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.ATT_NAME;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='Name of the pet
', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "Name of the pet\n",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='attName', nameInPascalCase='AttName', nameInSnakeCase='ATT_NAME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_att_name<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_att_name())
        }
}


impl Capitalization {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
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
impl std::fmt::Display for Capitalization {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.small_camel.as_ref().map(|small_camel| {
                [
                    "smallCamel".to_string(),
                    small_camel.to_string(),
                ].join(",")
            }),


            self.capital_camel.as_ref().map(|capital_camel| {
                [
                    "CapitalCamel".to_string(),
                    capital_camel.to_string(),
                ].join(",")
            }),


            self.small_snake.as_ref().map(|small_snake| {
                [
                    "small_Snake".to_string(),
                    small_snake.to_string(),
                ].join(",")
            }),


            self.capital_snake.as_ref().map(|capital_snake| {
                [
                    "Capital_Snake".to_string(),
                    capital_snake.to_string(),
                ].join(",")
            }),


            self.sca_eth_flow_points.as_ref().map(|sca_eth_flow_points| {
                [
                    "SCA_ETH_Flow_Points".to_string(),
                    sca_eth_flow_points.to_string(),
                ].join(",")
            }),


            self.att_name.as_ref().map(|att_name| {
                [
                    "ATT_NAME".to_string(),
                    att_name.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Capitalization value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Capitalization {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
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
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Capitalization".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "smallCamel" => intermediate_rep.small_camel.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "CapitalCamel" => intermediate_rep.capital_camel.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "small_Snake" => intermediate_rep.small_snake.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "Capital_Snake" => intermediate_rep.capital_snake.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "SCA_ETH_Flow_Points" => intermediate_rep.sca_eth_flow_points.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "ATT_NAME" => intermediate_rep.att_name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Capitalization".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Capitalization {
            small_camel: intermediate_rep.small_camel.into_iter().next(),
            capital_camel: intermediate_rep.capital_camel.into_iter().next(),
            small_snake: intermediate_rep.small_snake.into_iter().next(),
            capital_snake: intermediate_rep.capital_snake.into_iter().next(),
            sca_eth_flow_points: intermediate_rep.sca_eth_flow_points.into_iter().next(),
            att_name: intermediate_rep.att_name.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Capitalization> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Capitalization>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Capitalization>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Capitalization - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Capitalization> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Capitalization as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into Capitalization - {}",
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
    fn __name_for_class_name() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='className', complexType='string', getter='getClassName', setter='setClassName', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='class_name', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.className;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=true, isOverridden=false, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='className', nameInPascalCase='ClassName', nameInSnakeCase='CLASS_NAME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_class_name<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_class_name())
        }
}
impl Cat {
    fn __name_for_color() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='color', complexType='string', getter='getColor', setter='setColor', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='color', min='null', max='null', defaultValue='Some(r#"red"#.to_string())', defaultValueWithParam=' = data.color;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "default" : "red",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=false, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='color', nameInPascalCase='Color', nameInSnakeCase='COLOR', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_color<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_color())
        }
}
impl Cat {
    fn __name_for_declawed() -> String {
        String::from("CodegenProperty{openApiType='boolean', baseName='declawed', complexType='boolean', getter='getDeclawed', setter='setDeclawed', description='null', dataType='bool', datatypeWithEnum='bool', dataFormat='null', name='declawed', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.declawed;', baseType='boolean', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "boolean"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=true, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=false, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='declawed', nameInPascalCase='Declawed', nameInSnakeCase='DECLAWED', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_declawed<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_declawed())
        }
}


impl Cat {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(class_name: String, ) -> Cat {
        Cat {
            class_name,
            color: Some(r#"red"#.to_string()),
            declawed: None,
        }
    }
}

/// Converts the Cat value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Cat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            Some("className".to_string()),
            Some(self.class_name.to_string()),


            self.color.as_ref().map(|color| {
                [
                    "color".to_string(),
                    color.to_string(),
                ].join(",")
            }),


            self.declawed.as_ref().map(|declawed| {
                [
                    "declawed".to_string(),
                    declawed.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Cat value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Cat {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub class_name: Vec<String>,
            pub color: Vec<String>,
            pub declawed: Vec<bool>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Cat".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "className" => intermediate_rep.class_name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "color" => intermediate_rep.color.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "declawed" => intermediate_rep.declawed.push(<bool as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Cat".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Cat {
            class_name: intermediate_rep.class_name.into_iter().next().ok_or_else(|| "className missing in Cat".to_string())?,
            color: intermediate_rep.color.into_iter().next(),
            declawed: intermediate_rep.declawed.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Cat> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Cat>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Cat>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Cat - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Cat> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Cat as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into Cat - {}",
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
pub struct Category {
    #[serde(rename = "id")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub id: Option<i64>,

    #[serde(rename = "name")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub name: Option<String>,

}

impl Category {
    fn __name_for_id() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='id', complexType='long', getter='getId', setter='setId', description='null', dataType='i64', datatypeWithEnum='i64', dataFormat='int64', name='id', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.id;', baseType='long', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int64",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=true, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='id', nameInPascalCase='Id', nameInSnakeCase='ID', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int64, dependentRequired=null, contains=null}")
    }

    fn __serialize_id<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_id())
        }
}
impl Category {
    fn __name_for_name() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='name', complexType='string', getter='getName', setter='setName', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='name', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.name;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='name', nameInPascalCase='Name', nameInSnakeCase='NAME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_name<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_name())
        }
}


impl Category {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
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
impl std::fmt::Display for Category {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.id.as_ref().map(|id| {
                [
                    "id".to_string(),
                    id.to_string(),
                ].join(",")
            }),


            self.name.as_ref().map(|name| {
                [
                    "name".to_string(),
                    name.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Category value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Category {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub id: Vec<i64>,
            pub name: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Category".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "id" => intermediate_rep.id.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "name" => intermediate_rep.name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Category".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Category {
            id: intermediate_rep.id.into_iter().next(),
            name: intermediate_rep.name.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Category> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Category>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Category>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Category - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Category> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Category as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into Category - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




/// Model for testing model with \"_class\" property
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct ClassModel {
    #[serde(rename = "_class")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub _class: Option<String>,

}

impl ClassModel {
    fn __name_for__class() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='_class', complexType='string', getter='getClass', setter='setClass', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='_class', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data._class;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='class', nameInPascalCase='Class', nameInSnakeCase='CLASS', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize__class<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for__class())
        }
}


impl ClassModel {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> ClassModel {
        ClassModel {
            _class: None,
        }
    }
}

/// Converts the ClassModel value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for ClassModel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self._class.as_ref().map(|_class| {
                [
                    "_class".to_string(),
                    _class.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ClassModel value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ClassModel {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub _class: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ClassModel".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "_class" => intermediate_rep._class.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ClassModel".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ClassModel {
            _class: intermediate_rep._class.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ClassModel> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<ClassModel>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ClassModel>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ClassModel - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<ClassModel> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ClassModel as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into ClassModel - {}",
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
pub struct Client {
    #[serde(rename = "client")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub client: Option<String>,

}

impl Client {
    fn __name_for_client() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='client', complexType='string', getter='getClient', setter='setClient', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='client', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.client;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='client', nameInPascalCase='Client', nameInSnakeCase='CLIENT', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_client<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_client())
        }
}


impl Client {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> Client {
        Client {
            client: None,
        }
    }
}

/// Converts the Client value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Client {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.client.as_ref().map(|client| {
                [
                    "client".to_string(),
                    client.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Client value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Client {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub client: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Client".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "client" => intermediate_rep.client.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Client".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Client {
            client: intermediate_rep.client.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Client> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Client>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Client>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Client - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Client> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Client as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into Client - {}",
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
    fn __name_for_class_name() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='className', complexType='string', getter='getClassName', setter='setClassName', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='class_name', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.className;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=true, isOverridden=false, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='className', nameInPascalCase='ClassName', nameInSnakeCase='CLASS_NAME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_class_name<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_class_name())
        }
}
impl Dog {
    fn __name_for_color() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='color', complexType='string', getter='getColor', setter='setColor', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='color', min='null', max='null', defaultValue='Some(r#"red"#.to_string())', defaultValueWithParam=' = data.color;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "default" : "red",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=false, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='color', nameInPascalCase='Color', nameInSnakeCase='COLOR', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_color<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_color())
        }
}
impl Dog {
    fn __name_for_breed() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='breed', complexType='string', getter='getBreed', setter='setBreed', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='breed', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.breed;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=false, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='breed', nameInPascalCase='Breed', nameInSnakeCase='BREED', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_breed<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_breed())
        }
}


impl Dog {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(class_name: String, ) -> Dog {
        Dog {
            class_name,
            color: Some(r#"red"#.to_string()),
            breed: None,
        }
    }
}

/// Converts the Dog value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Dog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            Some("className".to_string()),
            Some(self.class_name.to_string()),


            self.color.as_ref().map(|color| {
                [
                    "color".to_string(),
                    color.to_string(),
                ].join(",")
            }),


            self.breed.as_ref().map(|breed| {
                [
                    "breed".to_string(),
                    breed.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Dog value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Dog {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub class_name: Vec<String>,
            pub color: Vec<String>,
            pub breed: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Dog".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "className" => intermediate_rep.class_name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "color" => intermediate_rep.color.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "breed" => intermediate_rep.breed.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Dog".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Dog {
            class_name: intermediate_rep.class_name.into_iter().next().ok_or_else(|| "className missing in Dog".to_string())?,
            color: intermediate_rep.color.into_iter().next(),
            breed: intermediate_rep.breed.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Dog> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Dog>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Dog>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Dog - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Dog> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Dog as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into Dog - {}",
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
pub struct DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket {
    #[serde(rename = "$special[property.name]")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub dollar_special_left_square_bracket_property_period_name_right_square_bracket: Option<i64>,

}

impl DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket {
    fn __name_for_dollar_special_left_square_bracket_property_period_name_right_square_bracket() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='$special[property.name]', complexType='long', getter='getDollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket', setter='setDollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket', description='null', dataType='i64', datatypeWithEnum='i64', dataFormat='int64', name='dollar_special_left_square_bracket_property_period_name_right_square_bracket', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.$special[property.name];', baseType='long', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int64",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=true, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket', nameInPascalCase='DollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket', nameInSnakeCase='DOLLAR_SPECIAL_LEFT_SQUARE_BRACKET_PROPERTY_PERIOD_NAME_RIGHT_SQUARE_BRACKET', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int64, dependentRequired=null, contains=null}")
    }

    fn __serialize_dollar_special_left_square_bracket_property_period_name_right_square_bracket<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_dollar_special_left_square_bracket_property_period_name_right_square_bracket())
        }
}


impl DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket {
        DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket {
            dollar_special_left_square_bracket_property_period_name_right_square_bracket: None,
        }
    }
}

/// Converts the DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.dollar_special_left_square_bracket_property_period_name_right_square_bracket.as_ref().map(|dollar_special_left_square_bracket_property_period_name_right_square_bracket| {
                [
                    "$special[property.name]".to_string(),
                    dollar_special_left_square_bracket_property_period_name_right_square_bracket.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub dollar_special_left_square_bracket_property_period_name_right_square_bracket: Vec<i64>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "$special[property.name]" => intermediate_rep.dollar_special_left_square_bracket_property_period_name_right_square_bracket.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket {
            dollar_special_left_square_bracket_property_period_name_right_square_bracket: intermediate_rep.dollar_special_left_square_bracket_property_period_name_right_square_bracket.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into DollarSpecialLeftSquareBracketModelPeriodNameRightSquareBracket - {}",
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
pub struct EnumArrays {
        /// Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "just_symbol")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub just_symbol: Option<String>,

        /// Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "array_enum")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_enum: Option<Vec<String>>,

        /// Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "array_array_enum")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub array_array_enum: Option<Vec<Vec<String>>>,

}

impl EnumArrays {
    fn __name_for_just_symbol() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='just_symbol', complexType='string', getter='getJustSymbol', setter='setJustSymbol', description='null', dataType='String', datatypeWithEnum='JustSymbol', dataFormat='null', name='just_symbol', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.just_symbol;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ ">=", "$" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[>=, $], allowableValues={enumVars=[{name=GreaterThanEqual, isString=true, value=">="}, {name=Dollar, isString=true, value="$"}], values=[>=, $]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='justSymbol', nameInPascalCase='JustSymbol', nameInSnakeCase='JUST_SYMBOL', enumName='JustSymbol', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_just_symbol<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_just_symbol())
        }
}
impl EnumArrays {
    fn __name_for_array_enum() -> String {
        String::from("CodegenProperty{openApiType='array', baseName='array_enum', complexType='string', getter='getArrayEnum', setter='setArrayEnum', description='null', dataType='Vec<String>', datatypeWithEnum='Vec<String>', dataFormat='null', name='array_enum', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.array_enum;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "string",
    "enum" : [ "fish", "crab" ]
  },
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues={enumVars=[{name=Fish, isString=true, value="fish"}, {name=Crab, isString=true, value="crab"}], values=[fish, crab]}, items=CodegenProperty{openApiType='string', baseName='array_enum', complexType='string', getter='getArrayEnum', setter='setArrayEnum', description='null', dataType='String', datatypeWithEnum='ArrayEnum', dataFormat='null', name='array_enum', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_enum;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "fish", "crab" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[fish, crab], allowableValues={values=[fish, crab], enumVars=[{name=Fish, isString=true, value="fish"}, {name=Crab, isString=true, value="crab"}]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayEnum', nameInPascalCase='ArrayEnum', nameInSnakeCase='ARRAY_ENUM', enumName='ArrayEnum', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='array_enum', complexType='string', getter='getArrayEnum', setter='setArrayEnum', description='null', dataType='String', datatypeWithEnum='ArrayEnum', dataFormat='null', name='array_enum', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_enum;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "fish", "crab" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[fish, crab], allowableValues={values=[fish, crab], enumVars=[{name=Fish, isString=true, value="fish"}, {name=Crab, isString=true, value="crab"}]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayEnum', nameInPascalCase='ArrayEnum', nameInSnakeCase='ARRAY_ENUM', enumName='ArrayEnum', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayEnum', nameInPascalCase='ArrayEnum', nameInSnakeCase='ARRAY_ENUM', enumName='ArrayEnum', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_array_enum<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_array_enum())
        }
}
impl EnumArrays {
    fn __name_for_array_array_enum() -> String {
        String::from("CodegenProperty{openApiType='array', baseName='array_array_enum', complexType='array', getter='getArrayArrayEnum', setter='setArrayArrayEnum', description='null', dataType='Vec<Vec<String>>', datatypeWithEnum='Vec<Vec<String>>', dataFormat='null', name='array_array_enum', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.array_array_enum;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "array",
    "items" : {
      "type" : "string",
      "enum" : [ "Cat", "Dog" ]
    }
  },
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues={enumVars=[{name=Cat, isString=true, value="Cat"}, {name=Dog, isString=true, value="Dog"}], values=[Cat, Dog]}, items=CodegenProperty{openApiType='array', baseName='array_array_enum', complexType='string', getter='getArrayArrayEnum', setter='setArrayArrayEnum', description='null', dataType='Vec<String>', datatypeWithEnum='Vec<String>', dataFormat='null', name='array_array_enum', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_array_enum;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "string",
    "enum" : [ "Cat", "Dog" ]
  },
  "type" : "array"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues={values=[Cat, Dog], enumVars=[{name=Cat, isString=true, value="Cat"}, {name=Dog, isString=true, value="Dog"}]}, items=CodegenProperty{openApiType='string', baseName='array_array_enum', complexType='string', getter='getArrayArrayEnum', setter='setArrayArrayEnum', description='null', dataType='String', datatypeWithEnum='ArrayArrayEnum', dataFormat='null', name='array_array_enum', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_array_enum;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "Cat", "Dog" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[Cat, Dog], allowableValues={values=[Cat, Dog], enumVars=[{name=Cat, isString=true, value="Cat"}, {name=Dog, isString=true, value="Dog"}]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayEnum', nameInPascalCase='ArrayArrayEnum', nameInSnakeCase='ARRAY_ARRAY_ENUM', enumName='ArrayArrayEnum', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='array_array_enum', complexType='string', getter='getArrayArrayEnum', setter='setArrayArrayEnum', description='null', dataType='String', datatypeWithEnum='ArrayArrayEnum', dataFormat='null', name='array_array_enum', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_array_enum;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "Cat", "Dog" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[Cat, Dog], allowableValues={values=[Cat, Dog], enumVars=[{name=Cat, isString=true, value="Cat"}, {name=Dog, isString=true, value="Dog"}]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayEnum', nameInPascalCase='ArrayArrayEnum', nameInSnakeCase='ARRAY_ARRAY_ENUM', enumName='ArrayArrayEnum', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayEnum', nameInPascalCase='ArrayArrayEnum', nameInSnakeCase='ARRAY_ARRAY_ENUM', enumName='ArrayArrayEnum', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='array_array_enum', complexType='string', getter='getArrayArrayEnum', setter='setArrayArrayEnum', description='null', dataType='String', datatypeWithEnum='ArrayArrayEnum', dataFormat='null', name='array_array_enum', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.array_array_enum;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "Cat", "Dog" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[Cat, Dog], allowableValues={values=[Cat, Dog], enumVars=[{name=Cat, isString=true, value="Cat"}, {name=Dog, isString=true, value="Dog"}]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayEnum', nameInPascalCase='ArrayArrayEnum', nameInSnakeCase='ARRAY_ARRAY_ENUM', enumName='ArrayArrayEnum', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='arrayArrayEnum', nameInPascalCase='ArrayArrayEnum', nameInSnakeCase='ARRAY_ARRAY_ENUM', enumName='ArrayArrayEnum', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_array_array_enum<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_array_array_enum())
        }
}


impl EnumArrays {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
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
impl std::fmt::Display for EnumArrays {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.just_symbol.as_ref().map(|just_symbol| {
                [
                    "just_symbol".to_string(),
                    just_symbol.to_string(),
                ].join(",")
            }),


            self.array_enum.as_ref().map(|array_enum| {
                [
                    "array_enum".to_string(),
                    array_enum.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(","),
                ].join(",")
            }),

            // Skipping array_array_enum in query parameter serialization

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a EnumArrays value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for EnumArrays {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub just_symbol: Vec<String>,
            pub array_enum: Vec<Vec<String>>,
            pub array_array_enum: Vec<Vec<Vec<String>>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing EnumArrays".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "just_symbol" => intermediate_rep.just_symbol.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    "array_enum" => return std::result::Result::Err("Parsing a container in this style is not supported in EnumArrays".to_string()),
                    "array_array_enum" => return std::result::Result::Err("Parsing a container in this style is not supported in EnumArrays".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing EnumArrays".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(EnumArrays {
            just_symbol: intermediate_rep.just_symbol.into_iter().next(),
            array_enum: intermediate_rep.array_enum.into_iter().next(),
            array_array_enum: intermediate_rep.array_array_enum.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<EnumArrays> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<EnumArrays>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<EnumArrays>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for EnumArrays - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<EnumArrays> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <EnumArrays as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into EnumArrays - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk_enum_derive::LabelledGenericEnum))]
pub enum EnumClass {
    #[serde(rename = "_abc")]
    Abc,
    #[serde(rename = "-efg")]
    Efg,
    #[serde(rename = "(xyz)")]
    LeftParenthesisXyzRightParenthesis,
}

impl std::fmt::Display for EnumClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            EnumClass::Abc => write!(f, "_abc"),
            EnumClass::Efg => write!(f, "-efg"),
            EnumClass::LeftParenthesisXyzRightParenthesis => write!(f, "(xyz)"),
        }
    }
}

impl std::str::FromStr for EnumClass {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "_abc" => std::result::Result::Ok(EnumClass::Abc),
            "-efg" => std::result::Result::Ok(EnumClass::Efg),
            "(xyz)" => std::result::Result::Ok(EnumClass::LeftParenthesisXyzRightParenthesis),
            _ => std::result::Result::Err(format!("Value not valid: {}", s)),
        }
    }
}


#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct EnumTest {
        /// Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "enum_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub enum_string: Option<String>,

        /// Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "enum_string_required")]
    pub enum_string_required: String,

        /// Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "enum_integer")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub enum_integer: Option<i32>,

        /// Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "enum_number")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub enum_number: Option<f64>,

    #[serde(rename = "outerEnum")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub outer_enum: Option<models::OuterEnum>,

}

impl EnumTest {
    fn __name_for_enum_string() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='enum_string', complexType='string', getter='getEnumString', setter='setEnumString', description='null', dataType='String', datatypeWithEnum='EnumString', dataFormat='null', name='enum_string', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.enum_string;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "UPPER", "lower", "" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[UPPER, lower, ], allowableValues={enumVars=[{name=Upper, isString=true, value="UPPER"}, {name=Lower, isString=true, value="lower"}, {name=Empty, isString=true, value=""}], values=[UPPER, lower, ]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='enumString', nameInPascalCase='EnumString', nameInSnakeCase='ENUM_STRING', enumName='EnumString', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_enum_string<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_enum_string())
        }
}
impl EnumTest {
    fn __name_for_enum_string_required() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='enum_string_required', complexType='string', getter='getEnumStringRequired', setter='setEnumStringRequired', description='null', dataType='String', datatypeWithEnum='EnumStringRequired', dataFormat='null', name='enum_string_required', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.enum_string_required;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "UPPER", "lower", "" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[UPPER, lower, ], allowableValues={enumVars=[{name=Upper, isString=true, value="UPPER"}, {name=Lower, isString=true, value="lower"}, {name=Empty, isString=true, value=""}], values=[UPPER, lower, ]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='enumStringRequired', nameInPascalCase='EnumStringRequired', nameInSnakeCase='ENUM_STRING_REQUIRED', enumName='EnumStringRequired', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_enum_string_required<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_enum_string_required())
        }
}
impl EnumTest {
    fn __name_for_enum_integer() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='enum_integer', complexType='integer', getter='getEnumInteger', setter='setEnumInteger', description='null', dataType='i32', datatypeWithEnum='EnumInteger', dataFormat='int32', name='enum_integer', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.enum_integer;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ 1, -1 ],
  "format" : "int32",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=true, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[1, -1], allowableValues={enumVars=[{name=Variant1, isString=false, value="1"}, {name=Variant12, isString=false, value="-1"}], values=[1, -1]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='enumInteger', nameInPascalCase='EnumInteger', nameInSnakeCase='ENUM_INTEGER', enumName='EnumInteger', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int32, dependentRequired=null, contains=null}")
    }

    fn __serialize_enum_integer<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_enum_integer())
        }
}
impl EnumTest {
    fn __name_for_enum_number() -> String {
        String::from("CodegenProperty{openApiType='number', baseName='enum_number', complexType='double', getter='getEnumNumber', setter='setEnumNumber', description='null', dataType='f64', datatypeWithEnum='EnumNumber', dataFormat='double', name='enum_number', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.enum_number;', baseType='double', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ 1.1, -1.2 ],
  "format" : "double",
  "type" : "number"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=true, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[1.1, -1.2], allowableValues={enumVars=[{name=Variant1Period1, isString=false, value="1.1"}, {name=Variant1Period2, isString=false, value="-1.2"}], values=[1.1, -1.2]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='enumNumber', nameInPascalCase='EnumNumber', nameInSnakeCase='ENUM_NUMBER', enumName='EnumNumber', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=double, dependentRequired=null, contains=null}")
    }

    fn __serialize_enum_number<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_enum_number())
        }
}
impl EnumTest {
    fn __name_for_outer_enum() -> String {
        String::from("CodegenProperty{openApiType='OuterEnum', baseName='outerEnum', complexType='OuterEnum', getter='getOuterEnum', setter='setOuterEnum', description='null', dataType='models::OuterEnum', datatypeWithEnum='models::OuterEnum', dataFormat='null', name='outer_enum', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.outerEnum;', baseType='OuterEnum', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "$ref" : "#/components/schemas/OuterEnum"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=true, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues={enumVars=[{name=Placed, isString=false, value="placed"}, {name=Approved, isString=false, value="approved"}, {name=Delivered, isString=false, value="delivered"}], values=[placed, approved, delivered]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='outerEnum', nameInPascalCase='OuterEnum', nameInSnakeCase='OUTER_ENUM', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=#/components/schemas/OuterEnum, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_outer_enum<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_outer_enum())
        }
}


impl EnumTest {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(enum_string_required: String, ) -> EnumTest {
        EnumTest {
            enum_string: None,
            enum_string_required,
            enum_integer: None,
            enum_number: None,
            outer_enum: None,
        }
    }
}

/// Converts the EnumTest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for EnumTest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.enum_string.as_ref().map(|enum_string| {
                [
                    "enum_string".to_string(),
                    enum_string.to_string(),
                ].join(",")
            }),


            Some("enum_string_required".to_string()),
            Some(self.enum_string_required.to_string()),


            self.enum_integer.as_ref().map(|enum_integer| {
                [
                    "enum_integer".to_string(),
                    enum_integer.to_string(),
                ].join(",")
            }),


            self.enum_number.as_ref().map(|enum_number| {
                [
                    "enum_number".to_string(),
                    enum_number.to_string(),
                ].join(",")
            }),

            // Skipping outerEnum in query parameter serialization

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a EnumTest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for EnumTest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub enum_string: Vec<String>,
            pub enum_string_required: Vec<String>,
            pub enum_integer: Vec<i32>,
            pub enum_number: Vec<f64>,
            pub outer_enum: Vec<models::OuterEnum>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing EnumTest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "enum_string" => intermediate_rep.enum_string.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "enum_string_required" => intermediate_rep.enum_string_required.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "enum_integer" => intermediate_rep.enum_integer.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "enum_number" => intermediate_rep.enum_number.push(<f64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "outerEnum" => intermediate_rep.outer_enum.push(<models::OuterEnum as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing EnumTest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(EnumTest {
            enum_string: intermediate_rep.enum_string.into_iter().next(),
            enum_string_required: intermediate_rep.enum_string_required.into_iter().next().ok_or_else(|| "enum_string_required missing in EnumTest".to_string())?,
            enum_integer: intermediate_rep.enum_integer.into_iter().next(),
            enum_number: intermediate_rep.enum_number.into_iter().next(),
            outer_enum: intermediate_rep.outer_enum.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<EnumTest> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<EnumTest>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<EnumTest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for EnumTest - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<EnumTest> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <EnumTest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into EnumTest - {}",
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
pub struct FormatTest {
    #[serde(rename = "integer")]
    #[validate(
            range(min = 10u8, max = 100u8),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub integer: Option<u8>,

    #[serde(rename = "int32")]
    #[validate(
            range(min = 20u8, max = 200u8),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub int32: Option<u8>,

    #[serde(rename = "int64")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub int64: Option<i64>,

    #[serde(rename = "number")]
    #[validate(
            range(min = 32.1f64, max = 543.2f64),
        )]
    pub number: f64,

    #[serde(rename = "float")]
    #[validate(
            range(min = 54.3f32, max = 987.6f32),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub float: Option<f32>,

    #[serde(rename = "double")]
    #[validate(
            range(min = 67.8f64, max = 123.4f64),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub double: Option<f64>,

    #[serde(rename = "string")]
    #[validate(
            regex(path = *RE_FORMATTEST_STRING),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub string: Option<String>,

    #[serde(rename = "byte")]
    #[validate(
            custom(function = "validate_byte_formattest_byte"),
        )]
    pub byte: ByteArray,

    #[serde(rename = "binary")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub binary: Option<ByteArray>,

    #[serde(rename = "date")]
    pub date: chrono::naive::NaiveDate,

    #[serde(rename = "dateTime")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub date_time: Option<chrono::DateTime::<chrono::Utc>>,

    #[serde(rename = "uuid")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub uuid: Option<uuid::Uuid>,

    #[serde(rename = "password")]
    #[validate(
            length(min = 10, max = 64),
        )]
    pub password: String,

}

impl FormatTest {
    fn __name_for_integer() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='integer', complexType='integer', getter='getInteger', setter='setInteger', description='null', dataType='u8', datatypeWithEnum='i32', dataFormat='null', name='integer', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.integer;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "maximum" : 100,
  "minimum" : 10,
  "type" : "integer"
}', minimum='10', maximum='100', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=false, isLong=false, isUnboundedInteger=true, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='integer', nameInPascalCase='Integer', nameInSnakeCase='INTEGER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_integer<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_integer())
        }
}
impl FormatTest {
    fn __name_for_int32() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='int32', complexType='integer', getter='getInt32', setter='setInt32', description='null', dataType='u8', datatypeWithEnum='i32', dataFormat='int32', name='int32', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.int32;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int32",
  "maximum" : 200,
  "minimum" : 20,
  "type" : "integer"
}', minimum='20', maximum='200', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=true, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='int32', nameInPascalCase='Int32', nameInSnakeCase='INT32', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int32, dependentRequired=null, contains=null}")
    }

    fn __serialize_int32<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_int32())
        }
}
impl FormatTest {
    fn __name_for_int64() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='int64', complexType='long', getter='getInt64', setter='setInt64', description='null', dataType='i64', datatypeWithEnum='i64', dataFormat='int64', name='int64', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.int64;', baseType='long', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int64",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=true, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='int64', nameInPascalCase='Int64', nameInSnakeCase='INT64', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int64, dependentRequired=null, contains=null}")
    }

    fn __serialize_int64<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_int64())
        }
}
impl FormatTest {
    fn __name_for_number() -> String {
        String::from("CodegenProperty{openApiType='number', baseName='number', complexType='number', getter='getNumber', setter='setNumber', description='null', dataType='f64', datatypeWithEnum='f64', dataFormat='null', name='number', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.number;', baseType='number', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "maximum" : 543.2,
  "minimum" : 32.1,
  "type" : "number"
}', minimum='32.1', maximum='543.2', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=true, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='number', nameInPascalCase='Number', nameInSnakeCase='NUMBER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_number<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_number())
        }
}
impl FormatTest {
    fn __name_for_float() -> String {
        String::from("CodegenProperty{openApiType='number', baseName='float', complexType='float', getter='getFloat', setter='setFloat', description='null', dataType='f32', datatypeWithEnum='f32', dataFormat='float', name='float', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.float;', baseType='float', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "float",
  "maximum" : 987.6,
  "minimum" : 54.3,
  "type" : "number"
}', minimum='54.3', maximum='987.6', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=true, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='float', nameInPascalCase='Float', nameInSnakeCase='FLOAT', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=float, dependentRequired=null, contains=null}")
    }

    fn __serialize_float<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_float())
        }
}
impl FormatTest {
    fn __name_for_double() -> String {
        String::from("CodegenProperty{openApiType='number', baseName='double', complexType='double', getter='getDouble', setter='setDouble', description='null', dataType='f64', datatypeWithEnum='f64', dataFormat='double', name='double', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.double;', baseType='double', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "double",
  "maximum" : 123.4,
  "minimum" : 67.8,
  "type" : "number"
}', minimum='67.8', maximum='123.4', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=true, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='double', nameInPascalCase='Double', nameInSnakeCase='DOUBLE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=double, dependentRequired=null, contains=null}")
    }

    fn __serialize_double<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_double())
        }
}
impl FormatTest {
    fn __name_for_string() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='string', complexType='string', getter='getString', setter='setString', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='string', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.string;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='/[a-z]/i', example='null', jsonSchema='{
  "pattern" : "/[a-z]/i",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='string', nameInPascalCase='String', nameInSnakeCase='STRING', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_string<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_string())
        }
}
impl FormatTest {
    fn __name_for_byte() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='byte', complexType='ByteArray', getter='getByte', setter='setByte', description='null', dataType='ByteArray', datatypeWithEnum='ByteArray', dataFormat='byte', name='byte', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.byte;', baseType='ByteArray', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$', example='null', jsonSchema='{
  "format" : "byte",
  "pattern" : "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?$",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=true, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='byte', nameInPascalCase='Byte', nameInSnakeCase='BYTE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=byte, dependentRequired=null, contains=null}")
    }

    fn __serialize_byte<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_byte())
        }
}
impl FormatTest {
    fn __name_for_binary() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='binary', complexType='file', getter='getBinary', setter='setBinary', description='null', dataType='ByteArray', datatypeWithEnum='ByteArray', dataFormat='binary', name='binary', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.binary;', baseType='file', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "binary",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=true, isFile=true, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='binary', nameInPascalCase='Binary', nameInSnakeCase='BINARY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=binary, dependentRequired=null, contains=null}")
    }

    fn __serialize_binary<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_binary())
        }
}
impl FormatTest {
    fn __name_for_date() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='date', complexType='date', getter='getDate', setter='setDate', description='null', dataType='chrono::naive::NaiveDate', datatypeWithEnum='chrono::naive::NaiveDate', dataFormat='date', name='date', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.date;', baseType='date', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "date",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=true, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='date', nameInPascalCase='Date', nameInSnakeCase='DATE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=date, dependentRequired=null, contains=null}")
    }

    fn __serialize_date<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_date())
        }
}
impl FormatTest {
    fn __name_for_date_time() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='dateTime', complexType='DateTime', getter='getDateTime', setter='setDateTime', description='null', dataType='chrono::DateTime::<chrono::Utc>', datatypeWithEnum='chrono::DateTime::<chrono::Utc>', dataFormat='date-time', name='date_time', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.dateTime;', baseType='DateTime', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "date-time",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=true, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='dateTime', nameInPascalCase='DateTime', nameInSnakeCase='DATE_TIME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=date-time, dependentRequired=null, contains=null}")
    }

    fn __serialize_date_time<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_date_time())
        }
}
impl FormatTest {
    fn __name_for_uuid() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='uuid', complexType='UUID', getter='getUuid', setter='setUuid', description='null', dataType='uuid::Uuid', datatypeWithEnum='uuid::Uuid', dataFormat='uuid', name='uuid', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.uuid;', baseType='UUID', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "uuid",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=true, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='uuid', nameInPascalCase='Uuid', nameInSnakeCase='UUID', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=uuid, dependentRequired=null, contains=null}")
    }

    fn __serialize_uuid<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_uuid())
        }
}
impl FormatTest {
    fn __name_for_password() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='password', complexType='password', getter='getPassword', setter='setPassword', description='null', dataType='String', datatypeWithEnum='String', dataFormat='password', name='password', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.password;', baseType='password', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=64, minLength=10, pattern='null', example='null', jsonSchema='{
  "format" : "password",
  "maxLength" : 64,
  "minLength" : 10,
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=true, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='password', nameInPascalCase='Password', nameInSnakeCase='PASSWORD', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=password, dependentRequired=null, contains=null}")
    }

    fn __serialize_password<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_password())
        }
}

lazy_static::lazy_static! {
    static ref RE_FORMATTEST_STRING: regex::Regex = regex::Regex::new(r"/[a-z]/i").unwrap();
}
lazy_static::lazy_static! {
    static ref RE_FORMATTEST_BYTE: regex::bytes::Regex = regex::bytes::Regex::new(r"^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}&#x3D;&#x3D;|[A-Za-z0-9+/]{3}&#x3D;)?$").unwrap();
}
fn validate_byte_formattest_byte(
    b: &ByteArray
) -> std::result::Result<(), validator::ValidationError> {
    if !RE_FORMATTEST_BYTE.is_match(&b.0) {
        return Err(validator::ValidationError::new("Character not allowed"));
    }
    std::result::Result::Ok(())
}

impl FormatTest {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(number: f64, byte: ByteArray, date: chrono::naive::NaiveDate, password: String, ) -> FormatTest {
        FormatTest {
            integer: None,
            int32: None,
            int64: None,
            number,
            float: None,
            double: None,
            string: None,
            byte,
            binary: None,
            date,
            date_time: None,
            uuid: None,
            password,
        }
    }
}

/// Converts the FormatTest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for FormatTest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.integer.as_ref().map(|integer| {
                [
                    "integer".to_string(),
                    integer.to_string(),
                ].join(",")
            }),


            self.int32.as_ref().map(|int32| {
                [
                    "int32".to_string(),
                    int32.to_string(),
                ].join(",")
            }),


            self.int64.as_ref().map(|int64| {
                [
                    "int64".to_string(),
                    int64.to_string(),
                ].join(",")
            }),


            Some("number".to_string()),
            Some(self.number.to_string()),


            self.float.as_ref().map(|float| {
                [
                    "float".to_string(),
                    float.to_string(),
                ].join(",")
            }),


            self.double.as_ref().map(|double| {
                [
                    "double".to_string(),
                    double.to_string(),
                ].join(",")
            }),


            self.string.as_ref().map(|string| {
                [
                    "string".to_string(),
                    string.to_string(),
                ].join(",")
            }),

            // Skipping byte in query parameter serialization
            // Skipping byte in query parameter serialization

            // Skipping binary in query parameter serialization
            // Skipping binary in query parameter serialization

            // Skipping date in query parameter serialization

            // Skipping dateTime in query parameter serialization

            // Skipping uuid in query parameter serialization


            Some("password".to_string()),
            Some(self.password.to_string()),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a FormatTest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for FormatTest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub integer: Vec<u8>,
            pub int32: Vec<u8>,
            pub int64: Vec<i64>,
            pub number: Vec<f64>,
            pub float: Vec<f32>,
            pub double: Vec<f64>,
            pub string: Vec<String>,
            pub byte: Vec<ByteArray>,
            pub binary: Vec<ByteArray>,
            pub date: Vec<chrono::naive::NaiveDate>,
            pub date_time: Vec<chrono::DateTime::<chrono::Utc>>,
            pub uuid: Vec<uuid::Uuid>,
            pub password: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing FormatTest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "integer" => intermediate_rep.integer.push(<u8 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "int32" => intermediate_rep.int32.push(<u8 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "int64" => intermediate_rep.int64.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "number" => intermediate_rep.number.push(<f64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "float" => intermediate_rep.float.push(<f32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "double" => intermediate_rep.double.push(<f64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "string" => intermediate_rep.string.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    "byte" => return std::result::Result::Err("Parsing binary data in this style is not supported in FormatTest".to_string()),
                    "binary" => return std::result::Result::Err("Parsing binary data in this style is not supported in FormatTest".to_string()),
                    #[allow(clippy::redundant_clone)]
                    "date" => intermediate_rep.date.push(<chrono::naive::NaiveDate as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "dateTime" => intermediate_rep.date_time.push(<chrono::DateTime::<chrono::Utc> as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "uuid" => intermediate_rep.uuid.push(<uuid::Uuid as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "password" => intermediate_rep.password.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing FormatTest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(FormatTest {
            integer: intermediate_rep.integer.into_iter().next(),
            int32: intermediate_rep.int32.into_iter().next(),
            int64: intermediate_rep.int64.into_iter().next(),
            number: intermediate_rep.number.into_iter().next().ok_or_else(|| "number missing in FormatTest".to_string())?,
            float: intermediate_rep.float.into_iter().next(),
            double: intermediate_rep.double.into_iter().next(),
            string: intermediate_rep.string.into_iter().next(),
            byte: intermediate_rep.byte.into_iter().next().ok_or_else(|| "byte missing in FormatTest".to_string())?,
            binary: intermediate_rep.binary.into_iter().next(),
            date: intermediate_rep.date.into_iter().next().ok_or_else(|| "date missing in FormatTest".to_string())?,
            date_time: intermediate_rep.date_time.into_iter().next(),
            uuid: intermediate_rep.uuid.into_iter().next(),
            password: intermediate_rep.password.into_iter().next().ok_or_else(|| "password missing in FormatTest".to_string())?,
        })
    }
}

// Methods for converting between header::IntoHeaderValue<FormatTest> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<FormatTest>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<FormatTest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for FormatTest - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<FormatTest> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <FormatTest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into FormatTest - {}",
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
pub struct HasOnlyReadOnly {
    #[serde(rename = "bar")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub bar: Option<String>,

    #[serde(rename = "foo")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub foo: Option<String>,

}

impl HasOnlyReadOnly {
    fn __name_for_bar() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='bar', complexType='string', getter='getBar', setter='setBar', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='bar', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.bar;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "readOnly" : true,
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=true, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='bar', nameInPascalCase='Bar', nameInSnakeCase='BAR', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_bar<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_bar())
        }
}
impl HasOnlyReadOnly {
    fn __name_for_foo() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='foo', complexType='string', getter='getFoo', setter='setFoo', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='foo', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.foo;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "readOnly" : true,
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=true, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='foo', nameInPascalCase='Foo', nameInSnakeCase='FOO', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_foo<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_foo())
        }
}


impl HasOnlyReadOnly {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
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
impl std::fmt::Display for HasOnlyReadOnly {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.bar.as_ref().map(|bar| {
                [
                    "bar".to_string(),
                    bar.to_string(),
                ].join(",")
            }),


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

/// Converts Query Parameters representation (style=form, explode=false) to a HasOnlyReadOnly value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for HasOnlyReadOnly {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub bar: Vec<String>,
            pub foo: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing HasOnlyReadOnly".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "bar" => intermediate_rep.bar.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "foo" => intermediate_rep.foo.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing HasOnlyReadOnly".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(HasOnlyReadOnly {
            bar: intermediate_rep.bar.into_iter().next(),
            foo: intermediate_rep.foo.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<HasOnlyReadOnly> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<HasOnlyReadOnly>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<HasOnlyReadOnly>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for HasOnlyReadOnly - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<HasOnlyReadOnly> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <HasOnlyReadOnly as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into HasOnlyReadOnly - {}",
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
pub struct List {
    #[serde(rename = "123-list")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub param_123_list: Option<String>,

}

impl List {
    fn __name_for_param_123_list() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='123-list', complexType='string', getter='getParam123List', setter='setParam123List', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='param_123_list', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.123-list;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='param123List', nameInPascalCase='Param123List', nameInSnakeCase='PARAM123_LIST', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_param_123_list<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_param_123_list())
        }
}


impl List {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> List {
        List {
            param_123_list: None,
        }
    }
}

/// Converts the List value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.param_123_list.as_ref().map(|param_123_list| {
                [
                    "123-list".to_string(),
                    param_123_list.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a List value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for List {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub param_123_list: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing List".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "123-list" => intermediate_rep.param_123_list.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing List".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(List {
            param_123_list: intermediate_rep.param_123_list.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<List> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<List>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<List>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for List - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<List> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <List as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into List - {}",
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
pub struct MapTest {
    #[serde(rename = "map_map_of_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map_map_of_string: Option<std::collections::HashMap<String, std::collections::HashMap<String, String>>>,

        /// Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "map_map_of_enum")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map_map_of_enum: Option<std::collections::HashMap<String, std::collections::HashMap<String, String>>>,

        /// Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "map_of_enum_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map_of_enum_string: Option<std::collections::HashMap<String, String>>,

}

impl MapTest {
    fn __name_for_map_map_of_string() -> String {
        String::from("CodegenProperty{openApiType='object', baseName='map_map_of_string', complexType='map', getter='getMapMapOfString', setter='setMapMapOfString', description='null', dataType='std::collections::HashMap<String, std::collections::HashMap<String, String>>', datatypeWithEnum='std::collections::HashMap<String, std::collections::HashMap<String, String>>', dataFormat='null', name='map_map_of_string', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.map_map_of_string;', baseType='map', containerType='map', containerTypeMapped='std::collections::HashMap', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "additionalProperties" : {
    "type" : "object",
    "additionalProperties" : {
      "type" : "string"
    }
  },
  "type" : "object"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=true, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='object', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='std::collections::HashMap<String, String>', datatypeWithEnum='std::collections::HashMap<String, String>', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='map', containerType='map', containerTypeMapped='std::collections::HashMap', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "additionalProperties" : {
    "type" : "string"
  },
  "type" : "object"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=true, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=CodegenProperty{openApiType='string', baseName='additional_properties', complexType='string', getter='getAdditionalProperties', setter='setAdditionalProperties', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='additional_properties', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.additional_properties;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='additionalProperties', nameInPascalCase='AdditionalProperties', nameInSnakeCase='ADDITIONAL_PROPERTIES', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=true, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=CodegenProperty{openApiType='object', baseName='additional_properties', complexType='string', getter='getAdditionalProperties', setter='setAdditionalProperties', description='null', dataType='std::collections::HashMap<String, String>', datatypeWithEnum='std::collections::HashMap<String, String>', dataFormat='null', name='additional_properties', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.additional_properties;', baseType='map', containerType='map', containerTypeMapped='std::collections::HashMap', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "additionalProperties" : {
    "type" : "string"
  },
  "type" : "object"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=true, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=CodegenProperty{openApiType='string', baseName='additional_properties', complexType='string', getter='getAdditionalProperties', setter='setAdditionalProperties', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='additional_properties', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.additional_properties;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='additionalProperties', nameInPascalCase='AdditionalProperties', nameInSnakeCase='ADDITIONAL_PROPERTIES', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='additionalProperties', nameInPascalCase='AdditionalProperties', nameInSnakeCase='ADDITIONAL_PROPERTIES', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=true, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='mapMapOfString', nameInPascalCase='MapMapOfString', nameInSnakeCase='MAP_MAP_OF_STRING', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=true, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_map_map_of_string<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_map_map_of_string())
        }
}
impl MapTest {
    fn __name_for_map_map_of_enum() -> String {
        String::from("CodegenProperty{openApiType='object', baseName='map_map_of_enum', complexType='map', getter='getMapMapOfEnum', setter='setMapMapOfEnum', description='null', dataType='std::collections::HashMap<String, std::collections::HashMap<String, String>>', datatypeWithEnum='std::collections::HashMap<String, std::collections::HashMap<String, String>>', dataFormat='null', name='map_map_of_enum', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.map_map_of_enum;', baseType='map', containerType='map', containerTypeMapped='std::collections::HashMap', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "additionalProperties" : {
    "type" : "object",
    "additionalProperties" : {
      "type" : "string",
      "enum" : [ "UPPER", "lower" ]
    }
  },
  "type" : "object"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=true, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues={enumVars=[{name=Upper, isString=true, value="UPPER"}, {name=Lower, isString=true, value="lower"}], values=[UPPER, lower]}, items=CodegenProperty{openApiType='object', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='std::collections::HashMap<String, String>', datatypeWithEnum='std::collections::HashMap<String, String>', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='map', containerType='map', containerTypeMapped='std::collections::HashMap', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "additionalProperties" : {
    "type" : "string",
    "enum" : [ "UPPER", "lower" ]
  },
  "type" : "object"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=true, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues={values=[UPPER, lower], enumVars=[{name=Upper, isString=true, value="UPPER"}, {name=Lower, isString=true, value="lower"}]}, items=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='Inner', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "UPPER", "lower" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[UPPER, lower], allowableValues={values=[UPPER, lower], enumVars=[{name=Upper, isString=true, value="UPPER"}, {name=Lower, isString=true, value="lower"}]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='Inner', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=CodegenProperty{openApiType='string', baseName='additional_properties', complexType='string', getter='getAdditionalProperties', setter='setAdditionalProperties', description='null', dataType='String', datatypeWithEnum='AdditionalProperties', dataFormat='null', name='additional_properties', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.additional_properties;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "UPPER", "lower" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[UPPER, lower], allowableValues={values=[UPPER, lower]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='additionalProperties', nameInPascalCase='AdditionalProperties', nameInSnakeCase='ADDITIONAL_PROPERTIES', enumName='AdditionalProperties', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='Inner', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "UPPER", "lower" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[UPPER, lower], allowableValues={values=[UPPER, lower], enumVars=[{name=Upper, isString=true, value="UPPER"}, {name=Lower, isString=true, value="lower"}]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='Inner', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='Inner', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=true, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=CodegenProperty{openApiType='object', baseName='additional_properties', complexType='string', getter='getAdditionalProperties', setter='setAdditionalProperties', description='null', dataType='std::collections::HashMap<String, String>', datatypeWithEnum='std::collections::HashMap<String, String>', dataFormat='null', name='additional_properties', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.additional_properties;', baseType='map', containerType='map', containerTypeMapped='std::collections::HashMap', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "additionalProperties" : {
    "type" : "string",
    "enum" : [ "UPPER", "lower" ]
  },
  "type" : "object"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=true, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues={values=[UPPER, lower], enumVars=[{name=Upper, isString=true, value="UPPER"}, {name=Lower, isString=true, value="lower"}]}, items=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='Inner', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "UPPER", "lower" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[UPPER, lower], allowableValues={values=[UPPER, lower], enumVars=[{name=Upper, isString=true, value="UPPER"}, {name=Lower, isString=true, value="lower"}]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='Inner', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=CodegenProperty{openApiType='string', baseName='additional_properties', complexType='string', getter='getAdditionalProperties', setter='setAdditionalProperties', description='null', dataType='String', datatypeWithEnum='AdditionalProperties', dataFormat='null', name='additional_properties', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.additional_properties;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "UPPER", "lower" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[UPPER, lower], allowableValues={values=[UPPER, lower]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='additionalProperties', nameInPascalCase='AdditionalProperties', nameInSnakeCase='ADDITIONAL_PROPERTIES', enumName='AdditionalProperties', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='Inner', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "UPPER", "lower" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[UPPER, lower], allowableValues={values=[UPPER, lower], enumVars=[{name=Upper, isString=true, value="UPPER"}, {name=Lower, isString=true, value="lower"}]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='Inner', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='additionalProperties', nameInPascalCase='AdditionalProperties', nameInSnakeCase='ADDITIONAL_PROPERTIES', enumName='AdditionalProperties', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=true, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='Inner', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "UPPER", "lower" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[UPPER, lower], allowableValues={values=[UPPER, lower], enumVars=[{name=Upper, isString=true, value="UPPER"}, {name=Lower, isString=true, value="lower"}]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='Inner', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='mapMapOfEnum', nameInPascalCase='MapMapOfEnum', nameInSnakeCase='MAP_MAP_OF_ENUM', enumName='MapMapOfEnum', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=true, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_map_map_of_enum<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_map_map_of_enum())
        }
}
impl MapTest {
    fn __name_for_map_of_enum_string() -> String {
        String::from("CodegenProperty{openApiType='object', baseName='map_of_enum_string', complexType='string', getter='getMapOfEnumString', setter='setMapOfEnumString', description='null', dataType='std::collections::HashMap<String, String>', datatypeWithEnum='std::collections::HashMap<String, String>', dataFormat='null', name='map_of_enum_string', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.map_of_enum_string;', baseType='map', containerType='map', containerTypeMapped='std::collections::HashMap', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "additionalProperties" : {
    "type" : "string",
    "enum" : [ "UPPER", "lower" ]
  },
  "type" : "object"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=true, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues={enumVars=[{name=Upper, isString=true, value="UPPER"}, {name=Lower, isString=true, value="lower"}], values=[UPPER, lower]}, items=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='Inner', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "UPPER", "lower" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[UPPER, lower], allowableValues={values=[UPPER, lower], enumVars=[{name=Upper, isString=true, value="UPPER"}, {name=Lower, isString=true, value="lower"}]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='Inner', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=CodegenProperty{openApiType='string', baseName='additional_properties', complexType='string', getter='getAdditionalProperties', setter='setAdditionalProperties', description='null', dataType='String', datatypeWithEnum='AdditionalProperties', dataFormat='null', name='additional_properties', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.additional_properties;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "UPPER", "lower" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[UPPER, lower], allowableValues={values=[UPPER, lower]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='additionalProperties', nameInPascalCase='AdditionalProperties', nameInSnakeCase='ADDITIONAL_PROPERTIES', enumName='AdditionalProperties', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='inner', complexType='string', getter='getInner', setter='setInner', description='null', dataType='String', datatypeWithEnum='Inner', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "enum" : [ "UPPER", "lower" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[UPPER, lower], allowableValues={values=[UPPER, lower], enumVars=[{name=Upper, isString=true, value="UPPER"}, {name=Lower, isString=true, value="lower"}]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='Inner', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='mapOfEnumString', nameInPascalCase='MapOfEnumString', nameInSnakeCase='MAP_OF_ENUM_STRING', enumName='MapOfEnumString', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=true, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_map_of_enum_string<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_map_of_enum_string())
        }
}


impl MapTest {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
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
impl std::fmt::Display for MapTest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping map_map_of_string in query parameter serialization
            // Skipping map_map_of_string in query parameter serialization

            // Skipping map_map_of_enum in query parameter serialization
            // Skipping map_map_of_enum in query parameter serialization

            // Skipping map_of_enum_string in query parameter serialization

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MapTest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for MapTest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub map_map_of_string: Vec<std::collections::HashMap<String, std::collections::HashMap<String, String>>>,
            pub map_map_of_enum: Vec<std::collections::HashMap<String, std::collections::HashMap<String, String>>>,
            pub map_of_enum_string: Vec<std::collections::HashMap<String, String>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing MapTest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    "map_map_of_string" => return std::result::Result::Err("Parsing a container in this style is not supported in MapTest".to_string()),
                    "map_map_of_enum" => return std::result::Result::Err("Parsing a container in this style is not supported in MapTest".to_string()),
                    "map_of_enum_string" => return std::result::Result::Err("Parsing a container in this style is not supported in MapTest".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing MapTest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(MapTest {
            map_map_of_string: intermediate_rep.map_map_of_string.into_iter().next(),
            map_map_of_enum: intermediate_rep.map_map_of_enum.into_iter().next(),
            map_of_enum_string: intermediate_rep.map_of_enum_string.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<MapTest> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<MapTest>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<MapTest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for MapTest - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<MapTest> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <MapTest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into MapTest - {}",
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
pub struct MixedPropertiesAndAdditionalPropertiesClass {
    #[serde(rename = "uuid")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub uuid: Option<uuid::Uuid>,

    #[serde(rename = "dateTime")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub date_time: Option<chrono::DateTime::<chrono::Utc>>,

    #[serde(rename = "map")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub map: Option<std::collections::HashMap<String, models::Animal>>,

}

impl MixedPropertiesAndAdditionalPropertiesClass {
    fn __name_for_uuid() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='uuid', complexType='UUID', getter='getUuid', setter='setUuid', description='null', dataType='uuid::Uuid', datatypeWithEnum='uuid::Uuid', dataFormat='uuid', name='uuid', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.uuid;', baseType='UUID', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "uuid",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=true, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='uuid', nameInPascalCase='Uuid', nameInSnakeCase='UUID', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=uuid, dependentRequired=null, contains=null}")
    }

    fn __serialize_uuid<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_uuid())
        }
}
impl MixedPropertiesAndAdditionalPropertiesClass {
    fn __name_for_date_time() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='dateTime', complexType='DateTime', getter='getDateTime', setter='setDateTime', description='null', dataType='chrono::DateTime::<chrono::Utc>', datatypeWithEnum='chrono::DateTime::<chrono::Utc>', dataFormat='date-time', name='date_time', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.dateTime;', baseType='DateTime', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "date-time",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=true, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='dateTime', nameInPascalCase='DateTime', nameInSnakeCase='DATE_TIME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=date-time, dependentRequired=null, contains=null}")
    }

    fn __serialize_date_time<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_date_time())
        }
}
impl MixedPropertiesAndAdditionalPropertiesClass {
    fn __name_for_map() -> String {
        String::from("CodegenProperty{openApiType='object', baseName='map', complexType='Animal', getter='getMap', setter='setMap', description='null', dataType='std::collections::HashMap<String, models::Animal>', datatypeWithEnum='std::collections::HashMap<String, models::Animal>', dataFormat='null', name='map', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.map;', baseType='map', containerType='map', containerTypeMapped='std::collections::HashMap', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "additionalProperties" : {
    "$ref" : "#/components/schemas/Animal"
  },
  "type" : "object"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=true, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='Animal', baseName='inner', complexType='Animal', getter='getInner', setter='setInner', description='null', dataType='models::Animal', datatypeWithEnum='models::Animal', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='Animal', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "$ref" : "#/components/schemas/Animal"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=true, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=#/components/schemas/Animal, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=CodegenProperty{openApiType='Animal', baseName='additional_properties', complexType='Animal', getter='getAdditionalProperties', setter='setAdditionalProperties', description='null', dataType='models::Animal', datatypeWithEnum='models::Animal', dataFormat='null', name='additional_properties', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.additional_properties;', baseType='Animal', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "$ref" : "#/components/schemas/Animal"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=true, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='additionalProperties', nameInPascalCase='AdditionalProperties', nameInSnakeCase='ADDITIONAL_PROPERTIES', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=#/components/schemas/Animal, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='Animal', baseName='inner', complexType='Animal', getter='getInner', setter='setInner', description='null', dataType='models::Animal', datatypeWithEnum='models::Animal', dataFormat='null', name='inner', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.inner;', baseType='Animal', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "$ref" : "#/components/schemas/Animal"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=true, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=#/components/schemas/Animal, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='map', nameInPascalCase='Map', nameInSnakeCase='MAP', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=true, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_map<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_map())
        }
}


impl MixedPropertiesAndAdditionalPropertiesClass {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
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
impl std::fmt::Display for MixedPropertiesAndAdditionalPropertiesClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping uuid in query parameter serialization

            // Skipping dateTime in query parameter serialization

            // Skipping map in query parameter serialization
            // Skipping map in query parameter serialization

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a MixedPropertiesAndAdditionalPropertiesClass value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for MixedPropertiesAndAdditionalPropertiesClass {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub uuid: Vec<uuid::Uuid>,
            pub date_time: Vec<chrono::DateTime::<chrono::Utc>>,
            pub map: Vec<std::collections::HashMap<String, models::Animal>>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing MixedPropertiesAndAdditionalPropertiesClass".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "uuid" => intermediate_rep.uuid.push(<uuid::Uuid as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "dateTime" => intermediate_rep.date_time.push(<chrono::DateTime::<chrono::Utc> as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    "map" => return std::result::Result::Err("Parsing a container in this style is not supported in MixedPropertiesAndAdditionalPropertiesClass".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing MixedPropertiesAndAdditionalPropertiesClass".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(MixedPropertiesAndAdditionalPropertiesClass {
            uuid: intermediate_rep.uuid.into_iter().next(),
            date_time: intermediate_rep.date_time.into_iter().next(),
            map: intermediate_rep.map.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<MixedPropertiesAndAdditionalPropertiesClass> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<MixedPropertiesAndAdditionalPropertiesClass>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<MixedPropertiesAndAdditionalPropertiesClass>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for MixedPropertiesAndAdditionalPropertiesClass - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<MixedPropertiesAndAdditionalPropertiesClass> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <MixedPropertiesAndAdditionalPropertiesClass as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into MixedPropertiesAndAdditionalPropertiesClass - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




/// Model for testing model name starting with number
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Model200Response {
    #[serde(rename = "name")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub name: Option<i32>,

    #[serde(rename = "class")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub class: Option<String>,

}

impl Model200Response {
    fn __name_for_name() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='name', complexType='integer', getter='getName', setter='setName', description='null', dataType='i32', datatypeWithEnum='i32', dataFormat='int32', name='name', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.name;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int32",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=true, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='name', nameInPascalCase='Name', nameInSnakeCase='NAME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int32, dependentRequired=null, contains=null}")
    }

    fn __serialize_name<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_name())
        }
}
impl Model200Response {
    fn __name_for_class() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='class', complexType='string', getter='getClass', setter='setClass', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='class', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.class;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='class', nameInPascalCase='Class', nameInSnakeCase='CLASS', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_class<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_class())
        }
}


impl Model200Response {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
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
impl std::fmt::Display for Model200Response {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.name.as_ref().map(|name| {
                [
                    "name".to_string(),
                    name.to_string(),
                ].join(",")
            }),


            self.class.as_ref().map(|class| {
                [
                    "class".to_string(),
                    class.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Model200Response value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Model200Response {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub name: Vec<i32>,
            pub class: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Model200Response".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "name" => intermediate_rep.name.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "class" => intermediate_rep.class.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Model200Response".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Model200Response {
            name: intermediate_rep.name.into_iter().next(),
            class: intermediate_rep.class.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Model200Response> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Model200Response>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Model200Response>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Model200Response - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Model200Response> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Model200Response as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into Model200Response - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




/// Model for testing model name same as property name
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
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
    pub param_123_number: Option<i32>,

}

impl Name {
    fn __name_for_name() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='name', complexType='integer', getter='getName', setter='setName', description='null', dataType='i32', datatypeWithEnum='i32', dataFormat='int32', name='name', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.name;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int32",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=true, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='name', nameInPascalCase='Name', nameInSnakeCase='NAME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int32, dependentRequired=null, contains=null}")
    }

    fn __serialize_name<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_name())
        }
}
impl Name {
    fn __name_for_snake_case() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='snake_case', complexType='integer', getter='getSnakeCase', setter='setSnakeCase', description='null', dataType='i32', datatypeWithEnum='i32', dataFormat='int32', name='snake_case', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.snake_case;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int32",
  "readOnly" : true,
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=true, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=true, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='snakeCase', nameInPascalCase='SnakeCase', nameInSnakeCase='SNAKE_CASE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int32, dependentRequired=null, contains=null}")
    }

    fn __serialize_snake_case<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_snake_case())
        }
}
impl Name {
    fn __name_for_property() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='property', complexType='string', getter='getProperty', setter='setProperty', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='property', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.property;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='property', nameInPascalCase='Property', nameInSnakeCase='PROPERTY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_property<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_property())
        }
}
impl Name {
    fn __name_for_param_123_number() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='123Number', complexType='integer', getter='getParam123Number', setter='setParam123Number', description='null', dataType='i32', datatypeWithEnum='i32', dataFormat='null', name='param_123_number', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.123Number;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "readOnly" : true,
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=false, isLong=false, isUnboundedInteger=true, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=true, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='param123Number', nameInPascalCase='Param123Number', nameInSnakeCase='PARAM123_NUMBER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_param_123_number<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_param_123_number())
        }
}


impl Name {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(name: i32, ) -> Name {
        Name {
            name,
            snake_case: None,
            property: None,
            param_123_number: None,
        }
    }
}

/// Converts the Name value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            Some("name".to_string()),
            Some(self.name.to_string()),


            self.snake_case.as_ref().map(|snake_case| {
                [
                    "snake_case".to_string(),
                    snake_case.to_string(),
                ].join(",")
            }),


            self.property.as_ref().map(|property| {
                [
                    "property".to_string(),
                    property.to_string(),
                ].join(",")
            }),


            self.param_123_number.as_ref().map(|param_123_number| {
                [
                    "123Number".to_string(),
                    param_123_number.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Name value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Name {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub name: Vec<i32>,
            pub snake_case: Vec<i32>,
            pub property: Vec<String>,
            pub param_123_number: Vec<i32>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Name".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "name" => intermediate_rep.name.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "snake_case" => intermediate_rep.snake_case.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "property" => intermediate_rep.property.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "123Number" => intermediate_rep.param_123_number.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Name".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Name {
            name: intermediate_rep.name.into_iter().next().ok_or_else(|| "name missing in Name".to_string())?,
            snake_case: intermediate_rep.snake_case.into_iter().next(),
            property: intermediate_rep.property.into_iter().next(),
            param_123_number: intermediate_rep.param_123_number.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Name> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Name>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Name>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Name - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Name> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Name as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into Name - {}",
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
pub struct NumberOnly {
    #[serde(rename = "JustNumber")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub just_number: Option<f64>,

}

impl NumberOnly {
    fn __name_for_just_number() -> String {
        String::from("CodegenProperty{openApiType='number', baseName='JustNumber', complexType='number', getter='getJustNumber', setter='setJustNumber', description='null', dataType='f64', datatypeWithEnum='f64', dataFormat='null', name='just_number', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.JustNumber;', baseType='number', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "number"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=true, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='justNumber', nameInPascalCase='JustNumber', nameInSnakeCase='JUST_NUMBER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_just_number<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_just_number())
        }
}


impl NumberOnly {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> NumberOnly {
        NumberOnly {
            just_number: None,
        }
    }
}

/// Converts the NumberOnly value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for NumberOnly {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.just_number.as_ref().map(|just_number| {
                [
                    "JustNumber".to_string(),
                    just_number.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a NumberOnly value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for NumberOnly {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub just_number: Vec<f64>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing NumberOnly".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "JustNumber" => intermediate_rep.just_number.push(<f64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing NumberOnly".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(NumberOnly {
            just_number: intermediate_rep.just_number.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<NumberOnly> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<NumberOnly>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<NumberOnly>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for NumberOnly - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<NumberOnly> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <NumberOnly as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into NumberOnly - {}",
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
pub struct ObjectContainingObjectWithOnlyAdditionalProperties {
    #[serde(rename = "inner")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub inner: Option<models::ObjectWithOnlyAdditionalProperties>,

}

impl ObjectContainingObjectWithOnlyAdditionalProperties {
    fn __name_for_inner() -> String {
        String::from("CodegenProperty{openApiType='ObjectWithOnlyAdditionalProperties', baseName='inner', complexType='ObjectWithOnlyAdditionalProperties', getter='getInner', setter='setInner', description='null', dataType='models::ObjectWithOnlyAdditionalProperties', datatypeWithEnum='models::ObjectWithOnlyAdditionalProperties', dataFormat='null', name='inner', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.inner;', baseType='ObjectWithOnlyAdditionalProperties', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "$ref" : "#/components/schemas/ObjectWithOnlyAdditionalProperties"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='inner', nameInPascalCase='Inner', nameInSnakeCase='INNER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=#/components/schemas/ObjectWithOnlyAdditionalProperties, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_inner<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_inner())
        }
}


impl ObjectContainingObjectWithOnlyAdditionalProperties {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> ObjectContainingObjectWithOnlyAdditionalProperties {
        ObjectContainingObjectWithOnlyAdditionalProperties {
            inner: None,
        }
    }
}

/// Converts the ObjectContainingObjectWithOnlyAdditionalProperties value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for ObjectContainingObjectWithOnlyAdditionalProperties {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![
            // Skipping inner in query parameter serialization

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectContainingObjectWithOnlyAdditionalProperties value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ObjectContainingObjectWithOnlyAdditionalProperties {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub inner: Vec<models::ObjectWithOnlyAdditionalProperties>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ObjectContainingObjectWithOnlyAdditionalProperties".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "inner" => intermediate_rep.inner.push(<models::ObjectWithOnlyAdditionalProperties as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ObjectContainingObjectWithOnlyAdditionalProperties".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ObjectContainingObjectWithOnlyAdditionalProperties {
            inner: intermediate_rep.inner.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ObjectContainingObjectWithOnlyAdditionalProperties> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<ObjectContainingObjectWithOnlyAdditionalProperties>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ObjectContainingObjectWithOnlyAdditionalProperties>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ObjectContainingObjectWithOnlyAdditionalProperties - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<ObjectContainingObjectWithOnlyAdditionalProperties> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ObjectContainingObjectWithOnlyAdditionalProperties as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into ObjectContainingObjectWithOnlyAdditionalProperties - {}",
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
pub struct ObjectWithOnlyAdditionalProperties(std::collections::HashMap<String, String>);

impl validator::Validate for ObjectWithOnlyAdditionalProperties {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<std::collections::HashMap<String, String>> for ObjectWithOnlyAdditionalProperties {
    fn from(x: std::collections::HashMap<String, String>) -> Self {
        ObjectWithOnlyAdditionalProperties(x)
    }
}

impl std::convert::From<ObjectWithOnlyAdditionalProperties> for std::collections::HashMap<String, String> {
    fn from(x: ObjectWithOnlyAdditionalProperties) -> Self {
        x.0
    }
}

impl std::ops::Deref for ObjectWithOnlyAdditionalProperties {
    type Target = std::collections::HashMap<String, String>;
    fn deref(&self) -> &std::collections::HashMap<String, String> {
        &self.0
    }
}

impl std::ops::DerefMut for ObjectWithOnlyAdditionalProperties {
    fn deref_mut(&mut self) -> &mut std::collections::HashMap<String, String> {
        &mut self.0
    }
}

/// Converts the ObjectWithOnlyAdditionalProperties value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for ObjectWithOnlyAdditionalProperties {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Skipping additionalProperties in query parameter serialization
        write!(f, "")
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ObjectWithOnlyAdditionalProperties value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl ::std::str::FromStr for ObjectWithOnlyAdditionalProperties {
    type Err = &'static str;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Err("Parsing additionalProperties for ObjectWithOnlyAdditionalProperties is not supported")
    }
}


#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
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
        /// Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "status")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub status: Option<String>,

    #[serde(rename = "complete")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub complete: Option<bool>,

}

impl Order {
    fn __name_for_id() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='id', complexType='long', getter='getId', setter='setId', description='null', dataType='i64', datatypeWithEnum='i64', dataFormat='int64', name='id', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.id;', baseType='long', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int64",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=true, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='id', nameInPascalCase='Id', nameInSnakeCase='ID', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int64, dependentRequired=null, contains=null}")
    }

    fn __serialize_id<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_id())
        }
}
impl Order {
    fn __name_for_pet_id() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='petId', complexType='long', getter='getPetId', setter='setPetId', description='null', dataType='i64', datatypeWithEnum='i64', dataFormat='int64', name='pet_id', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.petId;', baseType='long', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int64",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=true, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='petId', nameInPascalCase='PetId', nameInSnakeCase='PET_ID', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int64, dependentRequired=null, contains=null}")
    }

    fn __serialize_pet_id<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_pet_id())
        }
}
impl Order {
    fn __name_for_quantity() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='quantity', complexType='integer', getter='getQuantity', setter='setQuantity', description='null', dataType='i32', datatypeWithEnum='i32', dataFormat='int32', name='quantity', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.quantity;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int32",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=true, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='quantity', nameInPascalCase='Quantity', nameInSnakeCase='QUANTITY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int32, dependentRequired=null, contains=null}")
    }

    fn __serialize_quantity<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_quantity())
        }
}
impl Order {
    fn __name_for_ship_date() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='shipDate', complexType='DateTime', getter='getShipDate', setter='setShipDate', description='null', dataType='chrono::DateTime::<chrono::Utc>', datatypeWithEnum='chrono::DateTime::<chrono::Utc>', dataFormat='date-time', name='ship_date', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.shipDate;', baseType='DateTime', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "date-time",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=true, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='shipDate', nameInPascalCase='ShipDate', nameInSnakeCase='SHIP_DATE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=date-time, dependentRequired=null, contains=null}")
    }

    fn __serialize_ship_date<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_ship_date())
        }
}
impl Order {
    fn __name_for_status() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='status', complexType='string', getter='getStatus', setter='setStatus', description='Order Status', dataType='String', datatypeWithEnum='Status', dataFormat='null', name='status', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.status;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='Order Status', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "Order Status",
  "enum" : [ "placed", "approved", "delivered" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[placed, approved, delivered], allowableValues={enumVars=[{name=Placed, isString=true, value="placed"}, {name=Approved, isString=true, value="approved"}, {name=Delivered, isString=true, value="delivered"}], values=[placed, approved, delivered]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='status', nameInPascalCase='Status', nameInSnakeCase='STATUS', enumName='Status', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_status<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_status())
        }
}
impl Order {
    fn __name_for_complete() -> String {
        String::from("CodegenProperty{openApiType='boolean', baseName='complete', complexType='boolean', getter='getComplete', setter='setComplete', description='null', dataType='bool', datatypeWithEnum='bool', dataFormat='null', name='complete', min='null', max='null', defaultValue='Some(false)', defaultValueWithParam=' = data.complete;', baseType='boolean', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "default" : false,
  "type" : "boolean"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=true, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='complete', nameInPascalCase='Complete', nameInSnakeCase='COMPLETE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_complete<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_complete())
        }
}


impl Order {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
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
impl std::fmt::Display for Order {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.id.as_ref().map(|id| {
                [
                    "id".to_string(),
                    id.to_string(),
                ].join(",")
            }),


            self.pet_id.as_ref().map(|pet_id| {
                [
                    "petId".to_string(),
                    pet_id.to_string(),
                ].join(",")
            }),


            self.quantity.as_ref().map(|quantity| {
                [
                    "quantity".to_string(),
                    quantity.to_string(),
                ].join(",")
            }),

            // Skipping shipDate in query parameter serialization


            self.status.as_ref().map(|status| {
                [
                    "status".to_string(),
                    status.to_string(),
                ].join(",")
            }),


            self.complete.as_ref().map(|complete| {
                [
                    "complete".to_string(),
                    complete.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Order value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Order {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
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
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Order".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "id" => intermediate_rep.id.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "petId" => intermediate_rep.pet_id.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "quantity" => intermediate_rep.quantity.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "shipDate" => intermediate_rep.ship_date.push(<chrono::DateTime::<chrono::Utc> as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "status" => intermediate_rep.status.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "complete" => intermediate_rep.complete.push(<bool as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Order".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Order {
            id: intermediate_rep.id.into_iter().next(),
            pet_id: intermediate_rep.pet_id.into_iter().next(),
            quantity: intermediate_rep.quantity.into_iter().next(),
            ship_date: intermediate_rep.ship_date.into_iter().next(),
            status: intermediate_rep.status.into_iter().next(),
            complete: intermediate_rep.complete.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Order> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Order>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Order>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Order - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Order> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Order as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into Order - {}",
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
pub struct OuterBoolean(bool);

impl validator::Validate for OuterBoolean {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<bool> for OuterBoolean {
    fn from(x: bool) -> Self {
        OuterBoolean(x)
    }
}

impl std::convert::From<OuterBoolean> for bool {
    fn from(x: OuterBoolean) -> Self {
        x.0
    }
}

impl std::ops::Deref for OuterBoolean {
    type Target = bool;
    fn deref(&self) -> &bool {
        &self.0
    }
}

impl std::ops::DerefMut for OuterBoolean {
    fn deref_mut(&mut self) -> &mut bool {
        &mut self.0
    }
}



#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
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
    fn __name_for_my_number() -> String {
        String::from("CodegenProperty{openApiType='number', baseName='my_number', complexType='number', getter='getMyNumber', setter='setMyNumber', description='null', dataType='f64', datatypeWithEnum='f64', dataFormat='null', name='my_number', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.my_number;', baseType='number', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "number"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=true, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='myNumber', nameInPascalCase='MyNumber', nameInSnakeCase='MY_NUMBER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_my_number<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_my_number())
        }
}
impl OuterComposite {
    fn __name_for_my_string() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='my_string', complexType='string', getter='getMyString', setter='setMyString', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='my_string', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.my_string;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='myString', nameInPascalCase='MyString', nameInSnakeCase='MY_STRING', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_my_string<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_my_string())
        }
}
impl OuterComposite {
    fn __name_for_my_boolean() -> String {
        String::from("CodegenProperty{openApiType='boolean', baseName='my_boolean', complexType='boolean', getter='getMyBoolean', setter='setMyBoolean', description='null', dataType='bool', datatypeWithEnum='bool', dataFormat='null', name='my_boolean', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.my_boolean;', baseType='boolean', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "boolean",
  "x-codegen-body-parameter-name" : "boolean_post_body"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=true, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={x-codegen-body-parameter-name=boolean_post_body}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='myBoolean', nameInPascalCase='MyBoolean', nameInSnakeCase='MY_BOOLEAN', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_my_boolean<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_my_boolean())
        }
}


impl OuterComposite {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
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
impl std::fmt::Display for OuterComposite {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.my_number.as_ref().map(|my_number| {
                [
                    "my_number".to_string(),
                    my_number.to_string(),
                ].join(",")
            }),


            self.my_string.as_ref().map(|my_string| {
                [
                    "my_string".to_string(),
                    my_string.to_string(),
                ].join(",")
            }),


            self.my_boolean.as_ref().map(|my_boolean| {
                [
                    "my_boolean".to_string(),
                    my_boolean.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a OuterComposite value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for OuterComposite {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub my_number: Vec<f64>,
            pub my_string: Vec<String>,
            pub my_boolean: Vec<bool>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing OuterComposite".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "my_number" => intermediate_rep.my_number.push(<f64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "my_string" => intermediate_rep.my_string.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "my_boolean" => intermediate_rep.my_boolean.push(<bool as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing OuterComposite".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(OuterComposite {
            my_number: intermediate_rep.my_number.into_iter().next(),
            my_string: intermediate_rep.my_string.into_iter().next(),
            my_boolean: intermediate_rep.my_boolean.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<OuterComposite> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<OuterComposite>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<OuterComposite>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for OuterComposite - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<OuterComposite> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <OuterComposite as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into OuterComposite - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




/// Enumeration of values.
/// Since this enum's variants do not hold data, we can easily define them as `#[repr(C)]`
/// which helps with FFI.
#[allow(non_camel_case_types)]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk_enum_derive::LabelledGenericEnum))]
pub enum OuterEnum {
    #[serde(rename = "placed")]
    Placed,
    #[serde(rename = "approved")]
    Approved,
    #[serde(rename = "delivered")]
    Delivered,
}

impl std::fmt::Display for OuterEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            OuterEnum::Placed => write!(f, "placed"),
            OuterEnum::Approved => write!(f, "approved"),
            OuterEnum::Delivered => write!(f, "delivered"),
        }
    }
}

impl std::str::FromStr for OuterEnum {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "placed" => std::result::Result::Ok(OuterEnum::Placed),
            "approved" => std::result::Result::Ok(OuterEnum::Approved),
            "delivered" => std::result::Result::Ok(OuterEnum::Delivered),
            _ => std::result::Result::Err(format!("Value not valid: {}", s)),
        }
    }
}


#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct OuterNumber(f64);

impl validator::Validate for OuterNumber {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<f64> for OuterNumber {
    fn from(x: f64) -> Self {
        OuterNumber(x)
    }
}

impl std::convert::From<OuterNumber> for f64 {
    fn from(x: OuterNumber) -> Self {
        x.0
    }
}

impl std::ops::Deref for OuterNumber {
    type Target = f64;
    fn deref(&self) -> &f64 {
        &self.0
    }
}

impl std::ops::DerefMut for OuterNumber {
    fn deref_mut(&mut self) -> &mut f64 {
        &mut self.0
    }
}



#[derive(Debug, Clone, PartialEq, PartialOrd, serde::Serialize, serde::Deserialize)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct OuterString(String);

impl validator::Validate for OuterString {
    fn validate(&self) -> std::result::Result<(), validator::ValidationErrors> {
        std::result::Result::Ok(())
    }
}

impl std::convert::From<String> for OuterString {
    fn from(x: String) -> Self {
        OuterString(x)
    }
}

impl std::fmt::Display for OuterString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       write!(f, "{:?}", self.0)
    }
}

impl std::str::FromStr for OuterString {
    type Err = std::string::ParseError;
    fn from_str(x: &str) -> std::result::Result<Self, Self::Err> {
        std::result::Result::Ok(OuterString(x.to_string()))
    }
}

impl std::convert::From<OuterString> for String {
    fn from(x: OuterString) -> Self {
        x.0
    }
}

impl std::ops::Deref for OuterString {
    type Target = String;
    fn deref(&self) -> &String {
        &self.0
    }
}

impl std::ops::DerefMut for OuterString {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}



#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
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
        /// Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "status")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub status: Option<String>,

}

impl Pet {
    fn __name_for_id() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='id', complexType='long', getter='getId', setter='setId', description='null', dataType='i64', datatypeWithEnum='i64', dataFormat='int64', name='id', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.id;', baseType='long', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int64",
  "type" : "integer",
  "x-is-unique" : true
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=true, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={x-is-unique=true}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='id', nameInPascalCase='Id', nameInSnakeCase='ID', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int64, dependentRequired=null, contains=null}")
    }

    fn __serialize_id<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_id())
        }
}
impl Pet {
    fn __name_for_category() -> String {
        String::from("CodegenProperty{openApiType='Category', baseName='category', complexType='Category', getter='getCategory', setter='setCategory', description='null', dataType='models::Category', datatypeWithEnum='models::Category', dataFormat='null', name='category', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.category;', baseType='Category', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "$ref" : "#/components/schemas/Category"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=true, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='category', nameInPascalCase='Category', nameInSnakeCase='CATEGORY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='Category', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=#/components/schemas/Category, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_category<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_category())
        }
}
impl Pet {
    fn __name_for_name() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='name', complexType='string', getter='getName', setter='setName', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='name', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.name;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='doggie', jsonSchema='{
  "example" : "doggie",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='name', nameInPascalCase='Name', nameInSnakeCase='NAME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_name<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_name())
        }
}
impl Pet {
    fn __name_for_photo_urls() -> String {
        String::from("CodegenProperty{openApiType='array', baseName='photoUrls', complexType='string', getter='getPhotoUrls', setter='setPhotoUrls', description='null', dataType='Vec<String>', datatypeWithEnum='Vec<String>', dataFormat='null', name='photo_urls', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.photoUrls;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "type" : "string"
  },
  "type" : "array",
  "xml" : {
    "name" : "photoUrl",
    "wrapped" : true
  }
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='string', baseName='photo_urls', complexType='string', getter='getPhotoUrls', setter='setPhotoUrls', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='photo_urls', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.photo_urls;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='photoUrls', nameInPascalCase='PhotoUrls', nameInSnakeCase='PHOTO_URLS', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='string', baseName='photo_urls', complexType='string', getter='getPhotoUrls', setter='setPhotoUrls', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='photo_urls', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.photo_urls;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='photoUrls', nameInPascalCase='PhotoUrls', nameInSnakeCase='PHOTO_URLS', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='photoUrls', nameInPascalCase='PhotoUrls', nameInSnakeCase='PHOTO_URLS', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='photoUrl', xmlNamespace='null', isXmlWrapped=true, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_photo_urls<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_photo_urls())
        }
}
impl Pet {
    fn __name_for_tags() -> String {
        String::from("CodegenProperty{openApiType='array', baseName='tags', complexType='Tag', getter='getTags', setter='setTags', description='null', dataType='Vec<models::Tag>', datatypeWithEnum='Vec<models::Tag>', dataFormat='null', name='tags', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.tags;', baseType='array', containerType='array', containerTypeMapped='Vec', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "items" : {
    "$ref" : "#/components/schemas/Tag"
  },
  "type" : "array",
  "xml" : {
    "name" : "tag",
    "wrapped" : true
  }
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=true, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=true, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=CodegenProperty{openApiType='Tag', baseName='tags', complexType='Tag', getter='getTags', setter='setTags', description='null', dataType='models::Tag', datatypeWithEnum='models::Tag', dataFormat='null', name='tags', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.tags;', baseType='Tag', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "$ref" : "#/components/schemas/Tag"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=true, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='tags', nameInPascalCase='Tags', nameInSnakeCase='TAGS', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='Tag', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=#/components/schemas/Tag, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=CodegenProperty{openApiType='Tag', baseName='tags', complexType='Tag', getter='getTags', setter='setTags', description='null', dataType='models::Tag', datatypeWithEnum='models::Tag', dataFormat='null', name='tags', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.tags;', baseType='Tag', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "$ref" : "#/components/schemas/Tag"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=true, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='tags', nameInPascalCase='Tags', nameInSnakeCase='TAGS', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='Tag', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=#/components/schemas/Tag, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='tags', nameInPascalCase='Tags', nameInSnakeCase='TAGS', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='tag', xmlNamespace='null', isXmlWrapped=true, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_tags<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_tags())
        }
}
impl Pet {
    fn __name_for_status() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='status', complexType='string', getter='getStatus', setter='setStatus', description='pet status in the store', dataType='String', datatypeWithEnum='Status', dataFormat='null', name='status', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.status;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='pet status in the store', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "pet status in the store",
  "enum" : [ "available", "pending", "sold" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[available, pending, sold], allowableValues={enumVars=[{name=Available, isString=true, value="available"}, {name=Pending, isString=true, value="pending"}, {name=Sold, isString=true, value="sold"}], values=[available, pending, sold]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='status', nameInPascalCase='Status', nameInSnakeCase='STATUS', enumName='Status', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_status<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_status())
        }
}


impl Pet {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(name: String, photo_urls: Vec<String>, ) -> Pet {
        Pet {
            id: None,
            category: None,
            name,
            photo_urls,
            tags: None,
            status: None,
        }
    }
}

/// Converts the Pet value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Pet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.id.as_ref().map(|id| {
                [
                    "id".to_string(),
                    id.to_string(),
                ].join(",")
            }),

            // Skipping category in query parameter serialization


            Some("name".to_string()),
            Some(self.name.to_string()),


            Some("photoUrls".to_string()),
            Some(self.photo_urls.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(",")),

            // Skipping tags in query parameter serialization


            self.status.as_ref().map(|status| {
                [
                    "status".to_string(),
                    status.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Pet value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Pet {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
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
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Pet".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "id" => intermediate_rep.id.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "category" => intermediate_rep.category.push(<models::Category as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "name" => intermediate_rep.name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    "photoUrls" => return std::result::Result::Err("Parsing a container in this style is not supported in Pet".to_string()),
                    "tags" => return std::result::Result::Err("Parsing a container in this style is not supported in Pet".to_string()),
                    #[allow(clippy::redundant_clone)]
                    "status" => intermediate_rep.status.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Pet".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Pet {
            id: intermediate_rep.id.into_iter().next(),
            category: intermediate_rep.category.into_iter().next(),
            name: intermediate_rep.name.into_iter().next().ok_or_else(|| "name missing in Pet".to_string())?,
            photo_urls: intermediate_rep.photo_urls.into_iter().next().ok_or_else(|| "photoUrls missing in Pet".to_string())?,
            tags: intermediate_rep.tags.into_iter().next(),
            status: intermediate_rep.status.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Pet> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Pet>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Pet>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Pet - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Pet> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Pet as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into Pet - {}",
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
pub struct ReadOnlyFirst {
    #[serde(rename = "bar")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub bar: Option<String>,

    #[serde(rename = "baz")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub baz: Option<String>,

}

impl ReadOnlyFirst {
    fn __name_for_bar() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='bar', complexType='string', getter='getBar', setter='setBar', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='bar', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.bar;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "readOnly" : true,
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=true, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='bar', nameInPascalCase='Bar', nameInSnakeCase='BAR', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_bar<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_bar())
        }
}
impl ReadOnlyFirst {
    fn __name_for_baz() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='baz', complexType='string', getter='getBaz', setter='setBaz', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='baz', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.baz;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='baz', nameInPascalCase='Baz', nameInSnakeCase='BAZ', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_baz<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_baz())
        }
}


impl ReadOnlyFirst {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
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
impl std::fmt::Display for ReadOnlyFirst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.bar.as_ref().map(|bar| {
                [
                    "bar".to_string(),
                    bar.to_string(),
                ].join(",")
            }),


            self.baz.as_ref().map(|baz| {
                [
                    "baz".to_string(),
                    baz.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a ReadOnlyFirst value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for ReadOnlyFirst {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub bar: Vec<String>,
            pub baz: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing ReadOnlyFirst".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "bar" => intermediate_rep.bar.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "baz" => intermediate_rep.baz.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing ReadOnlyFirst".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(ReadOnlyFirst {
            bar: intermediate_rep.bar.into_iter().next(),
            baz: intermediate_rep.baz.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<ReadOnlyFirst> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<ReadOnlyFirst>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<ReadOnlyFirst>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for ReadOnlyFirst - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<ReadOnlyFirst> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <ReadOnlyFirst as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into ReadOnlyFirst - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}




/// Model for testing reserved words
#[derive(Debug, Clone, PartialEq, serde::Serialize, serde::Deserialize, validator::Validate)]
#[cfg_attr(feature = "conversion", derive(frunk::LabelledGeneric))]
pub struct Return {
    #[serde(rename = "return")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub r#return: Option<i32>,

}

impl Return {
    fn __name_for_r#return() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='return', complexType='integer', getter='getR#return', setter='setR#return', description='null', dataType='i32', datatypeWithEnum='i32', dataFormat='int32', name='r#return', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.return;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int32",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=true, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='r#return', nameInPascalCase='R#return', nameInSnakeCase='R#RETURN', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int32, dependentRequired=null, contains=null}")
    }

    fn __serialize_r#return<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_r#return())
        }
}


impl Return {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> Return {
        Return {
            r#return: None,
        }
    }
}

/// Converts the Return value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.r#return.as_ref().map(|r#return| {
                [
                    "return".to_string(),
                    r#return.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Return value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Return {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub r#return: Vec<i32>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Return".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "return" => intermediate_rep.r#return.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Return".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Return {
            r#return: intermediate_rep.r#return.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Return> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Return>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Return>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Return - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Return> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Return as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into Return - {}",
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
pub struct Tag {
    #[serde(rename = "id")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub id: Option<i64>,

    #[serde(rename = "name")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub name: Option<String>,

}

impl Tag {
    fn __name_for_id() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='id', complexType='long', getter='getId', setter='setId', description='null', dataType='i64', datatypeWithEnum='i64', dataFormat='int64', name='id', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.id;', baseType='long', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int64",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=true, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='id', nameInPascalCase='Id', nameInSnakeCase='ID', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int64, dependentRequired=null, contains=null}")
    }

    fn __serialize_id<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_id())
        }
}
impl Tag {
    fn __name_for_name() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='name', complexType='string', getter='getName', setter='setName', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='name', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.name;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='name', nameInPascalCase='Name', nameInSnakeCase='NAME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_name<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_name())
        }
}


impl Tag {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
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
impl std::fmt::Display for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.id.as_ref().map(|id| {
                [
                    "id".to_string(),
                    id.to_string(),
                ].join(",")
            }),


            self.name.as_ref().map(|name| {
                [
                    "name".to_string(),
                    name.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a Tag value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for Tag {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub id: Vec<i64>,
            pub name: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing Tag".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "id" => intermediate_rep.id.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "name" => intermediate_rep.name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing Tag".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(Tag {
            id: intermediate_rep.id.into_iter().next(),
            name: intermediate_rep.name.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<Tag> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<Tag>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<Tag>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for Tag - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<Tag> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <Tag as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into Tag - {}",
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
pub struct TestEndpointParametersRequest {
    /// None
    #[serde(rename = "integer")]
    #[validate(
            range(min = 10u8, max = 100u8),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub integer: Option<u8>,

    /// None
    #[serde(rename = "int32")]
    #[validate(
            range(min = 20u8, max = 200u8),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub int32: Option<u8>,

    /// None
    #[serde(rename = "int64")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub int64: Option<i64>,

    /// None
    #[serde(rename = "number")]
    #[validate(
            range(min = 32.1f64, max = 543.2f64),
        )]
    pub number: f64,

    /// None
    #[serde(rename = "float")]
    #[validate(
            range(max = 987.6f32),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub float: Option<f32>,

    /// None
    #[serde(rename = "double")]
    #[validate(
            range(min = 67.8f64, max = 123.4f64),
        )]
    pub double: f64,

    /// None
    #[serde(rename = "string")]
    #[validate(
            regex(path = *RE_TESTENDPOINTPARAMETERSREQUEST_STRING),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub string: Option<String>,

    /// None
    #[serde(rename = "pattern_without_delimiter")]
    #[validate(
            regex(path = *RE_TESTENDPOINTPARAMETERSREQUEST_PATTERN_WITHOUT_DELIMITER),
        )]
    pub pattern_without_delimiter: String,

    /// None
    #[serde(rename = "byte")]
    pub byte: ByteArray,

    /// None
    #[serde(rename = "binary")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub binary: Option<ByteArray>,

    /// None
    #[serde(rename = "date")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub date: Option<chrono::naive::NaiveDate>,

    /// None
    #[serde(rename = "dateTime")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub date_time: Option<chrono::DateTime::<chrono::Utc>>,

    /// None
    #[serde(rename = "password")]
    #[validate(
            length(min = 10, max = 64),
        )]
    #[serde(skip_serializing_if="Option::is_none")]
    pub password: Option<String>,

    /// None
    #[serde(rename = "callback")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub callback: Option<String>,

}

impl TestEndpointParametersRequest {
    fn __name_for_integer() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='integer', complexType='integer', getter='getInteger', setter='setInteger', description='None', dataType='u8', datatypeWithEnum='i32', dataFormat='int32', name='integer', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.integer;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='None', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "None",
  "format" : "int32",
  "maximum" : 100,
  "minimum" : 10,
  "type" : "integer"
}', minimum='10', maximum='100', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=true, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='integer', nameInPascalCase='Integer', nameInSnakeCase='INTEGER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int32, dependentRequired=null, contains=null}")
    }

    fn __serialize_integer<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_integer())
        }
}
impl TestEndpointParametersRequest {
    fn __name_for_int32() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='int32', complexType='integer', getter='getInt32', setter='setInt32', description='None', dataType='u8', datatypeWithEnum='i32', dataFormat='int32', name='int32', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.int32;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='None', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "None",
  "format" : "int32",
  "maximum" : 200,
  "minimum" : 20,
  "type" : "integer"
}', minimum='20', maximum='200', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=true, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='int32', nameInPascalCase='Int32', nameInSnakeCase='INT32', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int32, dependentRequired=null, contains=null}")
    }

    fn __serialize_int32<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_int32())
        }
}
impl TestEndpointParametersRequest {
    fn __name_for_int64() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='int64', complexType='long', getter='getInt64', setter='setInt64', description='None', dataType='i64', datatypeWithEnum='i64', dataFormat='int64', name='int64', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.int64;', baseType='long', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='None', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "None",
  "format" : "int64",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=true, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='int64', nameInPascalCase='Int64', nameInSnakeCase='INT64', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int64, dependentRequired=null, contains=null}")
    }

    fn __serialize_int64<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_int64())
        }
}
impl TestEndpointParametersRequest {
    fn __name_for_number() -> String {
        String::from("CodegenProperty{openApiType='number', baseName='number', complexType='number', getter='getNumber', setter='setNumber', description='None', dataType='f64', datatypeWithEnum='f64', dataFormat='null', name='number', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.number;', baseType='number', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='None', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "None",
  "maximum" : 543.2,
  "minimum" : 32.1,
  "type" : "number"
}', minimum='32.1', maximum='543.2', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=true, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='number', nameInPascalCase='Number', nameInSnakeCase='NUMBER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_number<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_number())
        }
}
impl TestEndpointParametersRequest {
    fn __name_for_float() -> String {
        String::from("CodegenProperty{openApiType='number', baseName='float', complexType='float', getter='getFloat', setter='setFloat', description='None', dataType='f32', datatypeWithEnum='f32', dataFormat='float', name='float', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.float;', baseType='float', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='None', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "None",
  "format" : "float",
  "maximum" : 987.6,
  "type" : "number"
}', minimum='null', maximum='987.6', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=true, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='float', nameInPascalCase='Float', nameInSnakeCase='FLOAT', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=float, dependentRequired=null, contains=null}")
    }

    fn __serialize_float<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_float())
        }
}
impl TestEndpointParametersRequest {
    fn __name_for_double() -> String {
        String::from("CodegenProperty{openApiType='number', baseName='double', complexType='double', getter='getDouble', setter='setDouble', description='None', dataType='f64', datatypeWithEnum='f64', dataFormat='double', name='double', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.double;', baseType='double', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='None', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "None",
  "format" : "double",
  "maximum" : 123.4,
  "minimum" : 67.8,
  "type" : "number"
}', minimum='67.8', maximum='123.4', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=true, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='double', nameInPascalCase='Double', nameInSnakeCase='DOUBLE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=double, dependentRequired=null, contains=null}")
    }

    fn __serialize_double<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_double())
        }
}
impl TestEndpointParametersRequest {
    fn __name_for_string() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='string', complexType='string', getter='getString', setter='setString', description='None', dataType='String', datatypeWithEnum='String', dataFormat='null', name='string', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.string;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='None', maxLength=null, minLength=null, pattern='/[a-z]/i', example='null', jsonSchema='{
  "description" : "None",
  "pattern" : "/[a-z]/i",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='string', nameInPascalCase='String', nameInSnakeCase='STRING', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_string<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_string())
        }
}
impl TestEndpointParametersRequest {
    fn __name_for_pattern_without_delimiter() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='pattern_without_delimiter', complexType='string', getter='getPatternWithoutDelimiter', setter='setPatternWithoutDelimiter', description='None', dataType='String', datatypeWithEnum='String', dataFormat='null', name='pattern_without_delimiter', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.pattern_without_delimiter;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='None', maxLength=null, minLength=null, pattern='^[A-Z].*', example='null', jsonSchema='{
  "description" : "None",
  "pattern" : "^[A-Z].*",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='patternWithoutDelimiter', nameInPascalCase='PatternWithoutDelimiter', nameInSnakeCase='PATTERN_WITHOUT_DELIMITER', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_pattern_without_delimiter<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_pattern_without_delimiter())
        }
}
impl TestEndpointParametersRequest {
    fn __name_for_byte() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='byte', complexType='ByteArray', getter='getByte', setter='setByte', description='None', dataType='ByteArray', datatypeWithEnum='ByteArray', dataFormat='byte', name='byte', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.byte;', baseType='ByteArray', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='None', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "None",
  "format" : "byte",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=true, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='byte', nameInPascalCase='Byte', nameInSnakeCase='BYTE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=byte, dependentRequired=null, contains=null}")
    }

    fn __serialize_byte<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_byte())
        }
}
impl TestEndpointParametersRequest {
    fn __name_for_binary() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='binary', complexType='file', getter='getBinary', setter='setBinary', description='None', dataType='ByteArray', datatypeWithEnum='ByteArray', dataFormat='binary', name='binary', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.binary;', baseType='file', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='None', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "None",
  "format" : "binary",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=true, isFile=true, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='binary', nameInPascalCase='Binary', nameInSnakeCase='BINARY', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=binary, dependentRequired=null, contains=null}")
    }

    fn __serialize_binary<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_binary())
        }
}
impl TestEndpointParametersRequest {
    fn __name_for_date() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='date', complexType='date', getter='getDate', setter='setDate', description='None', dataType='chrono::naive::NaiveDate', datatypeWithEnum='chrono::naive::NaiveDate', dataFormat='date', name='date', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.date;', baseType='date', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='None', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "None",
  "format" : "date",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=true, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='date', nameInPascalCase='Date', nameInSnakeCase='DATE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=date, dependentRequired=null, contains=null}")
    }

    fn __serialize_date<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_date())
        }
}
impl TestEndpointParametersRequest {
    fn __name_for_date_time() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='dateTime', complexType='DateTime', getter='getDateTime', setter='setDateTime', description='None', dataType='chrono::DateTime::<chrono::Utc>', datatypeWithEnum='chrono::DateTime::<chrono::Utc>', dataFormat='date-time', name='date_time', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.dateTime;', baseType='DateTime', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='None', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "None",
  "format" : "date-time",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=true, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='dateTime', nameInPascalCase='DateTime', nameInSnakeCase='DATE_TIME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=date-time, dependentRequired=null, contains=null}")
    }

    fn __serialize_date_time<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_date_time())
        }
}
impl TestEndpointParametersRequest {
    fn __name_for_password() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='password', complexType='password', getter='getPassword', setter='setPassword', description='None', dataType='String', datatypeWithEnum='String', dataFormat='password', name='password', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.password;', baseType='password', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='None', maxLength=64, minLength=10, pattern='null', example='null', jsonSchema='{
  "description" : "None",
  "format" : "password",
  "maxLength" : 64,
  "minLength" : 10,
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=true, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=true, isInherited=false, discriminatorValue='null', nameInCamelCase='password', nameInPascalCase='Password', nameInSnakeCase='PASSWORD', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=password, dependentRequired=null, contains=null}")
    }

    fn __serialize_password<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_password())
        }
}
impl TestEndpointParametersRequest {
    fn __name_for_callback() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='callback', complexType='string', getter='getCallback', setter='setCallback', description='None', dataType='String', datatypeWithEnum='String', dataFormat='null', name='callback', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.callback;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='None', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "None",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='callback', nameInPascalCase='Callback', nameInSnakeCase='CALLBACK', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_callback<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_callback())
        }
}

lazy_static::lazy_static! {
    static ref RE_TESTENDPOINTPARAMETERSREQUEST_STRING: regex::Regex = regex::Regex::new(r"/[a-z]/i").unwrap();
}
lazy_static::lazy_static! {
    static ref RE_TESTENDPOINTPARAMETERSREQUEST_PATTERN_WITHOUT_DELIMITER: regex::Regex = regex::Regex::new(r"^[A-Z].*").unwrap();
}

impl TestEndpointParametersRequest {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(number: f64, double: f64, pattern_without_delimiter: String, byte: ByteArray, ) -> TestEndpointParametersRequest {
        TestEndpointParametersRequest {
            integer: None,
            int32: None,
            int64: None,
            number,
            float: None,
            double,
            string: None,
            pattern_without_delimiter,
            byte,
            binary: None,
            date: None,
            date_time: None,
            password: None,
            callback: None,
        }
    }
}

/// Converts the TestEndpointParametersRequest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for TestEndpointParametersRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.integer.as_ref().map(|integer| {
                [
                    "integer".to_string(),
                    integer.to_string(),
                ].join(",")
            }),


            self.int32.as_ref().map(|int32| {
                [
                    "int32".to_string(),
                    int32.to_string(),
                ].join(",")
            }),


            self.int64.as_ref().map(|int64| {
                [
                    "int64".to_string(),
                    int64.to_string(),
                ].join(",")
            }),


            Some("number".to_string()),
            Some(self.number.to_string()),


            self.float.as_ref().map(|float| {
                [
                    "float".to_string(),
                    float.to_string(),
                ].join(",")
            }),


            Some("double".to_string()),
            Some(self.double.to_string()),


            self.string.as_ref().map(|string| {
                [
                    "string".to_string(),
                    string.to_string(),
                ].join(",")
            }),


            Some("pattern_without_delimiter".to_string()),
            Some(self.pattern_without_delimiter.to_string()),

            // Skipping byte in query parameter serialization
            // Skipping byte in query parameter serialization

            // Skipping binary in query parameter serialization
            // Skipping binary in query parameter serialization

            // Skipping date in query parameter serialization

            // Skipping dateTime in query parameter serialization


            self.password.as_ref().map(|password| {
                [
                    "password".to_string(),
                    password.to_string(),
                ].join(",")
            }),


            self.callback.as_ref().map(|callback| {
                [
                    "callback".to_string(),
                    callback.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a TestEndpointParametersRequest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for TestEndpointParametersRequest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub integer: Vec<u8>,
            pub int32: Vec<u8>,
            pub int64: Vec<i64>,
            pub number: Vec<f64>,
            pub float: Vec<f32>,
            pub double: Vec<f64>,
            pub string: Vec<String>,
            pub pattern_without_delimiter: Vec<String>,
            pub byte: Vec<ByteArray>,
            pub binary: Vec<ByteArray>,
            pub date: Vec<chrono::naive::NaiveDate>,
            pub date_time: Vec<chrono::DateTime::<chrono::Utc>>,
            pub password: Vec<String>,
            pub callback: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing TestEndpointParametersRequest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "integer" => intermediate_rep.integer.push(<u8 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "int32" => intermediate_rep.int32.push(<u8 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "int64" => intermediate_rep.int64.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "number" => intermediate_rep.number.push(<f64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "float" => intermediate_rep.float.push(<f32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "double" => intermediate_rep.double.push(<f64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "string" => intermediate_rep.string.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "pattern_without_delimiter" => intermediate_rep.pattern_without_delimiter.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    "byte" => return std::result::Result::Err("Parsing binary data in this style is not supported in TestEndpointParametersRequest".to_string()),
                    "binary" => return std::result::Result::Err("Parsing binary data in this style is not supported in TestEndpointParametersRequest".to_string()),
                    #[allow(clippy::redundant_clone)]
                    "date" => intermediate_rep.date.push(<chrono::naive::NaiveDate as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "dateTime" => intermediate_rep.date_time.push(<chrono::DateTime::<chrono::Utc> as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "password" => intermediate_rep.password.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "callback" => intermediate_rep.callback.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing TestEndpointParametersRequest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(TestEndpointParametersRequest {
            integer: intermediate_rep.integer.into_iter().next(),
            int32: intermediate_rep.int32.into_iter().next(),
            int64: intermediate_rep.int64.into_iter().next(),
            number: intermediate_rep.number.into_iter().next().ok_or_else(|| "number missing in TestEndpointParametersRequest".to_string())?,
            float: intermediate_rep.float.into_iter().next(),
            double: intermediate_rep.double.into_iter().next().ok_or_else(|| "double missing in TestEndpointParametersRequest".to_string())?,
            string: intermediate_rep.string.into_iter().next(),
            pattern_without_delimiter: intermediate_rep.pattern_without_delimiter.into_iter().next().ok_or_else(|| "pattern_without_delimiter missing in TestEndpointParametersRequest".to_string())?,
            byte: intermediate_rep.byte.into_iter().next().ok_or_else(|| "byte missing in TestEndpointParametersRequest".to_string())?,
            binary: intermediate_rep.binary.into_iter().next(),
            date: intermediate_rep.date.into_iter().next(),
            date_time: intermediate_rep.date_time.into_iter().next(),
            password: intermediate_rep.password.into_iter().next(),
            callback: intermediate_rep.callback.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<TestEndpointParametersRequest> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<TestEndpointParametersRequest>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<TestEndpointParametersRequest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for TestEndpointParametersRequest - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<TestEndpointParametersRequest> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <TestEndpointParametersRequest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into TestEndpointParametersRequest - {}",
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
pub struct TestEnumParametersRequest {
    /// Form parameter enum test (string)
        /// Note: inline enums are not fully supported by openapi-generator
    #[serde(rename = "enum_form_string")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub enum_form_string: Option<String>,

}

impl TestEnumParametersRequest {
    fn __name_for_enum_form_string() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='enum_form_string', complexType='string', getter='getEnumFormString', setter='setEnumFormString', description='Form parameter enum test (string)', dataType='String', datatypeWithEnum='EnumFormString', dataFormat='null', name='enum_form_string', min='null', max='null', defaultValue='Some(r#"-efg"#.to_string())', defaultValueWithParam=' = data.enum_form_string;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='Form parameter enum test (string)', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "default" : "-efg",
  "description" : "Form parameter enum test (string)",
  "enum" : [ "_abc", "-efg", "(xyz)" ],
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=true, isInnerEnum=true, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=[_abc, -efg, (xyz)], allowableValues={enumVars=[{name=Abc, isString=true, value="_abc"}, {name=Efg, isString=true, value="-efg"}, {name=LeftParenthesisXyzRightParenthesis, isString=true, value="(xyz)"}], values=[_abc, -efg, (xyz)]}, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='enumFormString', nameInPascalCase='EnumFormString', nameInSnakeCase='ENUM_FORM_STRING', enumName='EnumFormString', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_enum_form_string<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_enum_form_string())
        }
}


impl TestEnumParametersRequest {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> TestEnumParametersRequest {
        TestEnumParametersRequest {
            enum_form_string: Some(r#"-efg"#.to_string()),
        }
    }
}

/// Converts the TestEnumParametersRequest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for TestEnumParametersRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.enum_form_string.as_ref().map(|enum_form_string| {
                [
                    "enum_form_string".to_string(),
                    enum_form_string.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a TestEnumParametersRequest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for TestEnumParametersRequest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub enum_form_string: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing TestEnumParametersRequest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "enum_form_string" => intermediate_rep.enum_form_string.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing TestEnumParametersRequest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(TestEnumParametersRequest {
            enum_form_string: intermediate_rep.enum_form_string.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<TestEnumParametersRequest> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<TestEnumParametersRequest>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<TestEnumParametersRequest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for TestEnumParametersRequest - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<TestEnumParametersRequest> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <TestEnumParametersRequest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into TestEnumParametersRequest - {}",
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
pub struct TestJsonFormDataRequest {
    /// field1
    #[serde(rename = "param")]
    pub param: String,

    /// field2
    #[serde(rename = "param2")]
    pub param2: String,

}

impl TestJsonFormDataRequest {
    fn __name_for_param() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='param', complexType='string', getter='getParam', setter='setParam', description='field1', dataType='String', datatypeWithEnum='String', dataFormat='null', name='param', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.param;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='field1', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "field1",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='param', nameInPascalCase='Param', nameInSnakeCase='PARAM', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_param<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_param())
        }
}
impl TestJsonFormDataRequest {
    fn __name_for_param2() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='param2', complexType='string', getter='getParam2', setter='setParam2', description='field2', dataType='String', datatypeWithEnum='String', dataFormat='null', name='param2', min='null', max='null', defaultValue='null', defaultValueWithParam=' = data.param2;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='field2', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "field2",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=true, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='param2', nameInPascalCase='Param2', nameInSnakeCase='PARAM2', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_param2<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_param2())
        }
}


impl TestJsonFormDataRequest {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new(param: String, param2: String, ) -> TestJsonFormDataRequest {
        TestJsonFormDataRequest {
            param,
            param2,
        }
    }
}

/// Converts the TestJsonFormDataRequest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for TestJsonFormDataRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            Some("param".to_string()),
            Some(self.param.to_string()),


            Some("param2".to_string()),
            Some(self.param2.to_string()),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a TestJsonFormDataRequest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for TestJsonFormDataRequest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub param: Vec<String>,
            pub param2: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing TestJsonFormDataRequest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "param" => intermediate_rep.param.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "param2" => intermediate_rep.param2.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing TestJsonFormDataRequest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(TestJsonFormDataRequest {
            param: intermediate_rep.param.into_iter().next().ok_or_else(|| "param missing in TestJsonFormDataRequest".to_string())?,
            param2: intermediate_rep.param2.into_iter().next().ok_or_else(|| "param2 missing in TestJsonFormDataRequest".to_string())?,
        })
    }
}

// Methods for converting between header::IntoHeaderValue<TestJsonFormDataRequest> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<TestJsonFormDataRequest>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<TestJsonFormDataRequest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for TestJsonFormDataRequest - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<TestJsonFormDataRequest> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <TestJsonFormDataRequest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into TestJsonFormDataRequest - {}",
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
pub struct UpdatePetWithFormRequest {
    /// Updated name of the pet
    #[serde(rename = "name")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub name: Option<String>,

    /// Updated status of the pet
    #[serde(rename = "status")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub status: Option<String>,

}

impl UpdatePetWithFormRequest {
    fn __name_for_name() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='name', complexType='string', getter='getName', setter='setName', description='Updated name of the pet', dataType='String', datatypeWithEnum='String', dataFormat='null', name='name', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.name;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='Updated name of the pet', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "Updated name of the pet",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='name', nameInPascalCase='Name', nameInSnakeCase='NAME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_name<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_name())
        }
}
impl UpdatePetWithFormRequest {
    fn __name_for_status() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='status', complexType='string', getter='getStatus', setter='setStatus', description='Updated status of the pet', dataType='String', datatypeWithEnum='String', dataFormat='null', name='status', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.status;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='Updated status of the pet', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "Updated status of the pet",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='status', nameInPascalCase='Status', nameInSnakeCase='STATUS', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_status<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_status())
        }
}


impl UpdatePetWithFormRequest {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> UpdatePetWithFormRequest {
        UpdatePetWithFormRequest {
            name: None,
            status: None,
        }
    }
}

/// Converts the UpdatePetWithFormRequest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for UpdatePetWithFormRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.name.as_ref().map(|name| {
                [
                    "name".to_string(),
                    name.to_string(),
                ].join(",")
            }),


            self.status.as_ref().map(|status| {
                [
                    "status".to_string(),
                    status.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a UpdatePetWithFormRequest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for UpdatePetWithFormRequest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub name: Vec<String>,
            pub status: Vec<String>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing UpdatePetWithFormRequest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "name" => intermediate_rep.name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "status" => intermediate_rep.status.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing UpdatePetWithFormRequest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(UpdatePetWithFormRequest {
            name: intermediate_rep.name.into_iter().next(),
            status: intermediate_rep.status.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<UpdatePetWithFormRequest> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<UpdatePetWithFormRequest>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<UpdatePetWithFormRequest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for UpdatePetWithFormRequest - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<UpdatePetWithFormRequest> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <UpdatePetWithFormRequest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into UpdatePetWithFormRequest - {}",
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
pub struct UploadFileRequest {
    /// Additional data to pass to server
    #[serde(rename = "additionalMetadata")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub additional_metadata: Option<String>,

    /// file to upload
    #[serde(rename = "file")]
    #[serde(skip_serializing_if="Option::is_none")]
    pub file: Option<ByteArray>,

}

impl UploadFileRequest {
    fn __name_for_additional_metadata() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='additionalMetadata', complexType='string', getter='getAdditionalMetadata', setter='setAdditionalMetadata', description='Additional data to pass to server', dataType='String', datatypeWithEnum='String', dataFormat='null', name='additional_metadata', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.additionalMetadata;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='Additional data to pass to server', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "Additional data to pass to server",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='additionalMetadata', nameInPascalCase='AdditionalMetadata', nameInSnakeCase='ADDITIONAL_METADATA', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_additional_metadata<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_additional_metadata())
        }
}
impl UploadFileRequest {
    fn __name_for_file() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='file', complexType='file', getter='getFile', setter='setFile', description='file to upload', dataType='ByteArray', datatypeWithEnum='ByteArray', dataFormat='binary', name='file', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.file;', baseType='file', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='file to upload', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "file to upload",
  "format" : "binary",
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=false, isModel=false, isContainer=false, isString=false, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=true, isFile=true, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='file', nameInPascalCase='File', nameInSnakeCase='FILE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=binary, dependentRequired=null, contains=null}")
    }

    fn __serialize_file<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_file())
        }
}


impl UploadFileRequest {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
    pub fn new() -> UploadFileRequest {
        UploadFileRequest {
            additional_metadata: None,
            file: None,
        }
    }
}

/// Converts the UploadFileRequest value to the Query Parameters representation (style=form, explode=false)
/// specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde serializer
impl std::fmt::Display for UploadFileRequest {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.additional_metadata.as_ref().map(|additional_metadata| {
                [
                    "additionalMetadata".to_string(),
                    additional_metadata.to_string(),
                ].join(",")
            }),

            // Skipping file in query parameter serialization
            // Skipping file in query parameter serialization

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a UploadFileRequest value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for UploadFileRequest {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
        struct IntermediateRep {
            pub additional_metadata: Vec<String>,
            pub file: Vec<ByteArray>,
        }

        let mut intermediate_rep = IntermediateRep::default();

        // Parse into intermediate representation
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing UploadFileRequest".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "additionalMetadata" => intermediate_rep.additional_metadata.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    "file" => return std::result::Result::Err("Parsing binary data in this style is not supported in UploadFileRequest".to_string()),
                    _ => return std::result::Result::Err("Unexpected key while parsing UploadFileRequest".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(UploadFileRequest {
            additional_metadata: intermediate_rep.additional_metadata.into_iter().next(),
            file: intermediate_rep.file.into_iter().next(),
        })
    }
}

// Methods for converting between header::IntoHeaderValue<UploadFileRequest> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<UploadFileRequest>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<UploadFileRequest>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for UploadFileRequest - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<UploadFileRequest> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <UploadFileRequest as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into UploadFileRequest - {}",
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
    fn __name_for_id() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='id', complexType='long', getter='getId', setter='setId', description='null', dataType='i64', datatypeWithEnum='i64', dataFormat='int64', name='id', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.id;', baseType='long', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "format" : "int64",
  "type" : "integer",
  "x-is-unique" : true
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=false, isShort=false, isLong=true, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={x-is-unique=true}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='id', nameInPascalCase='Id', nameInSnakeCase='ID', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int64, dependentRequired=null, contains=null}")
    }

    fn __serialize_id<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_id())
        }
}
impl User {
    fn __name_for_username() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='username', complexType='string', getter='getUsername', setter='setUsername', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='username', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.username;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='username', nameInPascalCase='Username', nameInSnakeCase='USERNAME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_username<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_username())
        }
}
impl User {
    fn __name_for_first_name() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='firstName', complexType='string', getter='getFirstName', setter='setFirstName', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='first_name', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.firstName;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='firstName', nameInPascalCase='FirstName', nameInSnakeCase='FIRST_NAME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_first_name<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_first_name())
        }
}
impl User {
    fn __name_for_last_name() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='lastName', complexType='string', getter='getLastName', setter='setLastName', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='last_name', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.lastName;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='lastName', nameInPascalCase='LastName', nameInSnakeCase='LAST_NAME', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_last_name<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_last_name())
        }
}
impl User {
    fn __name_for_email() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='email', complexType='string', getter='getEmail', setter='setEmail', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='email', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.email;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='email', nameInPascalCase='Email', nameInSnakeCase='EMAIL', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_email<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_email())
        }
}
impl User {
    fn __name_for_password() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='password', complexType='string', getter='getPassword', setter='setPassword', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='password', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.password;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='password', nameInPascalCase='Password', nameInSnakeCase='PASSWORD', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_password<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_password())
        }
}
impl User {
    fn __name_for_phone() -> String {
        String::from("CodegenProperty{openApiType='string', baseName='phone', complexType='string', getter='getPhone', setter='setPhone', description='null', dataType='String', datatypeWithEnum='String', dataFormat='null', name='phone', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.phone;', baseType='string', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='null', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "type" : "string"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=true, isNumeric=false, isInteger=false, isShort=false, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='phone', nameInPascalCase='Phone', nameInSnakeCase='PHONE', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=false, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=null, dependentRequired=null, contains=null}")
    }

    fn __serialize_phone<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_phone())
        }
}
impl User {
    fn __name_for_user_status() -> String {
        String::from("CodegenProperty{openApiType='integer', baseName='userStatus', complexType='integer', getter='getUserStatus', setter='setUserStatus', description='User Status', dataType='i32', datatypeWithEnum='i32', dataFormat='int32', name='user_status', min='null', max='null', defaultValue='None', defaultValueWithParam=' = data.userStatus;', baseType='integer', containerType='null', containerTypeMapped='null', title='null', unescapedDescription='User Status', maxLength=null, minLength=null, pattern='null', example='null', jsonSchema='{
  "description" : "User Status",
  "format" : "int32",
  "type" : "integer"
}', minimum='null', maximum='null', exclusiveMinimum=false, exclusiveMaximum=false, required=false, deprecated=false, hasMoreNonReadOnly=false, isPrimitiveType=true, isModel=false, isContainer=false, isString=false, isNumeric=true, isInteger=true, isShort=true, isLong=false, isUnboundedInteger=false, isNumber=false, isFloat=false, isDouble=false, isDecimal=false, isByteArray=false, isBinary=false, isFile=false, isBoolean=false, isDate=false, isDateTime=false, isUuid=false, isUri=false, isEmail=false, isPassword=false, isFreeFormObject=false, isArray=false, isMap=false, isOptional=false, isEnum=false, isInnerEnum=false, isEnumRef=false, isAnyType=false, isReadOnly=false, isWriteOnly=false, isNullable=false, isSelfReference=false, isCircularReference=false, isDiscriminator=false, isNew=false, isOverridden=null, _enum=null, allowableValues=null, items=null, additionalProperties=null, vars=[], requiredVars=[], mostInnerItems=null, vendorExtensions={}, hasValidation=false, isInherited=false, discriminatorValue='null', nameInCamelCase='userStatus', nameInPascalCase='UserStatus', nameInSnakeCase='USER_STATUS', enumName='null', maxItems=null, minItems=null, maxProperties=null, minProperties=null, uniqueItems=false, uniqueItemsBoolean=null, multipleOf=null, isXmlAttribute=false, xmlPrefix='null', xmlName='null', xmlNamespace='null', isXmlWrapped=false, isNull=false, isVoid=false, getAdditionalPropertiesIsAnyType=false, getHasVars=false, getHasRequired=false, getHasDiscriminatorWithNonEmptyMapping=false, composedSchemas=null, hasMultipleTypes=false, hasSanitizedName=true, requiredVarsMap=null, ref=null, schemaIsFromAdditionalProperties=false, isBooleanSchemaTrue=false, isBooleanSchemaFalse=false, format=int32, dependentRequired=null, contains=null}")
    }

    fn __serialize_user_status<S>(_: &String, s: S) -> Result<S::Ok, S::Error>
        where S: serde::Serializer,
        {
            s.serialize_str(&Self::__name_for_user_status())
        }
}


impl User {
    #[allow(clippy::new_without_default, clippy::too_many_arguments)]
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
impl std::fmt::Display for User {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params: Vec<Option<String>> = vec![

            self.id.as_ref().map(|id| {
                [
                    "id".to_string(),
                    id.to_string(),
                ].join(",")
            }),


            self.username.as_ref().map(|username| {
                [
                    "username".to_string(),
                    username.to_string(),
                ].join(",")
            }),


            self.first_name.as_ref().map(|first_name| {
                [
                    "firstName".to_string(),
                    first_name.to_string(),
                ].join(",")
            }),


            self.last_name.as_ref().map(|last_name| {
                [
                    "lastName".to_string(),
                    last_name.to_string(),
                ].join(",")
            }),


            self.email.as_ref().map(|email| {
                [
                    "email".to_string(),
                    email.to_string(),
                ].join(",")
            }),


            self.password.as_ref().map(|password| {
                [
                    "password".to_string(),
                    password.to_string(),
                ].join(",")
            }),


            self.phone.as_ref().map(|phone| {
                [
                    "phone".to_string(),
                    phone.to_string(),
                ].join(",")
            }),


            self.user_status.as_ref().map(|user_status| {
                [
                    "userStatus".to_string(),
                    user_status.to_string(),
                ].join(",")
            }),

        ];

        write!(f, "{}", params.into_iter().flatten().collect::<Vec<_>>().join(","))
    }
}

/// Converts Query Parameters representation (style=form, explode=false) to a User value
/// as specified in https://swagger.io/docs/specification/serialization/
/// Should be implemented in a serde deserializer
impl std::str::FromStr for User {
    type Err = String;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        /// An intermediate representation of the struct to use for parsing.
        #[derive(Default)]
        #[allow(dead_code)]
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
        let mut string_iter = s.split(',');
        let mut key_result = string_iter.next();

        while key_result.is_some() {
            let val = match string_iter.next() {
                Some(x) => x,
                None => return std::result::Result::Err("Missing value while parsing User".to_string())
            };

            if let Some(key) = key_result {
                #[allow(clippy::match_single_binding)]
                match key {
                    #[allow(clippy::redundant_clone)]
                    "id" => intermediate_rep.id.push(<i64 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "username" => intermediate_rep.username.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "firstName" => intermediate_rep.first_name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "lastName" => intermediate_rep.last_name.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "email" => intermediate_rep.email.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "password" => intermediate_rep.password.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "phone" => intermediate_rep.phone.push(<String as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    #[allow(clippy::redundant_clone)]
                    "userStatus" => intermediate_rep.user_status.push(<i32 as std::str::FromStr>::from_str(val).map_err(|x| x.to_string())?),
                    _ => return std::result::Result::Err("Unexpected key while parsing User".to_string())
                }
            }

            // Get the next key
            key_result = string_iter.next();
        }

        // Use the intermediate representation to return the struct
        std::result::Result::Ok(User {
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

// Methods for converting between header::IntoHeaderValue<User> and HeaderValue

#[cfg(feature = "server")]
impl std::convert::TryFrom<header::IntoHeaderValue<User>> for HeaderValue {
    type Error = String;

    fn try_from(hdr_value: header::IntoHeaderValue<User>) -> std::result::Result<Self, Self::Error> {
        let hdr_value = hdr_value.to_string();
        match HeaderValue::from_str(&hdr_value) {
             std::result::Result::Ok(value) => std::result::Result::Ok(value),
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Invalid header value for User - value: {} is invalid {}",
                     hdr_value, e))
        }
    }
}

#[cfg(feature = "server")]
impl std::convert::TryFrom<HeaderValue> for header::IntoHeaderValue<User> {
    type Error = String;

    fn try_from(hdr_value: HeaderValue) -> std::result::Result<Self, Self::Error> {
        match hdr_value.to_str() {
             std::result::Result::Ok(value) => {
                    match <User as std::str::FromStr>::from_str(value) {
                        std::result::Result::Ok(value) => std::result::Result::Ok(header::IntoHeaderValue(value)),
                        std::result::Result::Err(err) => std::result::Result::Err(
                            format!("Unable to convert header value '{}' into User - {}",
                                value, err))
                    }
             },
             std::result::Result::Err(e) => std::result::Result::Err(
                 format!("Unable to convert header: {:?} to string: {}",
                     hdr_value, e))
        }
    }
}



