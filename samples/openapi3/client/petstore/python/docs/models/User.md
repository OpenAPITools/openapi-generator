# petstore_api.model.user.User

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
dict, frozendict.frozendict,  | frozendict.frozendict,  |  | 

### Dictionary Keys
Key | Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | ------------- | -------------
**id** | decimal.Decimal, int,  | decimal.Decimal,  |  | [optional] value must be a 64 bit integer
**username** | str,  | str,  |  | [optional] 
**firstName** | str,  | str,  |  | [optional] 
**lastName** | str,  | str,  |  | [optional] 
**email** | str,  | str,  |  | [optional] 
**password** | str,  | str,  |  | [optional] 
**phone** | str,  | str,  |  | [optional] 
**userStatus** | decimal.Decimal, int,  | decimal.Decimal,  | User Status | [optional] value must be a 32 bit integer
**[objectWithNoDeclaredProps](#objectWithNoDeclaredProps)** | dict, frozendict.frozendict,  | frozendict.frozendict,  | test code generation for objects Value must be a map of strings to values. It cannot be the &#x27;null&#x27; value. | [optional] 
**[objectWithNoDeclaredPropsNullable](#objectWithNoDeclaredPropsNullable)** | dict, frozendict.frozendict, None,  | frozendict.frozendict, NoneClass,  | test code generation for nullable objects. Value must be a map of strings to values or the &#x27;null&#x27; value. | [optional] 
**anyTypeProp** | dict, frozendict.frozendict, str, date, datetime, uuid.UUID, int, float, decimal.Decimal, bool, None, list, tuple, bytes, io.FileIO, io.BufferedReader,  | frozendict.frozendict, str, decimal.Decimal, BoolClass, NoneClass, tuple, bytes, FileIO | test code generation for any type Here the &#x27;type&#x27; attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. See https://github.com/OAI/OpenAPI-Specification/issues/1389 | [optional] 
**[anyTypeExceptNullProp](#anyTypeExceptNullProp)** | dict, frozendict.frozendict, str, date, datetime, uuid.UUID, int, float, decimal.Decimal, bool, None, list, tuple, bytes, io.FileIO, io.BufferedReader,  | frozendict.frozendict, str, decimal.Decimal, BoolClass, NoneClass, tuple, bytes, FileIO | any type except &#x27;null&#x27; Here the &#x27;type&#x27; attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. | [optional] 
**anyTypePropNullable** | dict, frozendict.frozendict, str, date, datetime, uuid.UUID, int, float, decimal.Decimal, bool, None, list, tuple, bytes, io.FileIO, io.BufferedReader,  | frozendict.frozendict, str, decimal.Decimal, BoolClass, NoneClass, tuple, bytes, FileIO | test code generation for any type Here the &#x27;type&#x27; attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. The &#x27;nullable&#x27; attribute does not change the allowed values. | [optional] 
**any_string_name** | dict, frozendict.frozendict, str, date, datetime, int, float, bool, decimal.Decimal, None, list, tuple, bytes, io.FileIO, io.BufferedReader | frozendict.frozendict, str, BoolClass, decimal.Decimal, NoneClass, tuple, bytes, FileIO | any string name can be used but the value must be the correct type | [optional]

# objectWithNoDeclaredProps

test code generation for objects Value must be a map of strings to values. It cannot be the 'null' value.

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
dict, frozendict.frozendict,  | frozendict.frozendict,  | test code generation for objects Value must be a map of strings to values. It cannot be the &#x27;null&#x27; value. | 

# objectWithNoDeclaredPropsNullable

test code generation for nullable objects. Value must be a map of strings to values or the 'null' value.

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
dict, frozendict.frozendict, None,  | frozendict.frozendict, NoneClass,  | test code generation for nullable objects. Value must be a map of strings to values or the &#x27;null&#x27; value. | 

# anyTypeExceptNullProp

any type except 'null' Here the 'type' attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object.

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
dict, frozendict.frozendict, str, date, datetime, uuid.UUID, int, float, decimal.Decimal, bool, None, list, tuple, bytes, io.FileIO, io.BufferedReader,  | frozendict.frozendict, str, decimal.Decimal, BoolClass, NoneClass, tuple, bytes, FileIO | any type except &#x27;null&#x27; Here the &#x27;type&#x27; attribute is not specified, which means the value can be anything, including the null value, string, number, boolean, array or object. | 

### Composed Schemas (allOf/anyOf/oneOf/not)
#### not
Class Name | Input Type | Accessed Type | Description | Notes
------------- | ------------- | ------------- | ------------- | -------------
[not_schema](#not_schema) | None,  | NoneClass,  |  | 

# not_schema

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
None,  | NoneClass,  |  | 

[[Back to Model list]](../../README.md#documentation-for-models) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to README]](../../README.md)

