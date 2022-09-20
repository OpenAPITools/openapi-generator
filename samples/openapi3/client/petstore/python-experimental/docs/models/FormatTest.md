# petstore_api.model.format_test.FormatTest

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
dict, frozendict.frozendict,  | frozendict.frozendict,  |  | 

### Dictionary Keys
Key | Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | ------------- | -------------
**date** | str, date,  | str,  |  | value must conform to RFC-3339 full-date YYYY-MM-DD
**number** | decimal.Decimal, int, float,  | decimal.Decimal,  |  | 
**password** | str,  | str,  |  | 
**byte** | str,  | str,  |  | 
**integer** | decimal.Decimal, int,  | decimal.Decimal,  |  | [optional] 
**int32** | decimal.Decimal, int,  | decimal.Decimal,  |  | [optional] value must be a 32 bit integer
**int32withValidations** | decimal.Decimal, int,  | decimal.Decimal,  |  | [optional] value must be a 32 bit integer
**int64** | decimal.Decimal, int,  | decimal.Decimal,  |  | [optional] value must be a 64 bit integer
**float** | decimal.Decimal, int, float,  | decimal.Decimal,  | this is a reserved python keyword | [optional] value must be a 32 bit float
**float32** | decimal.Decimal, int, float,  | decimal.Decimal,  |  | [optional] value must be a 32 bit float
**double** | decimal.Decimal, int, float,  | decimal.Decimal,  |  | [optional] value must be a 64 bit float
**float64** | decimal.Decimal, int, float,  | decimal.Decimal,  |  | [optional] value must be a 64 bit float
**[arrayWithUniqueItems](#arrayWithUniqueItems)** | list, tuple,  | tuple,  |  | [optional] 
**string** | str,  | str,  |  | [optional] 
**binary** | bytes, io.FileIO, io.BufferedReader,  | bytes, FileIO,  |  | [optional] 
**dateTime** | str, datetime,  | str,  |  | [optional] value must conform to RFC-3339 date-time
**uuid** | str, uuid.UUID,  | str,  |  | [optional] value must be a uuid
**uuidNoExample** | str, uuid.UUID,  | str,  |  | [optional] value must be a uuid
**pattern_with_digits** | str,  | str,  | A string that is a 10 digit number. Can have leading zeros. | [optional] 
**pattern_with_digits_and_delimiter** | str,  | str,  | A string starting with &#x27;image_&#x27; (case insensitive) and one to three digits following i.e. Image_01. | [optional] 
**noneProp** | None,  | NoneClass,  |  | [optional] 
**any_string_name** | dict, frozendict.frozendict, str, date, datetime, int, float, bool, decimal.Decimal, None, list, tuple, bytes, io.FileIO, io.BufferedReader | frozendict.frozendict, str, BoolClass, decimal.Decimal, NoneClass, tuple, bytes, FileIO | any string name can be used but the value must be the correct type | [optional]

# arrayWithUniqueItems

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
list, tuple,  | tuple,  |  | 

### Tuple Items
Class Name | Input Type | Accessed Type | Description | Notes
------------- | ------------- | ------------- | ------------- | -------------
items | decimal.Decimal, int, float,  | decimal.Decimal,  |  | 

[[Back to Model list]](../../README.md#documentation-for-models) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to README]](../../README.md)

