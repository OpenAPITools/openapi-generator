# petstore_api.model.format_test.FormatTest

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
dict, frozendict.frozendict,  | frozendict.frozendict,  |  | 

### Dictionary Keys
Key | Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | ------------- | -------------
**date** | str, date,  | str,  |  | 
**number** | decimal.Decimal, int, float,  | decimal.Decimal,  |  | 
**password** | str,  | str,  |  | 
**byte** | str,  | str,  |  | 
**integer** | decimal.Decimal, int,  | decimal.Decimal,  |  | [optional] 
**int32** | decimal.Decimal, int,  | decimal.Decimal,  |  | [optional] 
**int32withValidations** | decimal.Decimal, int,  | decimal.Decimal,  |  | [optional] 
**int64** | decimal.Decimal, int,  | decimal.Decimal,  |  | [optional] 
**float** | decimal.Decimal, int, float,  | decimal.Decimal,  | this is a reserved python keyword | [optional] 
**float32** | decimal.Decimal, int, float,  | decimal.Decimal,  |  | [optional] 
**double** | decimal.Decimal, int, float,  | decimal.Decimal,  |  | [optional] 
**float64** | decimal.Decimal, int, float,  | decimal.Decimal,  |  | [optional] 
**arrayWithUniqueItems** | list, tuple,  | tuple,  |  | [optional] 
**string** | str,  | str,  |  | [optional] 
**binary** | bytes, io.FileIO, io.BufferedReader,  | bytes, FileIO,  |  | [optional] 
**dateTime** | str, datetime,  | str,  |  | [optional] 
**uuid** | str, uuid.UUID,  | str,  |  | [optional] 
**uuidNoExample** | str, uuid.UUID,  | str,  |  | [optional] 
**pattern_with_digits** | str,  | str,  | A string that is a 10 digit number. Can have leading zeros. | [optional] 
**pattern_with_digits_and_delimiter** | str,  | str,  | A string starting with &#x27;image_&#x27; (case insensitive) and one to three digits following i.e. Image_01. | [optional] 
**noneProp** | None,  | NoneClass,  |  | [optional] 
**any string name** | dict, frozendict.frozendict, str, date, datetime, int, float, bool, decimal.Decimal, None, list, tuple, bytes, io.FileIO, io.BufferedReader | frozendict.frozendict, str, BoolClass, decimal.Decimal, NoneClass, tuple, bytes, FileIO | any string name can be used but the value must be the correct type | [optional]

# arrayWithUniqueItems

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
list, tuple,  | tuple,  |  | 

### Tuple Items
Input Type | Accessed Type | Description | Notes
------------- | ------------- | ------------- | -------------
decimal.Decimal, int, float,  | decimal.Decimal,  |  | 

[[Back to Model list]](../../README.md#documentation-for-models) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to README]](../../README.md)

