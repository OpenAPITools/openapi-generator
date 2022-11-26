# unit_test_api.model.properties_with_escaped_characters.PropertiesWithEscapedCharacters

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
dict, frozendict.frozendict, str, date, datetime, uuid.UUID, int, float, decimal.Decimal, bool, None, list, tuple, bytes, io.FileIO, io.BufferedReader,  | frozendict.frozendict, str, decimal.Decimal, BoolClass, NoneClass, tuple, bytes, FileIO |  | 

### Dictionary Keys
Key | Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | ------------- | -------------
**foo\nbar** | decimal.Decimal, int, float,  | decimal.Decimal,  |  | [optional] 
**foo\&quot;bar** | decimal.Decimal, int, float,  | decimal.Decimal,  |  | [optional] 
**foo\\bar** | decimal.Decimal, int, float,  | decimal.Decimal,  |  | [optional] 
**foo\rbar** | decimal.Decimal, int, float,  | decimal.Decimal,  |  | [optional] 
**foo\tbar** | decimal.Decimal, int, float,  | decimal.Decimal,  |  | [optional] 
**foo\fbar** | decimal.Decimal, int, float,  | decimal.Decimal,  |  | [optional] 
**any_string_name** | dict, frozendict.frozendict, str, date, datetime, int, float, bool, decimal.Decimal, None, list, tuple, bytes, io.FileIO, io.BufferedReader | frozendict.frozendict, str, BoolClass, decimal.Decimal, NoneClass, tuple, bytes, FileIO | any string name can be used but the value must be the correct type | [optional]

[[Back to Model list]](../../README.md#documentation-for-models) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to README]](../../README.md)

