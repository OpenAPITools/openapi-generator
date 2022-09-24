# petstore_api.model.object_model_with_ref_props.ObjectModelWithRefProps

a model that includes properties which should stay primitive (String + Boolean) and one which is defined as a class, NumberWithValidations

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
dict, frozendict.frozendict,  | frozendict.frozendict,  | a model that includes properties which should stay primitive (String + Boolean) and one which is defined as a class, NumberWithValidations | 

### Dictionary Keys
Key | Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | ------------- | -------------
**myNumber** | [**NumberWithValidations**](NumberWithValidations.md) | [**NumberWithValidations**](NumberWithValidations.md) |  | [optional] 
**myString** | str,  | str,  |  | [optional] 
**myBoolean** | bool,  | BoolClass,  |  | [optional] 
**any_string_name** | dict, frozendict.frozendict, str, date, datetime, int, float, bool, decimal.Decimal, None, list, tuple, bytes, io.FileIO, io.BufferedReader | frozendict.frozendict, str, BoolClass, decimal.Decimal, NoneClass, tuple, bytes, FileIO | any string name can be used but the value must be the correct type | [optional]

[[Back to Model list]](../../README.md#documentation-for-models) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to README]](../../README.md)

