# petstore_api.model.pet.Pet

Pet object that needs to be added to the store

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
dict, frozendict.frozendict,  | frozendict.frozendict,  | Pet object that needs to be added to the store | 

### Dictionary Keys
Key | Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | ------------- | -------------
**photoUrls** | list, tuple,  | tuple,  |  | 
**name** | str,  | str,  |  | 
**id** | decimal.Decimal, int,  | decimal.Decimal,  |  | [optional] 
**category** | [**Category**](Category.md) | [**Category**](Category.md) |  | [optional] 
**tags** | list, tuple,  | tuple,  |  | [optional] 
**status** | str,  | str,  | pet status in the store | [optional] must be one of ["available", "pending", "sold", ] 
**any string name** | dict, frozendict.frozendict, str, date, datetime, int, float, bool, decimal.Decimal, None, list, tuple, bytes, io.FileIO, io.BufferedReader | frozendict.frozendict, str, BoolClass, decimal.Decimal, NoneClass, tuple, bytes, FileIO | any string name can be used but the value must be the correct type | [optional]

# photoUrls

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
list, tuple,  | tuple,  |  | 

### Tuple Items
Input Type | Accessed Type | Description | Notes
------------- | ------------- | ------------- | -------------
str,  | str,  |  | 

# tags

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
list, tuple,  | tuple,  |  | 

### Tuple Items
Input Type | Accessed Type | Description | Notes
------------- | ------------- | ------------- | -------------
[**Tag**](Tag.md) | [**Tag**](Tag.md) |  | 

[[Back to Model list]](../../README.md#documentation-for-models) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to README]](../../README.md)

