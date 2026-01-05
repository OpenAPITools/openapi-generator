# EnumArrays


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**just_symbol** | **str** |  | [optional] 
**array_enum** | **List[str]** |  | [optional] 

## Example

```python
from petstore_api.models.enum_arrays import EnumArrays

# TODO update the JSON string below
json = "{}"
# create an instance of EnumArrays from a JSON string
enum_arrays_instance = EnumArrays.model_validate_json(json)
# print the JSON string representation of the object
print(EnumArrays.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
enum_arrays_dict = enum_arrays_instance.model_dump(by_alias=True)
# create an instance of EnumArrays from a dict
enum_arrays_from_dict = EnumArrays.model_validate(enum_arrays_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


