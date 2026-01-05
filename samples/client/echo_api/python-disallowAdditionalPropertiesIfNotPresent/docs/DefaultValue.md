# DefaultValue

to test the default value of properties

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**array_string_enum_ref_default** | [**List[StringEnumRef]**](StringEnumRef.md) |  | [optional] [default to ["success","failure"]]
**array_string_enum_default** | **List[str]** |  | [optional] [default to ["success","failure"]]
**array_string_default** | **List[str]** |  | [optional] [default to ["failure","skipped"]]
**array_integer_default** | **List[int]** |  | [optional] [default to [1,3]]
**array_string** | **List[str]** |  | [optional] 
**array_string_nullable** | **List[str]** |  | [optional] 
**array_string_extension_nullable** | **List[str]** |  | [optional] 
**string_nullable** | **str** |  | [optional] 

## Example

```python
from openapi_client.models.default_value import DefaultValue

# TODO update the JSON string below
json = "{}"
# create an instance of DefaultValue from a JSON string
default_value_instance = DefaultValue.model_validate_json(json)
# print the JSON string representation of the object
print(DefaultValue.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
default_value_dict = default_value_instance.model_dump(by_alias=True)
# create an instance of DefaultValue from a dict
default_value_from_dict = DefaultValue.model_validate(default_value_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


