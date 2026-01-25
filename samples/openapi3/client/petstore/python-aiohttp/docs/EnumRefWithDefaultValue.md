# EnumRefWithDefaultValue


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**report_format** | [**DataOutputFormat**](DataOutputFormat.md) |  | [optional] [default to DataOutputFormat.JSON]

## Example

```python
from petstore_api.models.enum_ref_with_default_value import EnumRefWithDefaultValue

# TODO update the JSON string below
json = "{}"
# create an instance of EnumRefWithDefaultValue from a JSON string
enum_ref_with_default_value_instance = EnumRefWithDefaultValue.from_json(json)
# print the JSON string representation of the object
print(EnumRefWithDefaultValue.to_json())

# convert the object into a dict
enum_ref_with_default_value_dict = enum_ref_with_default_value_instance.to_dict()
# create an instance of EnumRefWithDefaultValue from a dict
enum_ref_with_default_value_from_dict = EnumRefWithDefaultValue.from_dict(enum_ref_with_default_value_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


