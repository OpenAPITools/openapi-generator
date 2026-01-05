# AnyOfColor

Any of RGB array, RGBA array, or hex string.

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------

## Example

```python
from petstore_api.models.any_of_color import AnyOfColor

# TODO update the JSON string below
json = "{}"
# create an instance of AnyOfColor from a JSON string
any_of_color_instance = AnyOfColor.model_validate_json(json)
# print the JSON string representation of the object
print(AnyOfColor.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
any_of_color_dict = any_of_color_instance.model_dump(by_alias=True)
# create an instance of AnyOfColor from a dict
any_of_color_from_dict = AnyOfColor.model_validate(any_of_color_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


