# Color

RGB array, RGBA array, or hex string.

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------

## Example

```python
from petstore_api.models.color import Color

# TODO update the JSON string below
json = "{}"
# create an instance of Color from a JSON string
color_instance = Color.model_validate_json(json)
# print the JSON string representation of the object
print(Color.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
color_dict = color_instance.model_dump(by_alias=True)
# create an instance of Color from a dict
color_from_dict = Color.model_validate(color_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


