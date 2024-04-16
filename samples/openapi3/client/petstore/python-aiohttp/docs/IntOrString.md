# IntOrString


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------

## Example

```python
from petstore_api.models.int_or_string import IntOrString

# TODO update the JSON string below
json = "{}"
# create an instance of IntOrString from a JSON string
int_or_string_instance = IntOrString.from_json(json)
# print the JSON string representation of the object
print(IntOrString.to_json())

# convert the object into a dict
int_or_string_dict = int_or_string_instance.to_dict()
# create an instance of IntOrString from a dict
int_or_string_from_dict = IntOrString.from_dict(int_or_string_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


