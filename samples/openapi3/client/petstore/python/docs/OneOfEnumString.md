# OneOfEnumString

oneOf enum strings

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------

## Example

```python
from petstore_api.models.one_of_enum_string import OneOfEnumString

# TODO update the JSON string below
json = "{}"
# create an instance of OneOfEnumString from a JSON string
one_of_enum_string_instance = OneOfEnumString.model_validate_json(json)
# print the JSON string representation of the object
print(OneOfEnumString.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
one_of_enum_string_dict = one_of_enum_string_instance.model_dump(by_alias=True)
# create an instance of OneOfEnumString from a dict
one_of_enum_string_from_dict = OneOfEnumString.model_validate(one_of_enum_string_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


