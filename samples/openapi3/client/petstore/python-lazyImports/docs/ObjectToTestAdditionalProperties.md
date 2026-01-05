# ObjectToTestAdditionalProperties

Minimal object

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**var_property** | **bool** | Property | [optional] [default to False]

## Example

```python
from petstore_api.models.object_to_test_additional_properties import ObjectToTestAdditionalProperties

# TODO update the JSON string below
json = "{}"
# create an instance of ObjectToTestAdditionalProperties from a JSON string
object_to_test_additional_properties_instance = ObjectToTestAdditionalProperties.model_validate_json(json)
# print the JSON string representation of the object
print(ObjectToTestAdditionalProperties.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
object_to_test_additional_properties_dict = object_to_test_additional_properties_instance.model_dump(by_alias=True)
# create an instance of ObjectToTestAdditionalProperties from a dict
object_to_test_additional_properties_from_dict = ObjectToTestAdditionalProperties.model_validate(object_to_test_additional_properties_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


