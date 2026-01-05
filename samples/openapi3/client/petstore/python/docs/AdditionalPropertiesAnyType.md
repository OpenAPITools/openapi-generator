# AdditionalPropertiesAnyType


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.additional_properties_any_type import AdditionalPropertiesAnyType

# TODO update the JSON string below
json = "{}"
# create an instance of AdditionalPropertiesAnyType from a JSON string
additional_properties_any_type_instance = AdditionalPropertiesAnyType.model_validate_json(json)
# print the JSON string representation of the object
print(AdditionalPropertiesAnyType.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
additional_properties_any_type_dict = additional_properties_any_type_instance.model_dump(by_alias=True)
# create an instance of AdditionalPropertiesAnyType from a dict
additional_properties_any_type_from_dict = AdditionalPropertiesAnyType.model_validate(additional_properties_any_type_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


