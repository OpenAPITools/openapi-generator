# AdditionalPropertiesObject


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.additional_properties_object import AdditionalPropertiesObject

# TODO update the JSON string below
json = "{}"
# create an instance of AdditionalPropertiesObject from a JSON string
additional_properties_object_instance = AdditionalPropertiesObject.model_validate_json(json)
# print the JSON string representation of the object
print(AdditionalPropertiesObject.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
additional_properties_object_dict = additional_properties_object_instance.model_dump(by_alias=True)
# create an instance of AdditionalPropertiesObject from a dict
additional_properties_object_from_dict = AdditionalPropertiesObject.model_validate(additional_properties_object_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


