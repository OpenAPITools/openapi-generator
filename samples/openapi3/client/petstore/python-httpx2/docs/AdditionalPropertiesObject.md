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
additional_properties_object_instance = AdditionalPropertiesObject.from_json(json)
# print the JSON string representation of the object
print(AdditionalPropertiesObject.to_json())

# convert the object into a dict
additional_properties_object_dict = additional_properties_object_instance.to_dict()
# create an instance of AdditionalPropertiesObject from a dict
additional_properties_object_from_dict = AdditionalPropertiesObject.from_dict(additional_properties_object_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


