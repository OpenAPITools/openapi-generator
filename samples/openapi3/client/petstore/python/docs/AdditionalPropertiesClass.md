# AdditionalPropertiesClass


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**map_property** | **Dict[str, str]** |  | [optional] 
**map_of_map_property** | **Dict[str, Dict[str, str]]** |  | [optional] 

## Example

```python
from petstore_api.models.additional_properties_class import AdditionalPropertiesClass

# TODO update the JSON string below
json = "{}"
# create an instance of AdditionalPropertiesClass from a JSON string
additional_properties_class_instance = AdditionalPropertiesClass.from_json(json)
# print the JSON string representation of the object
print(AdditionalPropertiesClass.to_json())

# convert the object into a dict
additional_properties_class_dict = additional_properties_class_instance.to_dict()
# create an instance of AdditionalPropertiesClass from a dict
additional_properties_class_from_dict = AdditionalPropertiesClass.from_dict(additional_properties_class_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


