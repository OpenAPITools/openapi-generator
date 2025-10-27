# MixedPropertiesAndAdditionalPropertiesClass


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**uuid** | **str** |  | [optional] 
**date_time** | **datetime** |  | [optional] 
**map** | [**Dict[str, Animal]**](Animal.md) |  | [optional] 

## Example

```python
from petstore_api.models.mixed_properties_and_additional_properties_class import MixedPropertiesAndAdditionalPropertiesClass

# TODO update the JSON string below
json = "{}"
# create an instance of MixedPropertiesAndAdditionalPropertiesClass from a JSON string
mixed_properties_and_additional_properties_class_instance = MixedPropertiesAndAdditionalPropertiesClass.from_json(json)
# print the JSON string representation of the object
print(MixedPropertiesAndAdditionalPropertiesClass.to_json())

# convert the object into a dict
mixed_properties_and_additional_properties_class_dict = mixed_properties_and_additional_properties_class_instance.to_dict()
# create an instance of MixedPropertiesAndAdditionalPropertiesClass from a dict
mixed_properties_and_additional_properties_class_from_dict = MixedPropertiesAndAdditionalPropertiesClass.from_dict(mixed_properties_and_additional_properties_class_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


