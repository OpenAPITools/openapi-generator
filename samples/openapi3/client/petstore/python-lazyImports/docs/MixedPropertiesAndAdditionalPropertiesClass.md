# MixedPropertiesAndAdditionalPropertiesClass


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**uuid** | **UUID** |  | [optional] 
**date_time** | **datetime** |  | [optional] 
**map** | [**Dict[str, Animal]**](Animal.md) |  | [optional] 

## Example

```python
from petstore_api.models.mixed_properties_and_additional_properties_class import MixedPropertiesAndAdditionalPropertiesClass

# TODO update the JSON string below
json = "{}"
# create an instance of MixedPropertiesAndAdditionalPropertiesClass from a JSON string
mixed_properties_and_additional_properties_class_instance = MixedPropertiesAndAdditionalPropertiesClass.model_validate_json(json)
# print the JSON string representation of the object
print(MixedPropertiesAndAdditionalPropertiesClass.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
mixed_properties_and_additional_properties_class_dict = mixed_properties_and_additional_properties_class_instance.model_dump(by_alias=True)
# create an instance of MixedPropertiesAndAdditionalPropertiesClass from a dict
mixed_properties_and_additional_properties_class_from_dict = MixedPropertiesAndAdditionalPropertiesClass.model_validate(mixed_properties_and_additional_properties_class_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


