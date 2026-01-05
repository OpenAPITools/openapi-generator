# AdditionalPropertiesWithDescriptionOnly


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.additional_properties_with_description_only import AdditionalPropertiesWithDescriptionOnly

# TODO update the JSON string below
json = "{}"
# create an instance of AdditionalPropertiesWithDescriptionOnly from a JSON string
additional_properties_with_description_only_instance = AdditionalPropertiesWithDescriptionOnly.model_validate_json(json)
# print the JSON string representation of the object
print(AdditionalPropertiesWithDescriptionOnly.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
additional_properties_with_description_only_dict = additional_properties_with_description_only_instance.model_dump(by_alias=True)
# create an instance of AdditionalPropertiesWithDescriptionOnly from a dict
additional_properties_with_description_only_from_dict = AdditionalPropertiesWithDescriptionOnly.model_validate(additional_properties_with_description_only_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


