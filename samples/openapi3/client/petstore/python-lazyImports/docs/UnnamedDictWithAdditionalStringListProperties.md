# UnnamedDictWithAdditionalStringListProperties


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**dict_property** | **Dict[str, List[str]]** |  | [optional] 

## Example

```python
from petstore_api.models.unnamed_dict_with_additional_string_list_properties import UnnamedDictWithAdditionalStringListProperties

# TODO update the JSON string below
json = "{}"
# create an instance of UnnamedDictWithAdditionalStringListProperties from a JSON string
unnamed_dict_with_additional_string_list_properties_instance = UnnamedDictWithAdditionalStringListProperties.model_validate_json(json)
# print the JSON string representation of the object
print(UnnamedDictWithAdditionalStringListProperties.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
unnamed_dict_with_additional_string_list_properties_dict = unnamed_dict_with_additional_string_list_properties_instance.model_dump(by_alias=True)
# create an instance of UnnamedDictWithAdditionalStringListProperties from a dict
unnamed_dict_with_additional_string_list_properties_from_dict = UnnamedDictWithAdditionalStringListProperties.model_validate(unnamed_dict_with_additional_string_list_properties_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


