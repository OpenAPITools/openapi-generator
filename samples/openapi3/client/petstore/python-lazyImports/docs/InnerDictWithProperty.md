# InnerDictWithProperty


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**a_property** | **object** |  | [optional] 

## Example

```python
from petstore_api.models.inner_dict_with_property import InnerDictWithProperty

# TODO update the JSON string below
json = "{}"
# create an instance of InnerDictWithProperty from a JSON string
inner_dict_with_property_instance = InnerDictWithProperty.model_validate_json(json)
# print the JSON string representation of the object
print(InnerDictWithProperty.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
inner_dict_with_property_dict = inner_dict_with_property_instance.model_dump(by_alias=True)
# create an instance of InnerDictWithProperty from a dict
inner_dict_with_property_from_dict = InnerDictWithProperty.model_validate(inner_dict_with_property_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


