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
inner_dict_with_property_instance = InnerDictWithProperty.from_json(json)
# print the JSON string representation of the object
print(InnerDictWithProperty.to_json())

# convert the object into a dict
inner_dict_with_property_dict = inner_dict_with_property_instance.to_dict()
# create an instance of InnerDictWithProperty from a dict
inner_dict_with_property_from_dict = InnerDictWithProperty.from_dict(inner_dict_with_property_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


