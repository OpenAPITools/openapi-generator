# ParentWithOptionalDict


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**optional_dict** | [**Dict[str, InnerDictWithProperty]**](InnerDictWithProperty.md) |  | [optional] 

## Example

```python
from petstore_api.models.parent_with_optional_dict import ParentWithOptionalDict

# TODO update the JSON string below
json = "{}"
# create an instance of ParentWithOptionalDict from a JSON string
parent_with_optional_dict_instance = ParentWithOptionalDict.from_json(json)
# print the JSON string representation of the object
print(ParentWithOptionalDict.to_json())

# convert the object into a dict
parent_with_optional_dict_dict = parent_with_optional_dict_instance.to_dict()
# create an instance of ParentWithOptionalDict from a dict
parent_with_optional_dict_from_dict = ParentWithOptionalDict.from_dict(parent_with_optional_dict_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


