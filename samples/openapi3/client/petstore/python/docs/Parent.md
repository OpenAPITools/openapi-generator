# Parent


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**optional_dict** | [**Dict[str, InnerDictWithProperty]**](InnerDictWithProperty.md) |  | [optional] 

## Example

```python
from petstore_api.models.parent import Parent

# TODO update the JSON string below
json = "{}"
# create an instance of Parent from a JSON string
parent_instance = Parent.model_validate_json(json)
# print the JSON string representation of the object
print(Parent.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
parent_dict = parent_instance.model_dump(by_alias=True)
# create an instance of Parent from a dict
parent_from_dict = Parent.model_validate(parent_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


