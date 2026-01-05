# ListClass


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**var_123_list** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.list_class import ListClass

# TODO update the JSON string below
json = "{}"
# create an instance of ListClass from a JSON string
list_class_instance = ListClass.model_validate_json(json)
# print the JSON string representation of the object
print(ListClass.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
list_class_dict = list_class_instance.model_dump(by_alias=True)
# create an instance of ListClass from a dict
list_class_from_dict = ListClass.model_validate(list_class_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


