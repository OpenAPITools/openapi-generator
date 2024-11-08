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
list_class_instance = ListClass.from_json(json)
# print the JSON string representation of the object
print(ListClass.to_json())

# convert the object into a dict
list_class_dict = list_class_instance.to_dict()
# create an instance of ListClass from a dict
list_class_from_dict = ListClass.from_dict(list_class_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


