# List


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**var_123_list** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.list import List

# TODO update the JSON string below
json = "{}"
# create an instance of List from a JSON string
list_instance = List.from_json(json)
# print the JSON string representation of the object
print List.to_json()

# convert the object into a dict
list_dict = list_instance.to_dict()
# create an instance of List from a dict
list_form_dict = list.from_dict(list_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


