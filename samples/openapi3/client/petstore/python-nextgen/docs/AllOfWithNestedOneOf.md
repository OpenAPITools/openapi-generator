# AllOfWithNestedOneOf


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **int** |  | [optional] 
**name** | **str** |  | [optional] 
**inner_name** | **str** |  | [optional] 
**nested_oneof** | [**OneOfBasquePigDanishPig**](OneOfBasquePigDanishPig.md) |  | [optional] 

## Example

```python
from petstore_api.models.all_of_with_nested_one_of import AllOfWithNestedOneOf

# TODO update the JSON string below
json = "{}"
# create an instance of AllOfWithNestedOneOf from a JSON string
all_of_with_nested_one_of_instance = AllOfWithNestedOneOf.from_json(json)
# print the JSON string representation of the object
print AllOfWithNestedOneOf.to_json()

# convert the object into a dict
all_of_with_nested_one_of_dict = all_of_with_nested_one_of_instance.to_dict()
# create an instance of AllOfWithNestedOneOf from a dict
all_of_with_nested_one_of_form_dict = all_of_with_nested_one_of.from_dict(all_of_with_nested_one_of_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


