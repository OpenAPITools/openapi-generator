# OneOfModel


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**inherited_value** | **str** |  | [optional] 
**var_continue** | **str** |  | [optional] 
**renamed** | **str** |  | [optional] 
**read_only_value** | **str** |  | [optional] [readonly] 
**nested** | [**NestedModel**](NestedModel.md) |  | [optional] 
**nested_list** | [**list[NestedModel]**](NestedModel.md) |  | [optional] 
**nested_map** | [**dict[str, NestedModel]**](NestedModel.md) |  | [optional] 
**nested_lists** | **list[list[NestedModel]]** |  | [optional] 
**nested_maps** | **list[dict[str, NestedModel]]** |  | [optional] 
**map_of_lists** | **dict[str, list[NestedModel]]** |  | [optional] 
**map_of_maps** | **dict[str, dict[str, NestedModel]]** |  | [optional] 
**nullable_value** | **str** |  | [optional] 
**default_value** | **str** |  | [optional] [default to 'default']

## Example

```python
from legacy_model_dict_client.models.one_of_model import OneOfModel

# TODO update the JSON string below
json = "{}"
# create an instance of OneOfModel from a JSON string
one_of_model_instance = OneOfModel.from_json(json)
# print the JSON string representation of the object
print(OneOfModel.to_json())

# convert the object into a dict
one_of_model_dict = one_of_model_instance.to_dict()
# create an instance of OneOfModel from a dict
one_of_model_from_dict = OneOfModel.from_dict(one_of_model_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


