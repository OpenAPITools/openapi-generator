# AnyOfModel


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**inherited_value** | **str** |  | [optional] 
**var_continue** | **str** |  | [optional] 
**renamed** | **str** |  | [optional] 
**read_only_value** | **str** |  | [optional] [readonly] 
**nested** | [**NestedModel**](NestedModel.md) |  | [optional] 
**nested_list** | [**List[NestedModel]**](NestedModel.md) |  | [optional] 
**nested_map** | [**Dict[str, NestedModel]**](NestedModel.md) |  | [optional] 
**nested_lists** | **List[List[NestedModel]]** |  | [optional] 
**nested_maps** | **List[Dict[str, NestedModel]]** |  | [optional] 
**map_of_lists** | **Dict[str, List[NestedModel]]** |  | [optional] 
**map_of_maps** | **Dict[str, Dict[str, NestedModel]]** |  | [optional] 
**nullable_value** | **str** |  | [optional] 
**default_value** | **str** |  | [optional] [default to 'default']

## Example

```python
from legacy_model_dict_client.models.any_of_model import AnyOfModel

# TODO update the JSON string below
json = "{}"
# create an instance of AnyOfModel from a JSON string
any_of_model_instance = AnyOfModel.from_json(json)
# print the JSON string representation of the object
print(AnyOfModel.to_json())

# convert the object into a dict
any_of_model_dict = any_of_model_instance.to_dict()
# create an instance of AnyOfModel from a dict
any_of_model_from_dict = AnyOfModel.from_dict(any_of_model_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


