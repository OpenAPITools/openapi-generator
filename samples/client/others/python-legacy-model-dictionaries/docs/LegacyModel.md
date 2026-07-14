# LegacyModel


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**inherited_value** | **str** |  | [optional] 
**_continue** | **str** |  | [optional]
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
from legacy_model_dict_client.models.legacy_model import LegacyModel

# TODO update the JSON string below
json = "{}"
# create an instance of LegacyModel from a JSON string
legacy_model_instance = LegacyModel.from_json(json)
# print the JSON string representation of the object
print(LegacyModel.to_json())

# convert the object into a dict
legacy_model_dict = legacy_model_instance.to_dict()
# create an instance of LegacyModel from a dict
legacy_model_from_dict = LegacyModel.from_dict(legacy_model_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


