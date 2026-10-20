# NestedModel


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**camel_case** | **str** |  | [optional] 
**nested_read_only_value** | **str** |  | [optional] [readonly] 

## Example

```python
from legacy_model_dict_client.models.nested_model import NestedModel

# TODO update the JSON string below
json = "{}"
# create an instance of NestedModel from a JSON string
nested_model_instance = NestedModel.from_json(json)
# print the JSON string representation of the object
print(NestedModel.to_json())

# convert the object into a dict
nested_model_dict = nested_model_instance.to_dict()
# create an instance of NestedModel from a dict
nested_model_from_dict = NestedModel.from_dict(nested_model_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


