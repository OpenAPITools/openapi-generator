# ModelWithArrayAndMapDefaults


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**array** | **List[float]** |  | [optional] [default to [1,2]]
**map** | **Dict[str, str]** |  | [optional] 

## Example

```python
from petstore_api.models.model_with_array_and_map_defaults import ModelWithArrayAndMapDefaults

# TODO update the JSON string below
json = "{}"
# create an instance of ModelWithArrayAndMapDefaults from a JSON string
model_with_array_and_map_defaults_instance = ModelWithArrayAndMapDefaults.from_json(json)
# print the JSON string representation of the object
print(ModelWithArrayAndMapDefaults.to_json())

# convert the object into a dict
model_with_array_and_map_defaults_dict = model_with_array_and_map_defaults_instance.to_dict()
# create an instance of ModelWithArrayAndMapDefaults from a dict
model_with_array_and_map_defaults_from_dict = ModelWithArrayAndMapDefaults.from_dict(model_with_array_and_map_defaults_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


