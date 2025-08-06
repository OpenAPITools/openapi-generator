# ModelWithArrayAndMapDefaultsMap


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**key1** | **str** |  | [optional] [default to 'value1']
**key2** | **str** |  | [optional] [default to 'value2']

## Example

```python
from petstore_api.models.model_with_array_and_map_defaults_map import ModelWithArrayAndMapDefaultsMap

# TODO update the JSON string below
json = "{}"
# create an instance of ModelWithArrayAndMapDefaultsMap from a JSON string
model_with_array_and_map_defaults_map_instance = ModelWithArrayAndMapDefaultsMap.from_json(json)
# print the JSON string representation of the object
print(ModelWithArrayAndMapDefaultsMap.to_json())

# convert the object into a dict
model_with_array_and_map_defaults_map_dict = model_with_array_and_map_defaults_map_instance.to_dict()
# create an instance of ModelWithArrayAndMapDefaultsMap from a dict
model_with_array_and_map_defaults_map_from_dict = ModelWithArrayAndMapDefaultsMap.from_dict(model_with_array_and_map_defaults_map_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


