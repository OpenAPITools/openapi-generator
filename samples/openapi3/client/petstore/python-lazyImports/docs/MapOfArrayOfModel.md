# MapOfArrayOfModel


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**shop_id_to_org_online_lip_map** | **Dict[str, List[Tag]]** |  | [optional] 

## Example

```python
from petstore_api.models.map_of_array_of_model import MapOfArrayOfModel

# TODO update the JSON string below
json = "{}"
# create an instance of MapOfArrayOfModel from a JSON string
map_of_array_of_model_instance = MapOfArrayOfModel.model_validate_json(json)
# print the JSON string representation of the object
print(MapOfArrayOfModel.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
map_of_array_of_model_dict = map_of_array_of_model_instance.model_dump(by_alias=True)
# create an instance of MapOfArrayOfModel from a dict
map_of_array_of_model_from_dict = MapOfArrayOfModel.model_validate(map_of_array_of_model_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


