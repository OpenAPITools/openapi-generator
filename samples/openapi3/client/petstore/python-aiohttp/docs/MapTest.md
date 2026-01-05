# MapTest


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**map_map_of_string** | **Dict[str, Dict[str, str]]** |  | [optional] 
**map_of_enum_string** | **Dict[str, str]** |  | [optional] 
**direct_map** | **Dict[str, bool]** |  | [optional] 
**indirect_map** | **Dict[str, bool]** |  | [optional] 

## Example

```python
from petstore_api.models.map_test import MapTest

# TODO update the JSON string below
json = "{}"
# create an instance of MapTest from a JSON string
map_test_instance = MapTest.model_validate_json(json)
# print the JSON string representation of the object
print(MapTest.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
map_test_dict = map_test_instance.model_dump(by_alias=True)
# create an instance of MapTest from a dict
map_test_from_dict = MapTest.model_validate(map_test_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


