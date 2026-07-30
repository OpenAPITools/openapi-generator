# MapTest


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**map_map_of_string** | **dict[str, dict[str, str]]** |  | [optional] 
**map_of_enum_string** | **dict[str, str]** |  | [optional] 
**direct_map** | **dict[str, bool]** |  | [optional] 
**indirect_map** | **dict[str, bool]** |  | [optional] 

## Example

```python
from petstore_api.models.map_test import MapTest

# TODO update the JSON string below
json = "{}"
# create an instance of MapTest from a JSON string
map_test_instance = MapTest.from_json(json)
# print the JSON string representation of the object
print(MapTest.to_json())

# convert the object into a dict
map_test_dict = map_test_instance.to_dict()
# create an instance of MapTest from a dict
map_test_from_dict = MapTest.from_dict(map_test_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


