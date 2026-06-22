# ArrayOfMapModel


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**array_of_map_property** | **List[Dict[str, Tag]]** |  | [optional] 

## Example

```python
from petstore_api.models.array_of_map_model import ArrayOfMapModel

# TODO update the JSON string below
json = "{}"
# create an instance of ArrayOfMapModel from a JSON string
array_of_map_model_instance = ArrayOfMapModel.from_json(json)
# print the JSON string representation of the object
print ArrayOfMapModel.to_json()

# convert the object into a dict
array_of_map_model_dict = array_of_map_model_instance.to_dict()
# create an instance of ArrayOfMapModel from a dict
array_of_map_model_from_dict = ArrayOfMapModel.from_dict(array_of_map_model_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


