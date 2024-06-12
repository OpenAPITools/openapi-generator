# Bathing


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**task_name** | **str** |  | 
**function_name** | **str** |  | 
**content** | **str** |  | 

## Example

```python
from petstore_api.models.bathing import Bathing

# TODO update the JSON string below
json = "{}"
# create an instance of Bathing from a JSON string
bathing_instance = Bathing.from_json(json)
# print the JSON string representation of the object
print(Bathing.to_json())

# convert the object into a dict
bathing_dict = bathing_instance.to_dict()
# create an instance of Bathing from a dict
bathing_from_dict = Bathing.from_dict(bathing_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


