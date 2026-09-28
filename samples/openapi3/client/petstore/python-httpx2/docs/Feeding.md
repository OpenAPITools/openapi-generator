# Feeding


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**task_name** | **str** |  | 
**function_name** | **str** |  | 
**content** | **str** |  | 

## Example

```python
from petstore_api.models.feeding import Feeding

# TODO update the JSON string below
json = "{}"
# create an instance of Feeding from a JSON string
feeding_instance = Feeding.from_json(json)
# print the JSON string representation of the object
print(Feeding.to_json())

# convert the object into a dict
feeding_dict = feeding_instance.to_dict()
# create an instance of Feeding from a dict
feeding_from_dict = Feeding.from_dict(feeding_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


