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
feeding_instance = Feeding.model_validate_json(json)
# print the JSON string representation of the object
print(Feeding.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
feeding_dict = feeding_instance.model_dump(by_alias=True)
# create an instance of Feeding from a dict
feeding_from_dict = Feeding.model_validate(feeding_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


