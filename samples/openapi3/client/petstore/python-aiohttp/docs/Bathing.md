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
bathing_instance = Bathing.model_validate_json(json)
# print the JSON string representation of the object
print(Bathing.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
bathing_dict = bathing_instance.model_dump(by_alias=True)
# create an instance of Bathing from a dict
bathing_from_dict = Bathing.model_validate(bathing_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


