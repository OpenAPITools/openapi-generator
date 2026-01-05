# DanishPig


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**class_name** | **str** |  | 
**size** | **int** |  | 

## Example

```python
from petstore_api.models.danish_pig import DanishPig

# TODO update the JSON string below
json = "{}"
# create an instance of DanishPig from a JSON string
danish_pig_instance = DanishPig.model_validate_json(json)
# print the JSON string representation of the object
print(DanishPig.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
danish_pig_dict = danish_pig_instance.model_dump(by_alias=True)
# create an instance of DanishPig from a dict
danish_pig_from_dict = DanishPig.model_validate(danish_pig_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


