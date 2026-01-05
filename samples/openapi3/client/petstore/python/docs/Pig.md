# Pig


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**class_name** | **str** |  | 
**color** | **str** |  | 
**size** | **int** |  | 

## Example

```python
from petstore_api.models.pig import Pig

# TODO update the JSON string below
json = "{}"
# create an instance of Pig from a JSON string
pig_instance = Pig.model_validate_json(json)
# print the JSON string representation of the object
print(Pig.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
pig_dict = pig_instance.model_dump(by_alias=True)
# create an instance of Pig from a dict
pig_from_dict = Pig.model_validate(pig_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


