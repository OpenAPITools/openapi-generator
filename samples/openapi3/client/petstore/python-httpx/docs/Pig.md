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
pig_instance = Pig.from_json(json)
# print the JSON string representation of the object
print(Pig.to_json())

# convert the object into a dict
pig_dict = pig_instance.to_dict()
# create an instance of Pig from a dict
pig_from_dict = Pig.from_dict(pig_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


