# Name

Model for testing model name same as property name

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **int** |  | 
**snake_case** | **int** |  | [optional] [readonly] 
**var_property** | **str** |  | [optional] 
**var_123_number** | **int** |  | [optional] [readonly] 

## Example

```python
from petstore_api.models.name import Name

# TODO update the JSON string below
json = "{}"
# create an instance of Name from a JSON string
name_instance = Name.from_json(json)
# print the JSON string representation of the object
print(Name.to_json())

# convert the object into a dict
name_dict = name_instance.to_dict()
# create an instance of Name from a dict
name_from_dict = Name.from_dict(name_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


