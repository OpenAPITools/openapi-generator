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
name_instance = Name.model_validate_json(json)
# print the JSON string representation of the object
print(Name.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
name_dict = name_instance.model_dump(by_alias=True)
# create an instance of Name from a dict
name_from_dict = Name.model_validate(name_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


