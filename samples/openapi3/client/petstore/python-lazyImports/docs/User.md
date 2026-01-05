# User


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **int** |  | [optional] 
**username** | **str** |  | [optional] 
**first_name** | **str** |  | [optional] 
**last_name** | **str** |  | [optional] 
**email** | **str** |  | [optional] 
**password** | **str** |  | [optional] 
**phone** | **str** |  | [optional] 
**user_status** | **int** | User Status | [optional] 

## Example

```python
from petstore_api.models.user import User

# TODO update the JSON string below
json = "{}"
# create an instance of User from a JSON string
user_instance = User.model_validate_json(json)
# print the JSON string representation of the object
print(User.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
user_dict = user_instance.model_dump(by_alias=True)
# create an instance of User from a dict
user_from_dict = User.model_validate(user_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


