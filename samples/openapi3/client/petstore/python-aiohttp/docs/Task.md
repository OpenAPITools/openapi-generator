# Task

Used to test oneOf enums with only one string value.

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **UUID** |  | 
**activity** | [**TaskActivity**](TaskActivity.md) |  | 

## Example

```python
from petstore_api.models.task import Task

# TODO update the JSON string below
json = "{}"
# create an instance of Task from a JSON string
task_instance = Task.model_validate_json(json)
# print the JSON string representation of the object
print(Task.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
task_dict = task_instance.model_dump(by_alias=True)
# create an instance of Task from a dict
task_from_dict = Task.model_validate(task_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


