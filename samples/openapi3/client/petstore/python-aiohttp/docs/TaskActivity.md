# TaskActivity


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**task_name** | **str** |  | 
**function_name** | **str** |  | 
**content** | **str** |  | 

## Example

```python
from petstore_api.models.task_activity import TaskActivity

# TODO update the JSON string below
json = "{}"
# create an instance of TaskActivity from a JSON string
task_activity_instance = TaskActivity.model_validate_json(json)
# print the JSON string representation of the object
print(TaskActivity.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
task_activity_dict = task_activity_instance.model_dump(by_alias=True)
# create an instance of TaskActivity from a dict
task_activity_from_dict = TaskActivity.model_validate(task_activity_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


