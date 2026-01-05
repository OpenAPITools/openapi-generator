# HealthCheckResult

Just a string to inform instance is up and running. Make it nullable in hope to get it as pointer in generated model.

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**nullable_message** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.health_check_result import HealthCheckResult

# TODO update the JSON string below
json = "{}"
# create an instance of HealthCheckResult from a JSON string
health_check_result_instance = HealthCheckResult.model_validate_json(json)
# print the JSON string representation of the object
print(HealthCheckResult.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
health_check_result_dict = health_check_result_instance.model_dump(by_alias=True)
# create an instance of HealthCheckResult from a dict
health_check_result_from_dict = HealthCheckResult.model_validate(health_check_result_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


