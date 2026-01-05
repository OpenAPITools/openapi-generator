# TestErrorResponsesWithModel404Response


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**reason404** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.test_error_responses_with_model404_response import TestErrorResponsesWithModel404Response

# TODO update the JSON string below
json = "{}"
# create an instance of TestErrorResponsesWithModel404Response from a JSON string
test_error_responses_with_model404_response_instance = TestErrorResponsesWithModel404Response.model_validate_json(json)
# print the JSON string representation of the object
print(TestErrorResponsesWithModel404Response.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
test_error_responses_with_model404_response_dict = test_error_responses_with_model404_response_instance.model_dump(by_alias=True)
# create an instance of TestErrorResponsesWithModel404Response from a dict
test_error_responses_with_model404_response_from_dict = TestErrorResponsesWithModel404Response.model_validate(test_error_responses_with_model404_response_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


