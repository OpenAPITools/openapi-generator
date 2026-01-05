# TestObjectForMultipartRequestsRequestMarker


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.test_object_for_multipart_requests_request_marker import TestObjectForMultipartRequestsRequestMarker

# TODO update the JSON string below
json = "{}"
# create an instance of TestObjectForMultipartRequestsRequestMarker from a JSON string
test_object_for_multipart_requests_request_marker_instance = TestObjectForMultipartRequestsRequestMarker.model_validate_json(json)
# print the JSON string representation of the object
print(TestObjectForMultipartRequestsRequestMarker.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
test_object_for_multipart_requests_request_marker_dict = test_object_for_multipart_requests_request_marker_instance.model_dump(by_alias=True)
# create an instance of TestObjectForMultipartRequestsRequestMarker from a dict
test_object_for_multipart_requests_request_marker_from_dict = TestObjectForMultipartRequestsRequestMarker.model_validate(test_object_for_multipart_requests_request_marker_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


