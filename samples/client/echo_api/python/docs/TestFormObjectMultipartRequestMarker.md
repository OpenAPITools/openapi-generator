# TestFormObjectMultipartRequestMarker


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **str** |  | [optional] 

## Example

```python
from openapi_client.models.test_form_object_multipart_request_marker import TestFormObjectMultipartRequestMarker

# TODO update the JSON string below
json = "{}"
# create an instance of TestFormObjectMultipartRequestMarker from a JSON string
test_form_object_multipart_request_marker_instance = TestFormObjectMultipartRequestMarker.model_validate_json(json)
# print the JSON string representation of the object
print(TestFormObjectMultipartRequestMarker.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
test_form_object_multipart_request_marker_dict = test_form_object_multipart_request_marker_instance.model_dump(by_alias=True)
# create an instance of TestFormObjectMultipartRequestMarker from a dict
test_form_object_multipart_request_marker_from_dict = TestFormObjectMultipartRequestMarker.model_validate(test_form_object_multipart_request_marker_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


