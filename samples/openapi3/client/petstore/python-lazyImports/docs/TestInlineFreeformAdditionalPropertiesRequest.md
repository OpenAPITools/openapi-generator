# TestInlineFreeformAdditionalPropertiesRequest


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**some_property** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.test_inline_freeform_additional_properties_request import TestInlineFreeformAdditionalPropertiesRequest

# TODO update the JSON string below
json = "{}"
# create an instance of TestInlineFreeformAdditionalPropertiesRequest from a JSON string
test_inline_freeform_additional_properties_request_instance = TestInlineFreeformAdditionalPropertiesRequest.model_validate_json(json)
# print the JSON string representation of the object
print(TestInlineFreeformAdditionalPropertiesRequest.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
test_inline_freeform_additional_properties_request_dict = test_inline_freeform_additional_properties_request_instance.model_dump(by_alias=True)
# create an instance of TestInlineFreeformAdditionalPropertiesRequest from a dict
test_inline_freeform_additional_properties_request_from_dict = TestInlineFreeformAdditionalPropertiesRequest.model_validate(test_inline_freeform_additional_properties_request_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


