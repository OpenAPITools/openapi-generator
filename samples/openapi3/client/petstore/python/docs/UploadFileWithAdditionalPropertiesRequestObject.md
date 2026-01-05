# UploadFileWithAdditionalPropertiesRequestObject

Additional object

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.upload_file_with_additional_properties_request_object import UploadFileWithAdditionalPropertiesRequestObject

# TODO update the JSON string below
json = "{}"
# create an instance of UploadFileWithAdditionalPropertiesRequestObject from a JSON string
upload_file_with_additional_properties_request_object_instance = UploadFileWithAdditionalPropertiesRequestObject.model_validate_json(json)
# print the JSON string representation of the object
print(UploadFileWithAdditionalPropertiesRequestObject.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
upload_file_with_additional_properties_request_object_dict = upload_file_with_additional_properties_request_object_instance.model_dump(by_alias=True)
# create an instance of UploadFileWithAdditionalPropertiesRequestObject from a dict
upload_file_with_additional_properties_request_object_from_dict = UploadFileWithAdditionalPropertiesRequestObject.model_validate(upload_file_with_additional_properties_request_object_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


