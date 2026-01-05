# File

Must be named `File` for test.

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**source_uri** | **str** | Test capitalization | [optional] 

## Example

```python
from petstore_api.models.file import File

# TODO update the JSON string below
json = "{}"
# create an instance of File from a JSON string
file_instance = File.model_validate_json(json)
# print the JSON string representation of the object
print(File.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
file_dict = file_instance.model_dump(by_alias=True)
# create an instance of File from a dict
file_from_dict = File.model_validate(file_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


