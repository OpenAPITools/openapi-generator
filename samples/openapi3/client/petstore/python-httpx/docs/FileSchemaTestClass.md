# FileSchemaTestClass


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**file** | [**File**](File.md) |  | [optional] 
**files** | [**List[File]**](File.md) |  | [optional] 

## Example

```python
from petstore_api.models.file_schema_test_class import FileSchemaTestClass

# TODO update the JSON string below
json = "{}"
# create an instance of FileSchemaTestClass from a JSON string
file_schema_test_class_instance = FileSchemaTestClass.model_validate_json(json)
# print the JSON string representation of the object
print(FileSchemaTestClass.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
file_schema_test_class_dict = file_schema_test_class_instance.model_dump(by_alias=True)
# create an instance of FileSchemaTestClass from a dict
file_schema_test_class_from_dict = FileSchemaTestClass.model_validate(file_schema_test_class_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


