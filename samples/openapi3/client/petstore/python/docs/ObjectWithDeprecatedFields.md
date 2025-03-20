# ObjectWithDeprecatedFields


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**uuid** | **str** |  | [optional] 
**id** | **float** |  | [optional] 
**deprecated_ref** | [**DeprecatedObject**](DeprecatedObject.md) |  | [optional] 
**bars** | **List[str]** |  | [optional] 

## Example

```python
from petstore_api.models.object_with_deprecated_fields import ObjectWithDeprecatedFields

# TODO update the JSON string below
json = "{}"
# create an instance of ObjectWithDeprecatedFields from a JSON string
object_with_deprecated_fields_instance = ObjectWithDeprecatedFields.from_json(json)
# print the JSON string representation of the object
print(ObjectWithDeprecatedFields.to_json())

# convert the object into a dict
object_with_deprecated_fields_dict = object_with_deprecated_fields_instance.to_dict()
# create an instance of ObjectWithDeprecatedFields from a dict
object_with_deprecated_fields_from_dict = ObjectWithDeprecatedFields.from_dict(object_with_deprecated_fields_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


