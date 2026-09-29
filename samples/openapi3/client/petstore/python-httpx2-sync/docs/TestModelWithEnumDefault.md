# TestModelWithEnumDefault


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**test_enum** | [**TestEnum**](TestEnum.md) |  | 
**test_string** | **str** |  | [optional] 
**test_enum_with_default** | [**TestEnumWithDefault**](TestEnumWithDefault.md) |  | [optional] [default to TestEnumWithDefault.ZWEI]
**test_string_with_default** | **str** |  | [optional] [default to 'ahoy matey']
**test_inline_defined_enum_with_default** | **str** |  | [optional] [default to 'B']

## Example

```python
from petstore_api.models.test_model_with_enum_default import TestModelWithEnumDefault

# TODO update the JSON string below
json = "{}"
# create an instance of TestModelWithEnumDefault from a JSON string
test_model_with_enum_default_instance = TestModelWithEnumDefault.from_json(json)
# print the JSON string representation of the object
print(TestModelWithEnumDefault.to_json())

# convert the object into a dict
test_model_with_enum_default_dict = test_model_with_enum_default_instance.to_dict()
# create an instance of TestModelWithEnumDefault from a dict
test_model_with_enum_default_from_dict = TestModelWithEnumDefault.from_dict(test_model_with_enum_default_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


