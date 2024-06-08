# TestModel


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
from petstore_api.models.test_model import TestModel

# TODO update the JSON string below
json = "{}"
# create an instance of TestModel from a JSON string
test_model_instance = TestModel.from_json(json)
# print the JSON string representation of the object
print(TestModel.to_json())

# convert the object into a dict
test_model_dict = test_model_instance.to_dict()
# create an instance of TestModel from a dict
test_model_from_dict = TestModel.from_dict(test_model_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


