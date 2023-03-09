# EnumTest


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**enum_string** | **str** |  | [optional] 
**enum_string_required** | **str** |  | 
**enum_integer_default** | **int** |  | [optional] [default to 5]
**enum_integer** | **int** |  | [optional] 
**enum_number** | **float** |  | [optional] 
**outer_enum** | [**OuterEnum**](OuterEnum.md) |  | [optional] 
**outer_enum_integer** | [**OuterEnumInteger**](OuterEnumInteger.md) |  | [optional] 
**outer_enum_default_value** | [**OuterEnumDefaultValue**](OuterEnumDefaultValue.md) |  | [optional] 
**outer_enum_integer_default_value** | [**OuterEnumIntegerDefaultValue**](OuterEnumIntegerDefaultValue.md) |  | [optional] 

## Example

```python
from petstore_api.models.enum_test import EnumTest

# TODO update the JSON string below
json = "{}"
# create an instance of EnumTest from a JSON string
enum_test_instance = EnumTest.from_json(json)
# print the JSON string representation of the object
print EnumTest.to_json()

# convert the object into a dict
enum_test_dict = enum_test_instance.to_dict()
# create an instance of EnumTest from a dict
enum_test_form_dict = enum_test.from_dict(enum_test_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


