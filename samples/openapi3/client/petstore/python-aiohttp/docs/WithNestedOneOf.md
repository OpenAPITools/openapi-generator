# WithNestedOneOf


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**size** | **int** |  | [optional] 
**nested_pig** | [**Pig**](Pig.md) |  | [optional] 
**nested_oneof_enum_string** | [**OneOfEnumString**](OneOfEnumString.md) |  | [optional] 

## Example

```python
from petstore_api.models.with_nested_one_of import WithNestedOneOf

# TODO update the JSON string below
json = "{}"
# create an instance of WithNestedOneOf from a JSON string
with_nested_one_of_instance = WithNestedOneOf.model_validate_json(json)
# print the JSON string representation of the object
print(WithNestedOneOf.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
with_nested_one_of_dict = with_nested_one_of_instance.model_dump(by_alias=True)
# create an instance of WithNestedOneOf from a dict
with_nested_one_of_from_dict = WithNestedOneOf.model_validate(with_nested_one_of_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


