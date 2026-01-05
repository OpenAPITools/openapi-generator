# PrimitiveString


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**value** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.primitive_string import PrimitiveString

# TODO update the JSON string below
json = "{}"
# create an instance of PrimitiveString from a JSON string
primitive_string_instance = PrimitiveString.model_validate_json(json)
# print the JSON string representation of the object
print(PrimitiveString.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
primitive_string_dict = primitive_string_instance.model_dump(by_alias=True)
# create an instance of PrimitiveString from a dict
primitive_string_from_dict = PrimitiveString.model_validate(primitive_string_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


