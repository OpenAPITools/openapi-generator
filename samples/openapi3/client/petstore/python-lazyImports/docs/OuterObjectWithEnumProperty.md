# OuterObjectWithEnumProperty


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**str_value** | [**OuterEnum**](OuterEnum.md) |  | [optional] 
**value** | [**OuterEnumInteger**](OuterEnumInteger.md) |  | 

## Example

```python
from petstore_api.models.outer_object_with_enum_property import OuterObjectWithEnumProperty

# TODO update the JSON string below
json = "{}"
# create an instance of OuterObjectWithEnumProperty from a JSON string
outer_object_with_enum_property_instance = OuterObjectWithEnumProperty.model_validate_json(json)
# print the JSON string representation of the object
print(OuterObjectWithEnumProperty.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
outer_object_with_enum_property_dict = outer_object_with_enum_property_instance.model_dump(by_alias=True)
# create an instance of OuterObjectWithEnumProperty from a dict
outer_object_with_enum_property_from_dict = OuterObjectWithEnumProperty.model_validate(outer_object_with_enum_property_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


