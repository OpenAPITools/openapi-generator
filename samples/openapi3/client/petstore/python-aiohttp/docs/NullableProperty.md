# NullableProperty


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **int** |  | 
**name** | **str** |  | 

## Example

```python
from petstore_api.models.nullable_property import NullableProperty

# TODO update the JSON string below
json = "{}"
# create an instance of NullableProperty from a JSON string
nullable_property_instance = NullableProperty.model_validate_json(json)
# print the JSON string representation of the object
print(NullableProperty.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
nullable_property_dict = nullable_property_instance.model_dump(by_alias=True)
# create an instance of NullableProperty from a dict
nullable_property_from_dict = NullableProperty.model_validate(nullable_property_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


