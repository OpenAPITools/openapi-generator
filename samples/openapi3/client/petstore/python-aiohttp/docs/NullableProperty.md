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
nullable_property_instance = NullableProperty.from_json(json)
# print the JSON string representation of the object
print(NullableProperty.to_json())

# convert the object into a dict
nullable_property_dict = nullable_property_instance.to_dict()
# create an instance of NullableProperty from a dict
nullable_property_from_dict = NullableProperty.from_dict(nullable_property_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


