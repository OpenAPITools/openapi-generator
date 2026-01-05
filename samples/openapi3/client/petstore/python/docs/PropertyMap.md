# PropertyMap


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**some_data** | [**Dict[str, Tag]**](Tag.md) |  | [optional] 

## Example

```python
from petstore_api.models.property_map import PropertyMap

# TODO update the JSON string below
json = "{}"
# create an instance of PropertyMap from a JSON string
property_map_instance = PropertyMap.model_validate_json(json)
# print the JSON string representation of the object
print(PropertyMap.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
property_map_dict = property_map_instance.model_dump(by_alias=True)
# create an instance of PropertyMap from a dict
property_map_from_dict = PropertyMap.model_validate(property_map_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


