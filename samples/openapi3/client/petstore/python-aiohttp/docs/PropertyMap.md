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
property_map_instance = PropertyMap.from_json(json)
# print the JSON string representation of the object
print(PropertyMap.to_json())

# convert the object into a dict
property_map_dict = property_map_instance.to_dict()
# create an instance of PropertyMap from a dict
property_map_from_dict = PropertyMap.from_dict(property_map_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


