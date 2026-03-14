# PropertyNameCollision


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**underscore_type** | **str** |  | [optional] 
**type** | **str** |  | [optional] 
**type_with_underscore** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.property_name_collision import PropertyNameCollision

# TODO update the JSON string below
json = "{}"
# create an instance of PropertyNameCollision from a JSON string
property_name_collision_instance = PropertyNameCollision.from_json(json)
# print the JSON string representation of the object
print(PropertyNameCollision.to_json())

# convert the object into a dict
property_name_collision_dict = property_name_collision_instance.to_dict()
# create an instance of PropertyNameCollision from a dict
property_name_collision_from_dict = PropertyNameCollision.from_dict(property_name_collision_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


