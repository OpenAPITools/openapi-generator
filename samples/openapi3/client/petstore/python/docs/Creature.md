# Creature


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**info** | [**CreatureInfo**](CreatureInfo.md) |  | 
**type** | **str** |  | 

## Example

```python
from petstore_api.models.creature import Creature

# TODO update the JSON string below
json = "{}"
# create an instance of Creature from a JSON string
creature_instance = Creature.from_json(json)
# print the JSON string representation of the object
print(Creature.to_json())

# convert the object into a dict
creature_dict = creature_instance.to_dict()
# create an instance of Creature from a dict
creature_from_dict = Creature.from_dict(creature_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


