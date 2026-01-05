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
creature_instance = Creature.model_validate_json(json)
# print the JSON string representation of the object
print(Creature.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
creature_dict = creature_instance.model_dump(by_alias=True)
# create an instance of Creature from a dict
creature_from_dict = Creature.model_validate(creature_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


