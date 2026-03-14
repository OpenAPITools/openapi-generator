# CreatureInfo


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **str** |  | 

## Example

```python
from petstore_api.models.creature_info import CreatureInfo

# TODO update the JSON string below
json = "{}"
# create an instance of CreatureInfo from a JSON string
creature_info_instance = CreatureInfo.from_json(json)
# print the JSON string representation of the object
print(CreatureInfo.to_json())

# convert the object into a dict
creature_info_dict = creature_info_instance.to_dict()
# create an instance of CreatureInfo from a dict
creature_info_from_dict = CreatureInfo.from_dict(creature_info_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


