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
creature_info_instance = CreatureInfo.model_validate_json(json)
# print the JSON string representation of the object
print(CreatureInfo.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
creature_info_dict = creature_info_instance.model_dump(by_alias=True)
# create an instance of CreatureInfo from a dict
creature_info_from_dict = CreatureInfo.model_validate(creature_info_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


