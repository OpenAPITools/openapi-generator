# UuidWithPattern


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **UUID** |  | [optional] 

## Example

```python
from petstore_api.models.uuid_with_pattern import UuidWithPattern

# TODO update the JSON string below
json = "{}"
# create an instance of UuidWithPattern from a JSON string
uuid_with_pattern_instance = UuidWithPattern.from_json(json)
# print the JSON string representation of the object
print(UuidWithPattern.to_json())

# convert the object into a dict
uuid_with_pattern_dict = uuid_with_pattern_instance.to_dict()
# create an instance of UuidWithPattern from a dict
uuid_with_pattern_from_dict = UuidWithPattern.from_dict(uuid_with_pattern_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


