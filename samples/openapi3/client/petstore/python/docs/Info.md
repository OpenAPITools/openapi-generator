# Info


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**val** | [**BaseDiscriminator**](BaseDiscriminator.md) |  | [optional] 

## Example

```python
from petstore_api.models.info import Info

# TODO update the JSON string below
json = "{}"
# create an instance of Info from a JSON string
info_instance = Info.model_validate_json(json)
# print the JSON string representation of the object
print(Info.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
info_dict = info_instance.model_dump(by_alias=True)
# create an instance of Info from a dict
info_from_dict = Info.model_validate(info_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


