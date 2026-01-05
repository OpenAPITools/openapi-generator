# PonySizes


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**type** | [**Type**](Type.md) |  | [optional] 

## Example

```python
from petstore_api.models.pony_sizes import PonySizes

# TODO update the JSON string below
json = "{}"
# create an instance of PonySizes from a JSON string
pony_sizes_instance = PonySizes.model_validate_json(json)
# print the JSON string representation of the object
print(PonySizes.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
pony_sizes_dict = pony_sizes_instance.model_dump(by_alias=True)
# create an instance of PonySizes from a dict
pony_sizes_from_dict = PonySizes.model_validate(pony_sizes_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


