# Tiger


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**skill** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.tiger import Tiger

# TODO update the JSON string below
json = "{}"
# create an instance of Tiger from a JSON string
tiger_instance = Tiger.model_validate_json(json)
# print the JSON string representation of the object
print(Tiger.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
tiger_dict = tiger_instance.model_dump(by_alias=True)
# create an instance of Tiger from a dict
tiger_from_dict = Tiger.model_validate(tiger_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


