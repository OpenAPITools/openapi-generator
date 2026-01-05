# Tag


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **int** |  | [optional] 
**name** | **str** |  | [optional] 

## Example

```python
from openapi_client.models.tag import Tag

# TODO update the JSON string below
json = "{}"
# create an instance of Tag from a JSON string
tag_instance = Tag.model_validate_json(json)
# print the JSON string representation of the object
print(Tag.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
tag_dict = tag_instance.model_dump(by_alias=True)
# create an instance of Tag from a dict
tag_from_dict = Tag.model_validate(tag_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


