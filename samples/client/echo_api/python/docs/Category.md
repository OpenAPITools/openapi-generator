# Category


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **int** |  | [optional] 
**name** | **str** |  | [optional] 

## Example

```python
from openapi_client.models.category import Category

# TODO update the JSON string below
json = "{}"
# create an instance of Category from a JSON string
category_instance = Category.model_validate_json(json)
# print the JSON string representation of the object
print(Category.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
category_dict = category_instance.model_dump(by_alias=True)
# create an instance of Category from a dict
category_from_dict = Category.model_validate(category_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


