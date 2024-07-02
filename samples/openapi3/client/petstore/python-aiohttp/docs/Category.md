# Category


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **int** |  | [optional] 
**name** | **str** |  | [default to 'default-name']

## Example

```python
from petstore_api.models.category import Category

# TODO update the JSON string below
json = "{}"
# create an instance of Category from a JSON string
category_instance = Category.from_json(json)
# print the JSON string representation of the object
print(Category.to_json())

# convert the object into a dict
category_dict = category_instance.to_dict()
# create an instance of Category from a dict
category_from_dict = Category.from_dict(category_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


