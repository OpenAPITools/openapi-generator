# Cat


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**declawed** | **bool** |  | [optional] 

## Example

```python
from petstore_api.models.cat import Cat

# TODO update the JSON string below
json = "{}"
# create an instance of Cat from a JSON string
cat_instance = Cat.model_validate_json(json)
# print the JSON string representation of the object
print(Cat.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
cat_dict = cat_instance.model_dump(by_alias=True)
# create an instance of Cat from a dict
cat_from_dict = Cat.model_validate(cat_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


