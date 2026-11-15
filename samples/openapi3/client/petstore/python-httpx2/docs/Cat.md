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
cat_instance = Cat.from_json(json)
# print the JSON string representation of the object
print(Cat.to_json())

# convert the object into a dict
cat_dict = cat_instance.to_dict()
# create an instance of Cat from a dict
cat_from_dict = Cat.from_dict(cat_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


