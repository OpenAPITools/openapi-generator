# TaggedUnion


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**kind** | **str** |  | 

## Example

```python
from petstore_api.models.tagged_union import TaggedUnion

# TODO update the JSON string below
json = "{}"
# create an instance of TaggedUnion from a JSON string
tagged_union_instance = TaggedUnion.from_json(json)
# print the JSON string representation of the object
print TaggedUnion.to_json()

# convert the object into a dict
tagged_union_dict = tagged_union_instance.to_dict()
# create an instance of TaggedUnion from a dict
tagged_union_from_dict = TaggedUnion.from_dict(tagged_union_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


