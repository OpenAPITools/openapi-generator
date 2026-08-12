# Foo


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**bar** | **str** |  | [optional] [default to 'bar']

## Example

```python
from petstore_api.models.foo import Foo

# TODO update the JSON string below
json = "{}"
# create an instance of Foo from a JSON string
foo_instance = Foo.from_json(json)
# print the JSON string representation of the object
print(Foo.to_json())

# convert the object into a dict
foo_dict = foo_instance.to_dict()
# create an instance of Foo from a dict
foo_from_dict = Foo.from_dict(foo_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


