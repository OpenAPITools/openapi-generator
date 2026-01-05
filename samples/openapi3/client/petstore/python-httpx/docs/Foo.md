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
foo_instance = Foo.model_validate_json(json)
# print the JSON string representation of the object
print(Foo.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
foo_dict = foo_instance.model_dump(by_alias=True)
# create an instance of Foo from a dict
foo_from_dict = Foo.model_validate(foo_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


