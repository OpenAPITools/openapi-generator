# FooGetDefaultResponse


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**string** | [**Foo**](Foo.md) |  | [optional] 

## Example

```python
from petstore_api.models.foo_get_default_response import FooGetDefaultResponse

# TODO update the JSON string below
json = "{}"
# create an instance of FooGetDefaultResponse from a JSON string
foo_get_default_response_instance = FooGetDefaultResponse.from_json(json)
# print the JSON string representation of the object
print(FooGetDefaultResponse.to_json())

# convert the object into a dict
foo_get_default_response_dict = foo_get_default_response_instance.to_dict()
# create an instance of FooGetDefaultResponse from a dict
foo_get_default_response_from_dict = FooGetDefaultResponse.from_dict(foo_get_default_response_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


