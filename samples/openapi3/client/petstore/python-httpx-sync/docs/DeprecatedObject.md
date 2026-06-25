# DeprecatedObject


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.deprecated_object import DeprecatedObject

# TODO update the JSON string below
json = "{}"
# create an instance of DeprecatedObject from a JSON string
deprecated_object_instance = DeprecatedObject.from_json(json)
# print the JSON string representation of the object
print(DeprecatedObject.to_json())

# convert the object into a dict
deprecated_object_dict = deprecated_object_instance.to_dict()
# create an instance of DeprecatedObject from a dict
deprecated_object_from_dict = DeprecatedObject.from_dict(deprecated_object_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


