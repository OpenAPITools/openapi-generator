# MultiArrays


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**tags** | [**List[Tag]**](Tag.md) |  | [optional] 
**files** | [**List[File]**](File.md) | Another array of objects in addition to tags (mypy check to not to reuse the same iterator) | [optional] 

## Example

```python
from petstore_api.models.multi_arrays import MultiArrays

# TODO update the JSON string below
json = "{}"
# create an instance of MultiArrays from a JSON string
multi_arrays_instance = MultiArrays.from_json(json)
# print the JSON string representation of the object
print(MultiArrays.to_json())

# convert the object into a dict
multi_arrays_dict = multi_arrays_instance.to_dict()
# create an instance of MultiArrays from a dict
multi_arrays_from_dict = MultiArrays.from_dict(multi_arrays_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


