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
multi_arrays_instance = MultiArrays.model_validate_json(json)
# print the JSON string representation of the object
print(MultiArrays.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
multi_arrays_dict = multi_arrays_instance.model_dump(by_alias=True)
# create an instance of MultiArrays from a dict
multi_arrays_from_dict = MultiArrays.model_validate(multi_arrays_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


