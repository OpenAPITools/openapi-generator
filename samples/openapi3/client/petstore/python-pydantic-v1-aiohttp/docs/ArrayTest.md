# ArrayTest


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**array_of_string** | **list[str]** |  | [optional] 
**array_of_nullable_float** | **list[float]** |  | [optional] 
**array_array_of_integer** | **list[list[int]]** |  | [optional] 
**array_array_of_model** | **list[list[ReadOnlyFirst]]** |  | [optional] 

## Example

```python
from petstore_api.models.array_test import ArrayTest

# TODO update the JSON string below
json = "{}"
# create an instance of ArrayTest from a JSON string
array_test_instance = ArrayTest.from_json(json)
# print the JSON string representation of the object
print ArrayTest.to_json()

# convert the object into a dict
array_test_dict = array_test_instance.to_dict()
# create an instance of ArrayTest from a dict
array_test_from_dict = ArrayTest.from_dict(array_test_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


