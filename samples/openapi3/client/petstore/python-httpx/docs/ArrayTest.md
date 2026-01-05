# ArrayTest


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**array_of_string** | **List[str]** |  | [optional] 
**array_of_nullable_float** | **List[Optional[float]]** |  | [optional] 
**array_array_of_integer** | **List[List[int]]** |  | [optional] 
**array_array_of_model** | **List[List[ReadOnlyFirst]]** |  | [optional] 

## Example

```python
from petstore_api.models.array_test import ArrayTest

# TODO update the JSON string below
json = "{}"
# create an instance of ArrayTest from a JSON string
array_test_instance = ArrayTest.model_validate_json(json)
# print the JSON string representation of the object
print(ArrayTest.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
array_test_dict = array_test_instance.model_dump(by_alias=True)
# create an instance of ArrayTest from a dict
array_test_from_dict = ArrayTest.model_validate(array_test_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


