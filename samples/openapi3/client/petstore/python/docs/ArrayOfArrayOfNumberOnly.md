# ArrayOfArrayOfNumberOnly


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**array_array_number** | **List[List[float]]** |  | [optional] 

## Example

```python
from petstore_api.models.array_of_array_of_number_only import ArrayOfArrayOfNumberOnly

# TODO update the JSON string below
json = "{}"
# create an instance of ArrayOfArrayOfNumberOnly from a JSON string
array_of_array_of_number_only_instance = ArrayOfArrayOfNumberOnly.model_validate_json(json)
# print the JSON string representation of the object
print(ArrayOfArrayOfNumberOnly.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
array_of_array_of_number_only_dict = array_of_array_of_number_only_instance.model_dump(by_alias=True)
# create an instance of ArrayOfArrayOfNumberOnly from a dict
array_of_array_of_number_only_from_dict = ArrayOfArrayOfNumberOnly.model_validate(array_of_array_of_number_only_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


