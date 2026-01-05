# CircularReferenceModel


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**size** | **int** |  | [optional] 
**nested** | [**FirstRef**](FirstRef.md) |  | [optional] 

## Example

```python
from petstore_api.models.circular_reference_model import CircularReferenceModel

# TODO update the JSON string below
json = "{}"
# create an instance of CircularReferenceModel from a JSON string
circular_reference_model_instance = CircularReferenceModel.model_validate_json(json)
# print the JSON string representation of the object
print(CircularReferenceModel.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
circular_reference_model_dict = circular_reference_model_instance.model_dump(by_alias=True)
# create an instance of CircularReferenceModel from a dict
circular_reference_model_from_dict = CircularReferenceModel.model_validate(circular_reference_model_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


