# SelfReferenceModel


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**size** | **int** |  | [optional] 
**nested** | [**DummyModel**](DummyModel.md) |  | [optional] 

## Example

```python
from petstore_api.models.self_reference_model import SelfReferenceModel

# TODO update the JSON string below
json = "{}"
# create an instance of SelfReferenceModel from a JSON string
self_reference_model_instance = SelfReferenceModel.from_json(json)
# print the JSON string representation of the object
print(SelfReferenceModel.to_json())

# convert the object into a dict
self_reference_model_dict = self_reference_model_instance.to_dict()
# create an instance of SelfReferenceModel from a dict
self_reference_model_from_dict = SelfReferenceModel.from_dict(self_reference_model_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


