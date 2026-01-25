# DummyModel


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**category** | **str** |  | [optional] 
**self_ref** | [**SelfReferenceModel**](SelfReferenceModel.md) |  | [optional] 

## Example

```python
from petstore_api.models.dummy_model import DummyModel

# TODO update the JSON string below
json = "{}"
# create an instance of DummyModel from a JSON string
dummy_model_instance = DummyModel.from_json(json)
# print the JSON string representation of the object
print(DummyModel.to_json())

# convert the object into a dict
dummy_model_dict = dummy_model_instance.to_dict()
# create an instance of DummyModel from a dict
dummy_model_from_dict = DummyModel.from_dict(dummy_model_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


