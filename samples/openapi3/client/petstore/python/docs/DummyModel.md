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
dummy_model_instance = DummyModel.model_validate_json(json)
# print the JSON string representation of the object
print(DummyModel.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
dummy_model_dict = dummy_model_instance.model_dump(by_alias=True)
# create an instance of DummyModel from a dict
dummy_model_from_dict = DummyModel.model_validate(dummy_model_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


