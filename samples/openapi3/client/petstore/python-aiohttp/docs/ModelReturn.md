# ModelReturn

Model for testing reserved words

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**var_return** | **int** |  | [optional] 

## Example

```python
from petstore_api.models.model_return import ModelReturn

# TODO update the JSON string below
json = "{}"
# create an instance of ModelReturn from a JSON string
model_return_instance = ModelReturn.from_json(json)
# print the JSON string representation of the object
print(ModelReturn.to_json())

# convert the object into a dict
model_return_dict = model_return_instance.to_dict()
# create an instance of ModelReturn from a dict
model_return_from_dict = ModelReturn.from_dict(model_return_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


