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
model_return_instance = ModelReturn.model_validate_json(json)
# print the JSON string representation of the object
print(ModelReturn.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
model_return_dict = model_return_instance.model_dump(by_alias=True)
# create an instance of ModelReturn from a dict
model_return_from_dict = ModelReturn.model_validate(model_return_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


