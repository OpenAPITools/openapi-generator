# ModelField


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**var_field** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.model_field import ModelField

# TODO update the JSON string below
json = "{}"
# create an instance of ModelField from a JSON string
model_field_instance = ModelField.model_validate_json(json)
# print the JSON string representation of the object
print(ModelField.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
model_field_dict = model_field_instance.model_dump(by_alias=True)
# create an instance of ModelField from a dict
model_field_from_dict = ModelField.model_validate(model_field_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


