# SpecialModelName


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**special_property_name** | **int** |  | [optional] 

## Example

```python
from petstore_api.models.special_model_name import SpecialModelName

# TODO update the JSON string below
json = "{}"
# create an instance of SpecialModelName from a JSON string
special_model_name_instance = SpecialModelName.model_validate_json(json)
# print the JSON string representation of the object
print(SpecialModelName.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
special_model_name_dict = special_model_name_instance.model_dump(by_alias=True)
# create an instance of SpecialModelName from a dict
special_model_name_from_dict = SpecialModelName.model_validate(special_model_name_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


