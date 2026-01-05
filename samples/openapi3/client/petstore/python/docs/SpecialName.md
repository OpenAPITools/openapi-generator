# SpecialName


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**var_property** | **int** |  | [optional] 
**var_async** | [**Category**](Category.md) |  | [optional] 
**var_schema** | **str** | pet status in the store | [optional] 

## Example

```python
from petstore_api.models.special_name import SpecialName

# TODO update the JSON string below
json = "{}"
# create an instance of SpecialName from a JSON string
special_name_instance = SpecialName.model_validate_json(json)
# print the JSON string representation of the object
print(SpecialName.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
special_name_dict = special_name_instance.model_dump(by_alias=True)
# create an instance of SpecialName from a dict
special_name_from_dict = SpecialName.model_validate(special_name_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


