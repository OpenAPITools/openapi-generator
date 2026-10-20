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
special_name_instance = SpecialName.from_json(json)
# print the JSON string representation of the object
print(SpecialName.to_json())

# convert the object into a dict
special_name_dict = special_name_instance.to_dict()
# create an instance of SpecialName from a dict
special_name_from_dict = SpecialName.from_dict(special_name_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


