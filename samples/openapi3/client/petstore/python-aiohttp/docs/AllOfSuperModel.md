# AllOfSuperModel


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.all_of_super_model import AllOfSuperModel

# TODO update the JSON string below
json = "{}"
# create an instance of AllOfSuperModel from a JSON string
all_of_super_model_instance = AllOfSuperModel.model_validate_json(json)
# print the JSON string representation of the object
print(AllOfSuperModel.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
all_of_super_model_dict = all_of_super_model_instance.model_dump(by_alias=True)
# create an instance of AllOfSuperModel from a dict
all_of_super_model_from_dict = AllOfSuperModel.model_validate(all_of_super_model_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


