# ClassModel

Model for testing model with \"_class\" property

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**var_class** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.class_model import ClassModel

# TODO update the JSON string below
json = "{}"
# create an instance of ClassModel from a JSON string
class_model_instance = ClassModel.model_validate_json(json)
# print the JSON string representation of the object
print(ClassModel.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
class_model_dict = class_model_instance.model_dump(by_alias=True)
# create an instance of ClassModel from a dict
class_model_from_dict = ClassModel.model_validate(class_model_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


