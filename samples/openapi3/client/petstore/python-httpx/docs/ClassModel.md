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
class_model_instance = ClassModel.from_json(json)
# print the JSON string representation of the object
print(ClassModel.to_json())

# convert the object into a dict
class_model_dict = class_model_instance.to_dict()
# create an instance of ClassModel from a dict
class_model_from_dict = ClassModel.from_dict(class_model_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


