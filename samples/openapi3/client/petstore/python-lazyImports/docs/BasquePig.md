# BasquePig


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**class_name** | **str** |  | 
**color** | **str** |  | 

## Example

```python
from petstore_api.models.basque_pig import BasquePig

# TODO update the JSON string below
json = "{}"
# create an instance of BasquePig from a JSON string
basque_pig_instance = BasquePig.model_validate_json(json)
# print the JSON string representation of the object
print(BasquePig.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
basque_pig_dict = basque_pig_instance.model_dump(by_alias=True)
# create an instance of BasquePig from a dict
basque_pig_from_dict = BasquePig.model_validate(basque_pig_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


