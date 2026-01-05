# AnyOfPig


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**class_name** | **str** |  | 
**color** | **str** |  | 
**size** | **int** |  | 

## Example

```python
from petstore_api.models.any_of_pig import AnyOfPig

# TODO update the JSON string below
json = "{}"
# create an instance of AnyOfPig from a JSON string
any_of_pig_instance = AnyOfPig.model_validate_json(json)
# print the JSON string representation of the object
print(AnyOfPig.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
any_of_pig_dict = any_of_pig_instance.model_dump(by_alias=True)
# create an instance of AnyOfPig from a dict
any_of_pig_from_dict = AnyOfPig.model_validate(any_of_pig_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


