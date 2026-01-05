# FirstRef


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**category** | **str** |  | [optional] 
**self_ref** | [**SecondRef**](SecondRef.md) |  | [optional] 

## Example

```python
from petstore_api.models.first_ref import FirstRef

# TODO update the JSON string below
json = "{}"
# create an instance of FirstRef from a JSON string
first_ref_instance = FirstRef.model_validate_json(json)
# print the JSON string representation of the object
print(FirstRef.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
first_ref_dict = first_ref_instance.model_dump(by_alias=True)
# create an instance of FirstRef from a dict
first_ref_from_dict = FirstRef.model_validate(first_ref_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


