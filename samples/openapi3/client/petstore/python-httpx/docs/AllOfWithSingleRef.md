# AllOfWithSingleRef


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**username** | **str** |  | [optional] 
**single_ref_type** | [**SingleRefType**](SingleRefType.md) |  | [optional] 

## Example

```python
from petstore_api.models.all_of_with_single_ref import AllOfWithSingleRef

# TODO update the JSON string below
json = "{}"
# create an instance of AllOfWithSingleRef from a JSON string
all_of_with_single_ref_instance = AllOfWithSingleRef.model_validate_json(json)
# print the JSON string representation of the object
print(AllOfWithSingleRef.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
all_of_with_single_ref_dict = all_of_with_single_ref_instance.model_dump(by_alias=True)
# create an instance of AllOfWithSingleRef from a dict
all_of_with_single_ref_from_dict = AllOfWithSingleRef.model_validate(all_of_with_single_ref_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


