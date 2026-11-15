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
all_of_with_single_ref_instance = AllOfWithSingleRef.from_json(json)
# print the JSON string representation of the object
print(AllOfWithSingleRef.to_json())

# convert the object into a dict
all_of_with_single_ref_dict = all_of_with_single_ref_instance.to_dict()
# create an instance of AllOfWithSingleRef from a dict
all_of_with_single_ref_from_dict = AllOfWithSingleRef.from_dict(all_of_with_single_ref_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


