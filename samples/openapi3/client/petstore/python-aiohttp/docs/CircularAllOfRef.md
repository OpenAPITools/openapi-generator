# CircularAllOfRef


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **str** |  | [optional] 
**second_circular_all_of_ref** | [**List[SecondCircularAllOfRef]**](SecondCircularAllOfRef.md) |  | [optional] 

## Example

```python
from petstore_api.models.circular_all_of_ref import CircularAllOfRef

# TODO update the JSON string below
json = "{}"
# create an instance of CircularAllOfRef from a JSON string
circular_all_of_ref_instance = CircularAllOfRef.model_validate_json(json)
# print the JSON string representation of the object
print(CircularAllOfRef.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
circular_all_of_ref_dict = circular_all_of_ref_instance.model_dump(by_alias=True)
# create an instance of CircularAllOfRef from a dict
circular_all_of_ref_from_dict = CircularAllOfRef.model_validate(circular_all_of_ref_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


