# SecondCircularAllOfRef


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **str** |  | [optional] 
**circular_all_of_ref** | [**List[CircularAllOfRef]**](CircularAllOfRef.md) |  | [optional] 

## Example

```python
from petstore_api.models.second_circular_all_of_ref import SecondCircularAllOfRef

# TODO update the JSON string below
json = "{}"
# create an instance of SecondCircularAllOfRef from a JSON string
second_circular_all_of_ref_instance = SecondCircularAllOfRef.model_validate_json(json)
# print the JSON string representation of the object
print(SecondCircularAllOfRef.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
second_circular_all_of_ref_dict = second_circular_all_of_ref_instance.model_dump(by_alias=True)
# create an instance of SecondCircularAllOfRef from a dict
second_circular_all_of_ref_from_dict = SecondCircularAllOfRef.model_validate(second_circular_all_of_ref_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


