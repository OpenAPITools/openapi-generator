# SecondRef


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**category** | **str** |  | [optional] 
**circular_ref** | [**CircularReferenceModel**](CircularReferenceModel.md) |  | [optional] 

## Example

```python
from petstore_api.models.second_ref import SecondRef

# TODO update the JSON string below
json = "{}"
# create an instance of SecondRef from a JSON string
second_ref_instance = SecondRef.from_json(json)
# print the JSON string representation of the object
print(SecondRef.to_json())

# convert the object into a dict
second_ref_dict = second_ref_instance.to_dict()
# create an instance of SecondRef from a dict
second_ref_from_dict = SecondRef.from_dict(second_ref_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


