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
first_ref_instance = FirstRef.from_json(json)
# print the JSON string representation of the object
print(FirstRef.to_json())

# convert the object into a dict
first_ref_dict = first_ref_instance.to_dict()
# create an instance of FirstRef from a dict
first_ref_from_dict = FirstRef.from_dict(first_ref_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


