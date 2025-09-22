# Model200Response

Model for testing model name starting with number

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **int** |  | [optional] 
**var_class** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.model200_response import Model200Response

# TODO update the JSON string below
json = "{}"
# create an instance of Model200Response from a JSON string
model200_response_instance = Model200Response.from_json(json)
# print the JSON string representation of the object
print(Model200Response.to_json())

# convert the object into a dict
model200_response_dict = model200_response_instance.to_dict()
# create an instance of Model200Response from a dict
model200_response_from_dict = Model200Response.from_dict(model200_response_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


