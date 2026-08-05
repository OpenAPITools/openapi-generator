# ModelApiResponse


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**code** | **int** |  | [optional] 
**type** | **str** |  | [optional] 
**message** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.model_api_response import ModelApiResponse

# TODO update the JSON string below
json = "{}"
# create an instance of ModelApiResponse from a JSON string
model_api_response_instance = ModelApiResponse.from_json(json)
# print the JSON string representation of the object
print(ModelApiResponse.to_json())

# convert the object into a dict
model_api_response_dict = model_api_response_instance.to_dict()
# create an instance of ModelApiResponse from a dict
model_api_response_from_dict = ModelApiResponse.from_dict(model_api_response_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


