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
model_api_response_instance = ModelApiResponse.model_validate_json(json)
# print the JSON string representation of the object
print(ModelApiResponse.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
model_api_response_dict = model_api_response_instance.model_dump(by_alias=True)
# create an instance of ModelApiResponse from a dict
model_api_response_from_dict = ModelApiResponse.model_validate(model_api_response_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


