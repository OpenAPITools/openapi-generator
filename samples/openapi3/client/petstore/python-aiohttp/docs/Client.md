# Client


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**client** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.client import Client

# TODO update the JSON string below
json = "{}"
# create an instance of Client from a JSON string
client_instance = Client.model_validate_json(json)
# print the JSON string representation of the object
print(Client.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
client_dict = client_instance.model_dump(by_alias=True)
# create an instance of Client from a dict
client_from_dict = Client.model_validate(client_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


