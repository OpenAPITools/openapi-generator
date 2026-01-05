# Query


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **int** | Query | [optional] 
**outcomes** | **List[str]** |  | [optional] [default to ["SUCCESS","FAILURE"]]

## Example

```python
from openapi_client.models.query import Query

# TODO update the JSON string below
json = "{}"
# create an instance of Query from a JSON string
query_instance = Query.model_validate_json(json)
# print the JSON string representation of the object
print(Query.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
query_dict = query_instance.model_dump(by_alias=True)
# create an instance of Query from a dict
query_from_dict = Query.model_validate(query_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


