# DataQuery


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**suffix** | **str** | test suffix | [optional] 
**text** | **str** | Some text containing white spaces | [optional] 
**var_date** | **datetime** | A date | [optional] 

## Example

```python
from openapi_client.models.data_query import DataQuery

# TODO update the JSON string below
json = "{}"
# create an instance of DataQuery from a JSON string
data_query_instance = DataQuery.from_json(json)
# print the JSON string representation of the object
print DataQuery.to_json()

# convert the object into a dict
data_query_dict = data_query_instance.to_dict()
# create an instance of DataQuery from a dict
data_query_form_dict = data_query.from_dict(data_query_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


