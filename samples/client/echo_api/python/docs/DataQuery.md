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
data_query_instance = DataQuery.model_validate_json(json)
# print the JSON string representation of the object
print(DataQuery.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
data_query_dict = data_query_instance.model_dump(by_alias=True)
# create an instance of DataQuery from a dict
data_query_from_dict = DataQuery.model_validate(data_query_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


