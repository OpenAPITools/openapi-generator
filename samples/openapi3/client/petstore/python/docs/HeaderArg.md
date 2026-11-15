# HeaderArg


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**path** | **str** |  | [optional] 
**recursive** | **bool** |  | [optional] 

## Example

```python
from petstore_api.models.header_arg import HeaderArg

# TODO update the JSON string below
json = "{}"
# create an instance of HeaderArg from a JSON string
header_arg_instance = HeaderArg.from_json(json)
# print the JSON string representation of the object
print(HeaderArg.to_json())

# convert the object into a dict
header_arg_dict = header_arg_instance.to_dict()
# create an instance of HeaderArg from a dict
header_arg_from_dict = HeaderArg.from_dict(header_arg_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


