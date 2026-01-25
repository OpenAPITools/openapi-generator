# Capitalization


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**small_camel** | **str** |  | [optional] 
**capital_camel** | **str** |  | [optional] 
**small_snake** | **str** |  | [optional] 
**capital_snake** | **str** |  | [optional] 
**sca_eth_flow_points** | **str** |  | [optional] 
**att_name** | **str** | Name of the pet  | [optional] 

## Example

```python
from petstore_api.models.capitalization import Capitalization

# TODO update the JSON string below
json = "{}"
# create an instance of Capitalization from a JSON string
capitalization_instance = Capitalization.from_json(json)
# print the JSON string representation of the object
print(Capitalization.to_json())

# convert the object into a dict
capitalization_dict = capitalization_instance.to_dict()
# create an instance of Capitalization from a dict
capitalization_from_dict = Capitalization.from_dict(capitalization_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


