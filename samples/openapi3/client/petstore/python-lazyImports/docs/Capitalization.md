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
capitalization_instance = Capitalization.model_validate_json(json)
# print the JSON string representation of the object
print(Capitalization.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
capitalization_dict = capitalization_instance.model_dump(by_alias=True)
# create an instance of Capitalization from a dict
capitalization_from_dict = Capitalization.model_validate(capitalization_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


