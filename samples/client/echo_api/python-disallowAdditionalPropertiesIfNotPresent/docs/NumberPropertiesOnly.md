# NumberPropertiesOnly


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**number** | **float** |  | [optional] 
**var_float** | **float** |  | [optional] 
**double** | **float** |  | [optional] 

## Example

```python
from openapi_client.models.number_properties_only import NumberPropertiesOnly

# TODO update the JSON string below
json = "{}"
# create an instance of NumberPropertiesOnly from a JSON string
number_properties_only_instance = NumberPropertiesOnly.model_validate_json(json)
# print the JSON string representation of the object
print(NumberPropertiesOnly.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
number_properties_only_dict = number_properties_only_instance.model_dump(by_alias=True)
# create an instance of NumberPropertiesOnly from a dict
number_properties_only_from_dict = NumberPropertiesOnly.model_validate(number_properties_only_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


