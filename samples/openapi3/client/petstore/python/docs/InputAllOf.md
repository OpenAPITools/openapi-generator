# InputAllOf


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**some_data** | [**Dict[str, Tag]**](Tag.md) |  | [optional] 

## Example

```python
from petstore_api.models.input_all_of import InputAllOf

# TODO update the JSON string below
json = "{}"
# create an instance of InputAllOf from a JSON string
input_all_of_instance = InputAllOf.model_validate_json(json)
# print the JSON string representation of the object
print(InputAllOf.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
input_all_of_dict = input_all_of_instance.model_dump(by_alias=True)
# create an instance of InputAllOf from a dict
input_all_of_from_dict = InputAllOf.model_validate(input_all_of_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


