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
input_all_of_instance = InputAllOf.from_json(json)
# print the JSON string representation of the object
print(InputAllOf.to_json())

# convert the object into a dict
input_all_of_dict = input_all_of_instance.to_dict()
# create an instance of InputAllOf from a dict
input_all_of_from_dict = InputAllOf.from_dict(input_all_of_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


