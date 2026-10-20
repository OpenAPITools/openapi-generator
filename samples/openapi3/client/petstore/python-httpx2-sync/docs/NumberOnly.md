# NumberOnly


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**just_number** | **float** |  | [optional] 

## Example

```python
from petstore_api.models.number_only import NumberOnly

# TODO update the JSON string below
json = "{}"
# create an instance of NumberOnly from a JSON string
number_only_instance = NumberOnly.from_json(json)
# print the JSON string representation of the object
print(NumberOnly.to_json())

# convert the object into a dict
number_only_dict = number_only_instance.to_dict()
# create an instance of NumberOnly from a dict
number_only_from_dict = NumberOnly.from_dict(number_only_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


