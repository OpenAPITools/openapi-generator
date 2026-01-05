# ReadOnlyFirst


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**bar** | **str** |  | [optional] [readonly] 
**baz** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.read_only_first import ReadOnlyFirst

# TODO update the JSON string below
json = "{}"
# create an instance of ReadOnlyFirst from a JSON string
read_only_first_instance = ReadOnlyFirst.model_validate_json(json)
# print the JSON string representation of the object
print(ReadOnlyFirst.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
read_only_first_dict = read_only_first_instance.model_dump(by_alias=True)
# create an instance of ReadOnlyFirst from a dict
read_only_first_from_dict = ReadOnlyFirst.model_validate(read_only_first_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


