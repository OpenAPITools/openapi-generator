# HasOnlyReadOnly


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**bar** | **str** |  | [optional] [readonly] 
**foo** | **str** |  | [optional] [readonly] 

## Example

```python
from petstore_api.models.has_only_read_only import HasOnlyReadOnly

# TODO update the JSON string below
json = "{}"
# create an instance of HasOnlyReadOnly from a JSON string
has_only_read_only_instance = HasOnlyReadOnly.model_validate_json(json)
# print the JSON string representation of the object
print(HasOnlyReadOnly.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
has_only_read_only_dict = has_only_read_only_instance.model_dump(by_alias=True)
# create an instance of HasOnlyReadOnly from a dict
has_only_read_only_from_dict = HasOnlyReadOnly.model_validate(has_only_read_only_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


