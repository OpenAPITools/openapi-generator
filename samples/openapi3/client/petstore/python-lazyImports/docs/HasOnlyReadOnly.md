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
has_only_read_only_instance = HasOnlyReadOnly.from_json(json)
# print the JSON string representation of the object
print(HasOnlyReadOnly.to_json())

# convert the object into a dict
has_only_read_only_dict = has_only_read_only_instance.to_dict()
# create an instance of HasOnlyReadOnly from a dict
has_only_read_only_from_dict = HasOnlyReadOnly.from_dict(has_only_read_only_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


