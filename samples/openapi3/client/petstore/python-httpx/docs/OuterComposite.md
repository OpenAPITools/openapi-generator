# OuterComposite


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**my_number** | **float** |  | [optional] 
**my_string** | **str** |  | [optional] 
**my_boolean** | **bool** |  | [optional] 

## Example

```python
from petstore_api.models.outer_composite import OuterComposite

# TODO update the JSON string below
json = "{}"
# create an instance of OuterComposite from a JSON string
outer_composite_instance = OuterComposite.model_validate_json(json)
# print the JSON string representation of the object
print(OuterComposite.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
outer_composite_dict = outer_composite_instance.model_dump(by_alias=True)
# create an instance of OuterComposite from a dict
outer_composite_from_dict = OuterComposite.model_validate(outer_composite_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


