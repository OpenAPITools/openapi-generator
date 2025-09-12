# NullableClass


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**required_integer_prop** | **int** |  | 
**integer_prop** | **int** |  | [optional] 
**number_prop** | **float** |  | [optional] 
**boolean_prop** | **bool** |  | [optional] 
**string_prop** | **str** |  | [optional] 
**date_prop** | **date** |  | [optional] 
**datetime_prop** | **datetime** |  | [optional] 
**array_nullable_prop** | **List[object]** |  | [optional] 
**array_and_items_nullable_prop** | **List[Optional[object]]** |  | [optional] 
**array_items_nullable** | **List[Optional[object]]** |  | [optional] 
**object_nullable_prop** | **Dict[str, object]** |  | [optional] 
**object_and_items_nullable_prop** | **Dict[str, Optional[object]]** |  | [optional] 
**object_items_nullable** | **Dict[str, Optional[object]]** |  | [optional] 

## Example

```python
from petstore_api.models.nullable_class import NullableClass

# TODO update the JSON string below
json = "{}"
# create an instance of NullableClass from a JSON string
nullable_class_instance = NullableClass.from_json(json)
# print the JSON string representation of the object
print(NullableClass.to_json())

# convert the object into a dict
nullable_class_dict = nullable_class_instance.to_dict()
# create an instance of NullableClass from a dict
nullable_class_from_dict = NullableClass.from_dict(nullable_class_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


