# Animal


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**class_name** | **str** |  | 
**color** | **str** |  | [optional] [default to 'red']

## Example

```python
from petstore_api.models.animal import Animal

# TODO update the JSON string below
json = "{}"
# create an instance of Animal from a JSON string
animal_instance = Animal.model_validate_json(json)
# print the JSON string representation of the object
print(Animal.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
animal_dict = animal_instance.model_dump(by_alias=True)
# create an instance of Animal from a dict
animal_from_dict = Animal.model_validate(animal_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


