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
animal_instance = Animal.from_json(json)
# print the JSON string representation of the object
print(Animal.to_json())

# convert the object into a dict
animal_dict = animal_instance.to_dict()
# create an instance of Animal from a dict
animal_from_dict = Animal.from_dict(animal_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


