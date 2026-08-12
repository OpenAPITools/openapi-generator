# Dog


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**breed** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.dog import Dog

# TODO update the JSON string below
json = "{}"
# create an instance of Dog from a JSON string
dog_instance = Dog.from_json(json)
# print the JSON string representation of the object
print(Dog.to_json())

# convert the object into a dict
dog_dict = dog_instance.to_dict()
# create an instance of Dog from a dict
dog_from_dict = Dog.from_dict(dog_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


