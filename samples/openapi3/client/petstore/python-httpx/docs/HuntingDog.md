# HuntingDog


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**is_trained** | **bool** |  | [optional] 

## Example

```python
from petstore_api.models.hunting_dog import HuntingDog

# TODO update the JSON string below
json = "{}"
# create an instance of HuntingDog from a JSON string
hunting_dog_instance = HuntingDog.model_validate_json(json)
# print the JSON string representation of the object
print(HuntingDog.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
hunting_dog_dict = hunting_dog_instance.model_dump(by_alias=True)
# create an instance of HuntingDog from a dict
hunting_dog_from_dict = HuntingDog.model_validate(hunting_dog_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


