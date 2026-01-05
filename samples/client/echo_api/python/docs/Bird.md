# Bird


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**size** | **str** |  | [optional] 
**color** | **str** |  | [optional] 

## Example

```python
from openapi_client.models.bird import Bird

# TODO update the JSON string below
json = "{}"
# create an instance of Bird from a JSON string
bird_instance = Bird.model_validate_json(json)
# print the JSON string representation of the object
print(Bird.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
bird_dict = bird_instance.model_dump(by_alias=True)
# create an instance of Bird from a dict
bird_from_dict = Bird.model_validate(bird_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


