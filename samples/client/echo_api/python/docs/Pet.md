# Pet


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **int** |  | [optional] 
**name** | **str** |  | 
**category** | [**Category**](Category.md) |  | [optional] 
**photo_urls** | **List[str]** |  | 
**tags** | [**List[Tag]**](Tag.md) |  | [optional] 
**status** | **str** | pet status in the store | [optional] 

## Example

```python
from openapi_client.models.pet import Pet

# TODO update the JSON string below
json = "{}"
# create an instance of Pet from a JSON string
pet_instance = Pet.model_validate_json(json)
# print the JSON string representation of the object
print(Pet.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
pet_dict = pet_instance.model_dump(by_alias=True)
# create an instance of Pet from a dict
pet_from_dict = Pet.model_validate(pet_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


