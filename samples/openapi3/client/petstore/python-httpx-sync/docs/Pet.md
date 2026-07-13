# Pet


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **int** |  | [optional] 
**category** | [**Category**](Category.md) |  | [optional] 
**name** | **str** |  | 
**photo_urls** | **List[str]** |  | 
**tags** | [**List[Tag]**](Tag.md) |  | [optional] 
**status** | **str** | pet status in the store | [optional] 

## Example

```python
from petstore_api.models.pet import Pet

# TODO update the JSON string below
json = "{}"
# create an instance of Pet from a JSON string
pet_instance = Pet.from_json(json)
# print the JSON string representation of the object
print(Pet.to_json())

# convert the object into a dict
pet_dict = pet_instance.to_dict()
# create an instance of Pet from a dict
pet_from_dict = Pet.from_dict(pet_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


