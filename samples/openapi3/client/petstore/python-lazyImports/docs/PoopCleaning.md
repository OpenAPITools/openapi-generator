# PoopCleaning


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**task_name** | **str** |  | 
**function_name** | **str** |  | 
**content** | **str** |  | 

## Example

```python
from petstore_api.models.poop_cleaning import PoopCleaning

# TODO update the JSON string below
json = "{}"
# create an instance of PoopCleaning from a JSON string
poop_cleaning_instance = PoopCleaning.model_validate_json(json)
# print the JSON string representation of the object
print(PoopCleaning.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
poop_cleaning_dict = poop_cleaning_instance.model_dump(by_alias=True)
# create an instance of PoopCleaning from a dict
poop_cleaning_from_dict = PoopCleaning.model_validate(poop_cleaning_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


