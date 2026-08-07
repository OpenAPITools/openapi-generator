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
poop_cleaning_instance = PoopCleaning.from_json(json)
# print the JSON string representation of the object
print(PoopCleaning.to_json())

# convert the object into a dict
poop_cleaning_dict = poop_cleaning_instance.to_dict()
# create an instance of PoopCleaning from a dict
poop_cleaning_from_dict = PoopCleaning.from_dict(poop_cleaning_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


