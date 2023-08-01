# AdditionalPropertieObject


## Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.additional_propertie_object import AdditionalPropertieObject

# TODO update the JSON string below
json = "{}"
# create an instance of AdditionalPropertieObject from a JSON string
additional_propertie_object_instance = AdditionalPropertieObject.from_json(json)
# print the JSON string representation of the object
print AdditionalPropertieObject.to_json()

# convert the object into a dict
additional_propertie_object_dict = additional_propertie_object_instance.to_dict()
# create an instance of AdditionalPropertieObject from a dict
additional_propertie_object_form_dict = additional_propertie_object.from_dict(additional_propertie_object_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


