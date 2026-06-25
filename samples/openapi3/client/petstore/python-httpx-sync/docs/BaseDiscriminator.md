# BaseDiscriminator


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**type_name** | **str** |  | [optional] 

## Example

```python
from petstore_api.models.base_discriminator import BaseDiscriminator

# TODO update the JSON string below
json = "{}"
# create an instance of BaseDiscriminator from a JSON string
base_discriminator_instance = BaseDiscriminator.from_json(json)
# print the JSON string representation of the object
print(BaseDiscriminator.to_json())

# convert the object into a dict
base_discriminator_dict = base_discriminator_instance.to_dict()
# create an instance of BaseDiscriminator from a dict
base_discriminator_from_dict = BaseDiscriminator.from_dict(base_discriminator_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


