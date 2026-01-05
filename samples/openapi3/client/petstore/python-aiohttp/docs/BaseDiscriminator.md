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
base_discriminator_instance = BaseDiscriminator.model_validate_json(json)
# print the JSON string representation of the object
print(BaseDiscriminator.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
base_discriminator_dict = base_discriminator_instance.model_dump(by_alias=True)
# create an instance of BaseDiscriminator from a dict
base_discriminator_from_dict = BaseDiscriminator.model_validate(base_discriminator_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


