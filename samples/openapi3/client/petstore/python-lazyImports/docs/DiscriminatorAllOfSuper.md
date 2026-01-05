# DiscriminatorAllOfSuper


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**element_type** | **str** |  | 

## Example

```python
from petstore_api.models.discriminator_all_of_super import DiscriminatorAllOfSuper

# TODO update the JSON string below
json = "{}"
# create an instance of DiscriminatorAllOfSuper from a JSON string
discriminator_all_of_super_instance = DiscriminatorAllOfSuper.model_validate_json(json)
# print the JSON string representation of the object
print(DiscriminatorAllOfSuper.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
discriminator_all_of_super_dict = discriminator_all_of_super_instance.model_dump(by_alias=True)
# create an instance of DiscriminatorAllOfSuper from a dict
discriminator_all_of_super_from_dict = DiscriminatorAllOfSuper.model_validate(discriminator_all_of_super_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


