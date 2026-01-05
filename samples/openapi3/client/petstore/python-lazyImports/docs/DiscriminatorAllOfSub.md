# DiscriminatorAllOfSub


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------

## Example

```python
from petstore_api.models.discriminator_all_of_sub import DiscriminatorAllOfSub

# TODO update the JSON string below
json = "{}"
# create an instance of DiscriminatorAllOfSub from a JSON string
discriminator_all_of_sub_instance = DiscriminatorAllOfSub.model_validate_json(json)
# print the JSON string representation of the object
print(DiscriminatorAllOfSub.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
discriminator_all_of_sub_dict = discriminator_all_of_sub_instance.model_dump(by_alias=True)
# create an instance of DiscriminatorAllOfSub from a dict
discriminator_all_of_sub_from_dict = DiscriminatorAllOfSub.model_validate(discriminator_all_of_sub_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


