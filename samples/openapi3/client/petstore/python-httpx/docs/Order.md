# Order


## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | **int** |  | [optional] 
**pet_id** | **int** |  | [optional] 
**quantity** | **int** |  | [optional] 
**ship_date** | **datetime** |  | [optional] 
**status** | **str** | Order Status | [optional] 
**complete** | **bool** |  | [optional] [default to False]

## Example

```python
from petstore_api.models.order import Order

# TODO update the JSON string below
json = "{}"
# create an instance of Order from a JSON string
order_instance = Order.model_validate_json(json)
# print the JSON string representation of the object
print(Order.model_dump_json(by_alias=True, exclude_unset=True))

# convert the object into a dict
order_dict = order_instance.model_dump(by_alias=True)
# create an instance of Order from a dict
order_from_dict = Order.model_validate(order_dict)
```
[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


