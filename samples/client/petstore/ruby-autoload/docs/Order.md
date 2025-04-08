# Petstore::Order

## Properties

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **id** | **Integer** |  | [optional] |
| **pet_id** | **Integer** |  | [optional] |
| **quantity** | **Integer** |  | [optional] |
| **ship_date** | **Time** |  | [optional] |
| **status** | **String** | Order Status | [optional] |
| **complete** | **Boolean** |  | [optional][default to false] |
| **payment_method** | **Float** | Various payment methods | [optional][default to PAYMENT_METHOD::N1] |

## Example

```ruby
require 'petstore'

instance = Petstore::Order.new(
  id: null,
  pet_id: null,
  quantity: null,
  ship_date: null,
  status: null,
  complete: null,
  payment_method: 1
)
```

