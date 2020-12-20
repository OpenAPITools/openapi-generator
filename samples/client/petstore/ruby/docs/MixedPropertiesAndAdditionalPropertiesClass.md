# Petstore::MixedPropertiesAndAdditionalPropertiesClass

## Properties

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **uuid** | **String** |  | [optional] |
| **date_time** | **Time** |  | [optional] |
| **map** | [**Hash&lt;String, Animal&gt;**](Animal.md) |  | [optional] |

## Example

```ruby
require 'petstore'

instance = Petstore::MixedPropertiesAndAdditionalPropertiesClass.new(
  uuid: null,
  date_time: null,
  map: null
)
```

