# Petstore::AdditionalPropertiesClass

## Properties

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **map_property** | **Hash&lt;String, String&gt;** |  | [optional] |
| **map_of_map_property** | **Hash&lt;String, Hash&lt;String, String&gt;&gt;** |  | [optional] |

## Example

```ruby
require 'petstore'

instance = Petstore::AdditionalPropertiesClass.new(
  map_property: null,
  map_of_map_property: null
)
```

