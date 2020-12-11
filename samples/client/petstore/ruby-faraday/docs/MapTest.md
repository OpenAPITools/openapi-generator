# Petstore::MapTest

## Properties

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **map_map_of_string** | **Hash&lt;String, Hash&lt;String, String&gt;&gt;** |  | [optional] |
| **map_of_enum_string** | **Hash&lt;String, String&gt;** |  | [optional] |
| **direct_map** | **Hash&lt;String, Boolean&gt;** |  | [optional] |
| **indirect_map** | **Hash&lt;String, Boolean&gt;** |  | [optional] |

## Example

```ruby
require 'petstore'

instance = Petstore::MapTest.new(
  map_map_of_string: null,
  map_of_enum_string: null,
  direct_map: null,
  indirect_map: null
)
```

