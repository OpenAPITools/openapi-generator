# Petstore::ReadOnlyFirst

## Properties

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **bar** | **String** |  | [optional][readonly] |
| **baz** | **String** |  | [optional] |

## Example

```ruby
require 'petstore'

instance = Petstore::ReadOnlyFirst.new(
  bar: null,
  baz: null
)
```

