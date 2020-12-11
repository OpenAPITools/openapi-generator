# Petstore::HasOnlyReadOnly

## Properties

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **bar** | **String** |  | [optional][readonly] |
| **foo** | **String** |  | [optional][readonly] |

## Example

```ruby
require 'petstore'

instance = Petstore::HasOnlyReadOnly.new(
  bar: null,
  foo: null
)
```

