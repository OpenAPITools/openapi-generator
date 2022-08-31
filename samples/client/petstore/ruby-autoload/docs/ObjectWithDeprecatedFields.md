# Petstore::ObjectWithDeprecatedFields

## Properties

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **uuid** | **String** |  | [optional] |
| **id** | **Float** |  | [optional] |
| **deprecated_ref** | [**DeprecatedObject**](DeprecatedObject.md) |  | [optional] |
| **bars** | **Array&lt;String&gt;** |  | [optional] |

## Example

```ruby
require 'petstore'

instance = Petstore::ObjectWithDeprecatedFields.new(
  uuid: null,
  id: null,
  deprecated_ref: null,
  bars: null
)
```

