# Petstore::ArrayTest

## Properties

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **array_of_string** | **Array&lt;String&gt;** |  | [optional] |
| **array_array_of_integer** | **Array&lt;Array&lt;Integer&gt;&gt;** |  | [optional] |
| **array_array_of_model** | **Array&lt;Array&lt;ReadOnlyFirst&gt;&gt;** |  | [optional] |

## Example

```ruby
require 'petstore'

instance = Petstore::ArrayTest.new(
  array_of_string: null,
  array_array_of_integer: null,
  array_array_of_model: null
)
```

