# Petstore::InlineObject2

## Properties

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **enum_form_string_array** | **Array&lt;String&gt;** | Form parameter enum test (string array) | [optional] |
| **enum_form_string** | **String** | Form parameter enum test (string) | [optional][default to &#39;-efg&#39;] |

## Example

```ruby
require 'petstore'

instance = Petstore::InlineObject2.new(
  enum_form_string_array: null,
  enum_form_string: null
)
```

