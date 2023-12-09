# OpenapiClient::DefaultValue

## Properties

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **array_string_enum_ref_default** | [**Array&lt;StringEnumRef&gt;**](StringEnumRef.md) |  | [optional] |
| **array_string_enum_default** | **Array&lt;String&gt;** |  | [optional] |
| **array_string_default** | **Array&lt;String&gt;** |  | [optional] |
| **array_integer_default** | **Array&lt;Integer&gt;** |  | [optional] |
| **array_string** | **Array&lt;String&gt;** |  | [optional] |
| **array_string_nullable** | **Array&lt;String&gt;** |  | [optional] |
| **array_string_extension_nullable** | **Array&lt;String&gt;** |  | [optional] |
| **string_nullable** | **String** |  | [optional] |

## Example

```ruby
require 'openapi_client'

instance = OpenapiClient::DefaultValue.new(
  array_string_enum_ref_default: null,
  array_string_enum_default: null,
  array_string_default: null,
  array_integer_default: null,
  array_string: null,
  array_string_nullable: null,
  array_string_extension_nullable: null,
  string_nullable: null
)
```

