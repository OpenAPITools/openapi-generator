# Petstore::NullableClass

## Properties

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **integer_prop** | **Integer** |  | [optional] |
| **number_prop** | **Float** |  | [optional] |
| **boolean_prop** | **Boolean** |  | [optional] |
| **string_prop** | **String** |  | [optional] |
| **date_prop** | **Date** |  | [optional] |
| **datetime_prop** | **Time** |  | [optional] |
| **array_nullable_prop** | **Array&lt;Object&gt;** |  | [optional] |
| **array_and_items_nullable_prop** | **Array&lt;Object&gt;** |  | [optional] |
| **array_items_nullable** | **Array&lt;Object&gt;** |  | [optional] |
| **object_nullable_prop** | **Hash&lt;String, Object&gt;** |  | [optional] |
| **object_and_items_nullable_prop** | **Hash&lt;String, Object&gt;** |  | [optional] |
| **object_items_nullable** | **Hash&lt;String, Object&gt;** |  | [optional] |

## Example

```ruby
require 'petstore'

instance = Petstore::NullableClass.new(
  integer_prop: null,
  number_prop: null,
  boolean_prop: null,
  string_prop: null,
  date_prop: null,
  datetime_prop: null,
  array_nullable_prop: null,
  array_and_items_nullable_prop: null,
  array_items_nullable: null,
  object_nullable_prop: null,
  object_and_items_nullable_prop: null,
  object_items_nullable: null
)
```

