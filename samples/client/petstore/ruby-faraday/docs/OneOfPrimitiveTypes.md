# Petstore::OneOfPrimitiveTypes

## Class instance methods

### `openapi_one_of`

Returns the list of classes defined in oneOf.

#### Example

```ruby
require 'petstore'

Petstore::OneOfPrimitiveTypes.openapi_one_of
# =>
# [
#   :'Array<Boolean>',
#   :'Array<Date>',
#   :'Array<Float>',
#   :'Array<Integer>',
#   :'Array<String>',
#   :'Array<Time>',
#   :'Boolean',
#   :'Date',
#   :'Float',
#   :'Integer',
#   :'String',
#   :'Time'
# ]
```

### build

Find the appropriate object from the `openapi_one_of` list and casts the data into it.

#### Example

```ruby
require 'petstore'

Petstore::OneOfPrimitiveTypes.build(data)
# => #<Array<Boolean>:0x00007fdd4aab02a0>

Petstore::OneOfPrimitiveTypes.build(data_that_doesnt_match)
# => nil
```

#### Parameters

| Name | Type | Description |
| ---- | ---- | ----------- |
| **data** | **Mixed** | data to be matched against the list of oneOf items |

#### Return type

- `Array<Boolean>`
- `Array<Date>`
- `Array<Float>`
- `Array<Integer>`
- `Array<String>`
- `Array<Time>`
- `Boolean`
- `Date`
- `Float`
- `Integer`
- `String`
- `Time`
- `nil` (if no type matches)

