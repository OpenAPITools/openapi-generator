# Petstore::MamalWithEnum

## Class instance methods

### `openapi_one_of`

Returns the list of classes defined in oneOf.

#### Example

```ruby
require 'petstore'

Petstore::MamalWithEnum.openapi_one_of
# =>
# [
#   :'Cow'
# ]
```

### build

Find the appropriate object from the `openapi_one_of` list and casts the data into it.

#### Example

```ruby
require 'petstore'

Petstore::MamalWithEnum.build(data)
# => #<Cow:0x00007fdd4aab02a0>

Petstore::MamalWithEnum.build(data_that_doesnt_match)
# => nil
```

#### Parameters

| Name | Type | Description |
| ---- | ---- | ----------- |
| **data** | **Mixed** | data to be matched against the list of oneOf items |

#### Return type

- `Cow`
- `nil` (if no type matches)

