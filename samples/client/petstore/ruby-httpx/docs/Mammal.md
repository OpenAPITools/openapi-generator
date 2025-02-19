# Petstore::Mammal

## Class instance methods

### `openapi_one_of`

Returns the list of classes defined in oneOf.

#### Example

```ruby
require 'petstore'

Petstore::Mammal.openapi_one_of
# =>
# [
#   :'Whale',
#   :'Zebra'
# ]
```

### `openapi_discriminator_name`

Returns the discriminator's property name.

#### Example

```ruby
require 'petstore'

Petstore::Mammal.openapi_discriminator_name
# => :'classname'
```
```

### build

Find the appropriate object from the `openapi_one_of` list and casts the data into it.

#### Example

```ruby
require 'petstore'

Petstore::Mammal.build(data)
# => #<Whale:0x00007fdd4aab02a0>

Petstore::Mammal.build(data_that_doesnt_match)
# => nil
```

#### Parameters

| Name | Type | Description |
| ---- | ---- | ----------- |
| **data** | **Mixed** | data to be matched against the list of oneOf items |

#### Return type

- `Whale`
- `Zebra`
- `nil` (if no type matches)

