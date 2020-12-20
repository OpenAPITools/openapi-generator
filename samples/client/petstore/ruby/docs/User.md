# Petstore::User

## Properties

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **id** | **Integer** |  | [optional] |
| **username** | **String** |  | [optional] |
| **first_name** | **String** |  | [optional] |
| **last_name** | **String** |  | [optional] |
| **email** | **String** |  | [optional] |
| **password** | **String** |  | [optional] |
| **phone** | **String** |  | [optional] |
| **user_status** | **Integer** | User Status | [optional] |

## Example

```ruby
require 'petstore'

instance = Petstore::User.new(
  id: null,
  username: null,
  first_name: null,
  last_name: null,
  email: null,
  password: null,
  phone: null,
  user_status: null
)
```

