# OpenapiClient::Pet

## Properties

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **id** | **Integer** |  | [optional] |
| **name** | **String** |  |  |
| **category** | [**Category**](Category.md) |  | [optional] |
| **photo_urls** | **Array&lt;String&gt;** |  |  |
| **tags** | [**Array&lt;Tag&gt;**](Tag.md) |  | [optional] |
| **status** | **String** | pet status in the store | [optional] |

## Example

```ruby
require 'openapi_client'

instance = OpenapiClient::Pet.new(
  id: 10,
  name: doggie,
  category: null,
  photo_urls: null,
  tags: null,
  status: null
)
```

