# Petstore::UsageApi

All URIs are relative to *http://petstore.swagger.io/v2*

| Method | HTTP request | Description |
| ------ | ------------ | ----------- |
| [**array**](UsageApi.md#array) | **GET** /array | Use alias to array |
| [**map**](UsageApi.md#map) | **GET** /map | Use alias to map |


## array

> Object array(opts)

Use alias to array

Use alias to array

### Example

```ruby
# load the gem
require 'petstore'

api_instance = Petstore::UsageApi.new
opts = {
  array_alias: Petstore::ArrayAlias.new # ArrayAlias | 
}

begin
  #Use alias to array
  result = api_instance.array(opts)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling UsageApi->array: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **array_alias** | [**ArrayAlias**](ArrayAlias.md) |  | [optional] |

### Return type

**Object**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## map

> Object map(opts)

Use alias to map

Use alias to map

### Example

```ruby
# load the gem
require 'petstore'

api_instance = Petstore::UsageApi.new
opts = {
  map_alias: Petstore::MapAlias.new # MapAlias | 
}

begin
  #Use alias to map
  result = api_instance.map(opts)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling UsageApi->map: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **map_alias** | [**MapAlias**](MapAlias.md) |  | [optional] |

### Return type

**Object**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

