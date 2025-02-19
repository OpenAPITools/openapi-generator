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

### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::UsageApi.new
opts = {
  array_alias: Petstore::ArrayAlias.new # ArrayAlias | 
}

begin
  # Use alias to array
  result = api_instance.array(opts)
  p result
rescue Petstore::ApiError => e
  puts "Error when calling UsageApi->array: #{e}"
end
```

#### Using the array_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(Object, Integer, Hash)> array_with_http_info(opts)

```ruby
begin
  # Use alias to array
  data, status_code, headers = api_instance.array_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => Object
rescue Petstore::ApiError => e
  puts "Error when calling UsageApi->array_with_http_info: #{e}"
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

### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::UsageApi.new
opts = {
  map_alias: Petstore::MapAlias.new # MapAlias | 
}

begin
  # Use alias to map
  result = api_instance.map(opts)
  p result
rescue Petstore::ApiError => e
  puts "Error when calling UsageApi->map: #{e}"
end
```

#### Using the map_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(Object, Integer, Hash)> map_with_http_info(opts)

```ruby
begin
  # Use alias to map
  data, status_code, headers = api_instance.map_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => Object
rescue Petstore::ApiError => e
  puts "Error when calling UsageApi->map_with_http_info: #{e}"
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

