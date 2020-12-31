# DynamicServers::UsageApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
| ------ | ------------ | ----------- |
| [**custom_server**](UsageApi.md#custom_server) | **GET** /custom | Use custom server |
| [**default_server**](UsageApi.md#default_server) | **GET** /default | Use default server |


## custom_server

> Object custom_server

Use custom server

Use custom server

### Examples

```ruby
require 'time'
require 'dynamic_servers'

api_instance = DynamicServers::UsageApi.new

begin
  # Use custom server
  result = api_instance.custom_server
  p result
rescue DynamicServers::ApiError => e
  puts "Error when calling UsageApi->custom_server: #{e}"
end
```

#### Using the custom_server_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(Object, Integer, Hash)> custom_server_with_http_info

```ruby
begin
  # Use custom server
  data, status_code, headers = api_instance.custom_server_with_http_info
  p status_code # => 2xx
  p headers # => { ... }
  p data # => Object
rescue DynamicServers::ApiError => e
  puts "Error when calling UsageApi->custom_server_with_http_info: #{e}"
end
```

### Parameters

This endpoint does not need any parameter.

### Return type

**Object**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## default_server

> Object default_server

Use default server

Use default server

### Examples

```ruby
require 'time'
require 'dynamic_servers'

api_instance = DynamicServers::UsageApi.new

begin
  # Use default server
  result = api_instance.default_server
  p result
rescue DynamicServers::ApiError => e
  puts "Error when calling UsageApi->default_server: #{e}"
end
```

#### Using the default_server_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(Object, Integer, Hash)> default_server_with_http_info

```ruby
begin
  # Use default server
  data, status_code, headers = api_instance.default_server_with_http_info
  p status_code # => 2xx
  p headers # => { ... }
  p data # => Object
rescue DynamicServers::ApiError => e
  puts "Error when calling UsageApi->default_server_with_http_info: #{e}"
end
```

### Parameters

This endpoint does not need any parameter.

### Return type

**Object**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

