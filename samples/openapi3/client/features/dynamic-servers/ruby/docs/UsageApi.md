# DynamicServers::UsageApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**custom_server**](UsageApi.md#custom_server) | **GET** /custom | Use custom server
[**default_server**](UsageApi.md#default_server) | **GET** /default | Use default server



## custom_server

> Object custom_server

Use custom server

Use custom server

### Example

```ruby
# load the gem
require 'dynamic_servers'

api_instance = DynamicServers::UsageApi.new

begin
  #Use custom server
  result = api_instance.custom_server
  p result
rescue DynamicServers::ApiError => e
  puts "Exception when calling UsageApi->custom_server: #{e}"
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

### Example

```ruby
# load the gem
require 'dynamic_servers'

api_instance = DynamicServers::UsageApi.new

begin
  #Use default server
  result = api_instance.default_server
  p result
rescue DynamicServers::ApiError => e
  puts "Exception when calling UsageApi->default_server: #{e}"
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

