# XAuthIDAlias::UsageApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
| ------ | ------------ | ----------- |
| [**any_key**](UsageApi.md#any_key) | **GET** /any | Use any API key |
| [**both_keys**](UsageApi.md#both_keys) | **GET** /both | Use both API keys |
| [**key_in_header**](UsageApi.md#key_in_header) | **GET** /header | Use API key in header |
| [**key_in_query**](UsageApi.md#key_in_query) | **GET** /query | Use API key in query |


## any_key

> Object any_key

Use any API key

Use any API key

### Examples

```ruby
require 'time'
require 'x_auth_id_alias'
# setup authorization
XAuthIDAlias.configure do |config|
  # Configure API key authorization: api_key
  config.api_key['api_key'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'Bearer' (defaults to nil)
  # config.api_key_prefix['api_key'] = 'Bearer'

  # Configure API key authorization: api_key_query
  config.api_key['api_key_query'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'Bearer' (defaults to nil)
  # config.api_key_prefix['api_key_query'] = 'Bearer'
end

api_instance = XAuthIDAlias::UsageApi.new

begin
  # Use any API key
  result = api_instance.any_key
  p result
rescue XAuthIDAlias::ApiError => e
  puts "Error when calling UsageApi->any_key: #{e}"
end
```

#### Using the any_key_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(Object, Integer, Hash)> any_key_with_http_info

```ruby
begin
  # Use any API key
  data, status_code, headers = api_instance.any_key_with_http_info
  p status_code # => 2xx
  p headers # => { ... }
  p data # => Object
rescue XAuthIDAlias::ApiError => e
  puts "Error when calling UsageApi->any_key_with_http_info: #{e}"
end
```

### Parameters

This endpoint does not need any parameter.

### Return type

**Object**

### Authorization

[api_key](../README.md#api_key), [api_key_query](../README.md#api_key_query)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## both_keys

> Object both_keys

Use both API keys

Use both API keys

### Examples

```ruby
require 'time'
require 'x_auth_id_alias'
# setup authorization
XAuthIDAlias.configure do |config|
  # Configure API key authorization: api_key
  config.api_key['api_key'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'Bearer' (defaults to nil)
  # config.api_key_prefix['api_key'] = 'Bearer'

  # Configure API key authorization: api_key_query
  config.api_key['api_key_query'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'Bearer' (defaults to nil)
  # config.api_key_prefix['api_key_query'] = 'Bearer'
end

api_instance = XAuthIDAlias::UsageApi.new

begin
  # Use both API keys
  result = api_instance.both_keys
  p result
rescue XAuthIDAlias::ApiError => e
  puts "Error when calling UsageApi->both_keys: #{e}"
end
```

#### Using the both_keys_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(Object, Integer, Hash)> both_keys_with_http_info

```ruby
begin
  # Use both API keys
  data, status_code, headers = api_instance.both_keys_with_http_info
  p status_code # => 2xx
  p headers # => { ... }
  p data # => Object
rescue XAuthIDAlias::ApiError => e
  puts "Error when calling UsageApi->both_keys_with_http_info: #{e}"
end
```

### Parameters

This endpoint does not need any parameter.

### Return type

**Object**

### Authorization

[api_key](../README.md#api_key), [api_key_query](../README.md#api_key_query)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## key_in_header

> Object key_in_header

Use API key in header

Use API key in header

### Examples

```ruby
require 'time'
require 'x_auth_id_alias'
# setup authorization
XAuthIDAlias.configure do |config|
  # Configure API key authorization: api_key
  config.api_key['api_key'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'Bearer' (defaults to nil)
  # config.api_key_prefix['api_key'] = 'Bearer'
end

api_instance = XAuthIDAlias::UsageApi.new

begin
  # Use API key in header
  result = api_instance.key_in_header
  p result
rescue XAuthIDAlias::ApiError => e
  puts "Error when calling UsageApi->key_in_header: #{e}"
end
```

#### Using the key_in_header_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(Object, Integer, Hash)> key_in_header_with_http_info

```ruby
begin
  # Use API key in header
  data, status_code, headers = api_instance.key_in_header_with_http_info
  p status_code # => 2xx
  p headers # => { ... }
  p data # => Object
rescue XAuthIDAlias::ApiError => e
  puts "Error when calling UsageApi->key_in_header_with_http_info: #{e}"
end
```

### Parameters

This endpoint does not need any parameter.

### Return type

**Object**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## key_in_query

> Object key_in_query

Use API key in query

Use API key in query

### Examples

```ruby
require 'time'
require 'x_auth_id_alias'
# setup authorization
XAuthIDAlias.configure do |config|
  # Configure API key authorization: api_key_query
  config.api_key['api_key_query'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'Bearer' (defaults to nil)
  # config.api_key_prefix['api_key_query'] = 'Bearer'
end

api_instance = XAuthIDAlias::UsageApi.new

begin
  # Use API key in query
  result = api_instance.key_in_query
  p result
rescue XAuthIDAlias::ApiError => e
  puts "Error when calling UsageApi->key_in_query: #{e}"
end
```

#### Using the key_in_query_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(Object, Integer, Hash)> key_in_query_with_http_info

```ruby
begin
  # Use API key in query
  data, status_code, headers = api_instance.key_in_query_with_http_info
  p status_code # => 2xx
  p headers # => { ... }
  p data # => Object
rescue XAuthIDAlias::ApiError => e
  puts "Error when calling UsageApi->key_in_query_with_http_info: #{e}"
end
```

### Parameters

This endpoint does not need any parameter.

### Return type

**Object**

### Authorization

[api_key_query](../README.md#api_key_query)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

