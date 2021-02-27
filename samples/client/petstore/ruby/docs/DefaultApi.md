# Petstore::DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
| ------ | ------------ | ----------- |
| [**foo_get**](DefaultApi.md#foo_get) | **GET** /foo |  |


## foo_get

> <InlineResponseDefault> foo_get



### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::DefaultApi.new

begin
  
  result = api_instance.foo_get
  p result
rescue Petstore::ApiError => e
  puts "Error when calling DefaultApi->foo_get: #{e}"
end
```

#### Using the foo_get_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(<InlineResponseDefault>, Integer, Hash)> foo_get_with_http_info

```ruby
begin
  
  data, status_code, headers = api_instance.foo_get_with_http_info
  p status_code # => 2xx
  p headers # => { ... }
  p data # => <InlineResponseDefault>
rescue Petstore::ApiError => e
  puts "Error when calling DefaultApi->foo_get_with_http_info: #{e}"
end
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**InlineResponseDefault**](InlineResponseDefault.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

