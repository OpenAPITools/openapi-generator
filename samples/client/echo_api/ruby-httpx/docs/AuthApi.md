# OpenapiClient::AuthApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------ | ------------ | ----------- |
| [**test_auth_http_basic**](AuthApi.md#test_auth_http_basic) | **POST** /auth/http/basic | To test HTTP basic authentication |


## test_auth_http_basic

> String test_auth_http_basic

To test HTTP basic authentication

To test HTTP basic authentication

### Examples

```ruby
require 'time'
require 'openapi_client'
# setup authorization
OpenapiClient.configure do |config|
  # Configure HTTP basic authorization: http_auth
  config.username = 'YOUR USERNAME'
  config.password = 'YOUR PASSWORD'
end

api_instance = OpenapiClient::AuthApi.new

begin
  # To test HTTP basic authentication
  result = api_instance.test_auth_http_basic
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling AuthApi->test_auth_http_basic: #{e}"
end
```

#### Using the test_auth_http_basic_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_auth_http_basic_with_http_info

```ruby
begin
  # To test HTTP basic authentication
  data, status_code, headers = api_instance.test_auth_http_basic_with_http_info
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling AuthApi->test_auth_http_basic_with_http_info: #{e}"
end
```

### Parameters

This endpoint does not need any parameter.

### Return type

**String**

### Authorization

[http_auth](../README.md#http_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain

