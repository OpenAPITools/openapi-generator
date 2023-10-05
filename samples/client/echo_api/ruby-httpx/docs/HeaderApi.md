# OpenapiClient::HeaderApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------ | ------------ | ----------- |
| [**test_header_integer_boolean_string**](HeaderApi.md#test_header_integer_boolean_string) | **GET** /header/integer/boolean/string | Test header parameter(s) |


## test_header_integer_boolean_string

> String test_header_integer_boolean_string(opts)

Test header parameter(s)

Test header parameter(s)

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::HeaderApi.new
opts = {
  integer_header: 56, # Integer | 
  boolean_header: true, # Boolean | 
  string_header: 'string_header_example' # String | 
}

begin
  # Test header parameter(s)
  result = api_instance.test_header_integer_boolean_string(opts)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling HeaderApi->test_header_integer_boolean_string: #{e}"
end
```

#### Using the test_header_integer_boolean_string_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_header_integer_boolean_string_with_http_info(opts)

```ruby
begin
  # Test header parameter(s)
  data, status_code, headers = api_instance.test_header_integer_boolean_string_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling HeaderApi->test_header_integer_boolean_string_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **integer_header** | **Integer** |  | [optional] |
| **boolean_header** | **Boolean** |  | [optional] |
| **string_header** | **String** |  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain

