# OpenapiClient::PathApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------ | ------------ | ----------- |
| [**tests_path_string_path_string_integer_path_integer_enum_nonref_string_path_enum_ref_string_path**](PathApi.md#tests_path_string_path_string_integer_path_integer_enum_nonref_string_path_enum_ref_string_path) | **GET** /path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path} | Test path parameter(s) |


## tests_path_string_path_string_integer_path_integer_enum_nonref_string_path_enum_ref_string_path

> String tests_path_string_path_string_integer_path_integer_enum_nonref_string_path_enum_ref_string_path(path_string, path_integer, enum_nonref_string_path, enum_ref_string_path)

Test path parameter(s)

Test path parameter(s)

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::PathApi.new
path_string = 'path_string_example' # String | 
path_integer = 56 # Integer | 
enum_nonref_string_path = 'success' # String | 
enum_ref_string_path = OpenapiClient::StringEnumRef::SUCCESS # StringEnumRef | 

begin
  # Test path parameter(s)
  result = api_instance.tests_path_string_path_string_integer_path_integer_enum_nonref_string_path_enum_ref_string_path(path_string, path_integer, enum_nonref_string_path, enum_ref_string_path)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling PathApi->tests_path_string_path_string_integer_path_integer_enum_nonref_string_path_enum_ref_string_path: #{e}"
end
```

#### Using the tests_path_string_path_string_integer_path_integer_enum_nonref_string_path_enum_ref_string_path_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> tests_path_string_path_string_integer_path_integer_enum_nonref_string_path_enum_ref_string_path_with_http_info(path_string, path_integer, enum_nonref_string_path, enum_ref_string_path)

```ruby
begin
  # Test path parameter(s)
  data, status_code, headers = api_instance.tests_path_string_path_string_integer_path_integer_enum_nonref_string_path_enum_ref_string_path_with_http_info(path_string, path_integer, enum_nonref_string_path, enum_ref_string_path)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling PathApi->tests_path_string_path_string_integer_path_integer_enum_nonref_string_path_enum_ref_string_path_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **path_string** | **String** |  |  |
| **path_integer** | **Integer** |  |  |
| **enum_nonref_string_path** | **String** |  |  |
| **enum_ref_string_path** | [**StringEnumRef**](.md) |  |  |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain

