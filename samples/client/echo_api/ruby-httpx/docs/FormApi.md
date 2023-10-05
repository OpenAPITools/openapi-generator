# OpenapiClient::FormApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------ | ------------ | ----------- |
| [**test_form_integer_boolean_string**](FormApi.md#test_form_integer_boolean_string) | **POST** /form/integer/boolean/string | Test form parameter(s) |
| [**test_form_oneof**](FormApi.md#test_form_oneof) | **POST** /form/oneof | Test form parameter(s) for oneOf schema |


## test_form_integer_boolean_string

> String test_form_integer_boolean_string(opts)

Test form parameter(s)

Test form parameter(s)

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::FormApi.new
opts = {
  integer_form: 56, # Integer | 
  boolean_form: true, # Boolean | 
  string_form: 'string_form_example' # String | 
}

begin
  # Test form parameter(s)
  result = api_instance.test_form_integer_boolean_string(opts)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling FormApi->test_form_integer_boolean_string: #{e}"
end
```

#### Using the test_form_integer_boolean_string_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_form_integer_boolean_string_with_http_info(opts)

```ruby
begin
  # Test form parameter(s)
  data, status_code, headers = api_instance.test_form_integer_boolean_string_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling FormApi->test_form_integer_boolean_string_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **integer_form** | **Integer** |  | [optional] |
| **boolean_form** | **Boolean** |  | [optional] |
| **string_form** | **String** |  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: text/plain


## test_form_oneof

> String test_form_oneof(opts)

Test form parameter(s) for oneOf schema

Test form parameter(s) for oneOf schema

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::FormApi.new
opts = {
  form1: 'form1_example', # String | 
  form2: 56, # Integer | 
  form3: 'form3_example', # String | 
  form4: true, # Boolean | 
  id: 789, # Integer | 
  name: 'name_example' # String | 
}

begin
  # Test form parameter(s) for oneOf schema
  result = api_instance.test_form_oneof(opts)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling FormApi->test_form_oneof: #{e}"
end
```

#### Using the test_form_oneof_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_form_oneof_with_http_info(opts)

```ruby
begin
  # Test form parameter(s) for oneOf schema
  data, status_code, headers = api_instance.test_form_oneof_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling FormApi->test_form_oneof_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **form1** | **String** |  | [optional] |
| **form2** | **Integer** |  | [optional] |
| **form3** | **String** |  | [optional] |
| **form4** | **Boolean** |  | [optional] |
| **id** | **Integer** |  | [optional] |
| **name** | **String** |  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: text/plain

