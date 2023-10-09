# OpenapiClient::QueryApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------ | ------------ | ----------- |
| [**test_enum_ref_string**](QueryApi.md#test_enum_ref_string) | **GET** /query/enum_ref_string | Test query parameter(s) |
| [**test_query_datetime_date_string**](QueryApi.md#test_query_datetime_date_string) | **GET** /query/datetime/date/string | Test query parameter(s) |
| [**test_query_integer_boolean_string**](QueryApi.md#test_query_integer_boolean_string) | **GET** /query/integer/boolean/string | Test query parameter(s) |
| [**test_query_style_deep_object_explode_true_object**](QueryApi.md#test_query_style_deep_object_explode_true_object) | **GET** /query/style_deepObject/explode_true/object | Test query parameter(s) |
| [**test_query_style_deep_object_explode_true_object_all_of**](QueryApi.md#test_query_style_deep_object_explode_true_object_all_of) | **GET** /query/style_deepObject/explode_true/object/allOf | Test query parameter(s) |
| [**test_query_style_form_explode_true_array_string**](QueryApi.md#test_query_style_form_explode_true_array_string) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s) |
| [**test_query_style_form_explode_true_object**](QueryApi.md#test_query_style_form_explode_true_object) | **GET** /query/style_form/explode_true/object | Test query parameter(s) |
| [**test_query_style_form_explode_true_object_all_of**](QueryApi.md#test_query_style_form_explode_true_object_all_of) | **GET** /query/style_form/explode_true/object/allOf | Test query parameter(s) |


## test_enum_ref_string

> String test_enum_ref_string(opts)

Test query parameter(s)

Test query parameter(s)

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::QueryApi.new
opts = {
  enum_ref_string_query: OpenapiClient::StringEnumRef::SUCCESS # StringEnumRef | 
}

begin
  # Test query parameter(s)
  result = api_instance.test_enum_ref_string(opts)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling QueryApi->test_enum_ref_string: #{e}"
end
```

#### Using the test_enum_ref_string_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_enum_ref_string_with_http_info(opts)

```ruby
begin
  # Test query parameter(s)
  data, status_code, headers = api_instance.test_enum_ref_string_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling QueryApi->test_enum_ref_string_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **enum_ref_string_query** | [**StringEnumRef**](.md) |  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain


## test_query_datetime_date_string

> String test_query_datetime_date_string(opts)

Test query parameter(s)

Test query parameter(s)

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::QueryApi.new
opts = {
  datetime_query: Time.parse('2013-10-20T19:20:30+01:00'), # Time | 
  date_query: Date.parse('2013-10-20'), # Date | 
  string_query: 'string_query_example' # String | 
}

begin
  # Test query parameter(s)
  result = api_instance.test_query_datetime_date_string(opts)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling QueryApi->test_query_datetime_date_string: #{e}"
end
```

#### Using the test_query_datetime_date_string_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_query_datetime_date_string_with_http_info(opts)

```ruby
begin
  # Test query parameter(s)
  data, status_code, headers = api_instance.test_query_datetime_date_string_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling QueryApi->test_query_datetime_date_string_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **datetime_query** | **Time** |  | [optional] |
| **date_query** | **Date** |  | [optional] |
| **string_query** | **String** |  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain


## test_query_integer_boolean_string

> String test_query_integer_boolean_string(opts)

Test query parameter(s)

Test query parameter(s)

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::QueryApi.new
opts = {
  integer_query: 56, # Integer | 
  boolean_query: true, # Boolean | 
  string_query: 'string_query_example' # String | 
}

begin
  # Test query parameter(s)
  result = api_instance.test_query_integer_boolean_string(opts)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling QueryApi->test_query_integer_boolean_string: #{e}"
end
```

#### Using the test_query_integer_boolean_string_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_query_integer_boolean_string_with_http_info(opts)

```ruby
begin
  # Test query parameter(s)
  data, status_code, headers = api_instance.test_query_integer_boolean_string_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling QueryApi->test_query_integer_boolean_string_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **integer_query** | **Integer** |  | [optional] |
| **boolean_query** | **Boolean** |  | [optional] |
| **string_query** | **String** |  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain


## test_query_style_deep_object_explode_true_object

> String test_query_style_deep_object_explode_true_object(opts)

Test query parameter(s)

Test query parameter(s)

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::QueryApi.new
opts = {
  query_object: OpenapiClient::Pet.new({name: 'doggie', photo_urls: ['photo_urls_example']}) # Pet | 
}

begin
  # Test query parameter(s)
  result = api_instance.test_query_style_deep_object_explode_true_object(opts)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling QueryApi->test_query_style_deep_object_explode_true_object: #{e}"
end
```

#### Using the test_query_style_deep_object_explode_true_object_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_query_style_deep_object_explode_true_object_with_http_info(opts)

```ruby
begin
  # Test query parameter(s)
  data, status_code, headers = api_instance.test_query_style_deep_object_explode_true_object_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling QueryApi->test_query_style_deep_object_explode_true_object_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **query_object** | [**Pet**](.md) |  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain


## test_query_style_deep_object_explode_true_object_all_of

> String test_query_style_deep_object_explode_true_object_all_of(opts)

Test query parameter(s)

Test query parameter(s)

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::QueryApi.new
opts = {
  query_object: OpenapiClient::TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter.new # TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter | 
}

begin
  # Test query parameter(s)
  result = api_instance.test_query_style_deep_object_explode_true_object_all_of(opts)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling QueryApi->test_query_style_deep_object_explode_true_object_all_of: #{e}"
end
```

#### Using the test_query_style_deep_object_explode_true_object_all_of_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_query_style_deep_object_explode_true_object_all_of_with_http_info(opts)

```ruby
begin
  # Test query parameter(s)
  data, status_code, headers = api_instance.test_query_style_deep_object_explode_true_object_all_of_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling QueryApi->test_query_style_deep_object_explode_true_object_all_of_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **query_object** | [**TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter**](.md) |  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain


## test_query_style_form_explode_true_array_string

> String test_query_style_form_explode_true_array_string(opts)

Test query parameter(s)

Test query parameter(s)

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::QueryApi.new
opts = {
  query_object: OpenapiClient::TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter.new # TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter | 
}

begin
  # Test query parameter(s)
  result = api_instance.test_query_style_form_explode_true_array_string(opts)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling QueryApi->test_query_style_form_explode_true_array_string: #{e}"
end
```

#### Using the test_query_style_form_explode_true_array_string_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_query_style_form_explode_true_array_string_with_http_info(opts)

```ruby
begin
  # Test query parameter(s)
  data, status_code, headers = api_instance.test_query_style_form_explode_true_array_string_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling QueryApi->test_query_style_form_explode_true_array_string_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **query_object** | [**TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter**](.md) |  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain


## test_query_style_form_explode_true_object

> String test_query_style_form_explode_true_object(opts)

Test query parameter(s)

Test query parameter(s)

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::QueryApi.new
opts = {
  query_object: OpenapiClient::Pet.new({name: 'doggie', photo_urls: ['photo_urls_example']}) # Pet | 
}

begin
  # Test query parameter(s)
  result = api_instance.test_query_style_form_explode_true_object(opts)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling QueryApi->test_query_style_form_explode_true_object: #{e}"
end
```

#### Using the test_query_style_form_explode_true_object_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_query_style_form_explode_true_object_with_http_info(opts)

```ruby
begin
  # Test query parameter(s)
  data, status_code, headers = api_instance.test_query_style_form_explode_true_object_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling QueryApi->test_query_style_form_explode_true_object_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **query_object** | [**Pet**](.md) |  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain


## test_query_style_form_explode_true_object_all_of

> String test_query_style_form_explode_true_object_all_of(opts)

Test query parameter(s)

Test query parameter(s)

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::QueryApi.new
opts = {
  query_object: OpenapiClient::DataQuery.new # DataQuery | 
}

begin
  # Test query parameter(s)
  result = api_instance.test_query_style_form_explode_true_object_all_of(opts)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling QueryApi->test_query_style_form_explode_true_object_all_of: #{e}"
end
```

#### Using the test_query_style_form_explode_true_object_all_of_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_query_style_form_explode_true_object_all_of_with_http_info(opts)

```ruby
begin
  # Test query parameter(s)
  data, status_code, headers = api_instance.test_query_style_form_explode_true_object_all_of_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling QueryApi->test_query_style_form_explode_true_object_all_of_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **query_object** | [**DataQuery**](.md) |  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain

