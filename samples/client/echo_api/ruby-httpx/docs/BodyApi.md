# OpenapiClient::BodyApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------ | ------------ | ----------- |
| [**test_binary_gif**](BodyApi.md#test_binary_gif) | **POST** /binary/gif | Test binary (gif) response body |
| [**test_body_application_octetstream_binary**](BodyApi.md#test_body_application_octetstream_binary) | **POST** /body/application/octetstream/binary | Test body parameter(s) |
| [**test_body_multipart_formdata_array_of_binary**](BodyApi.md#test_body_multipart_formdata_array_of_binary) | **POST** /body/application/octetstream/array_of_binary | Test array of binary in multipart mime |
| [**test_echo_body_free_form_object_response_string**](BodyApi.md#test_echo_body_free_form_object_response_string) | **POST** /echo/body/FreeFormObject/response_string | Test free form object |
| [**test_echo_body_pet**](BodyApi.md#test_echo_body_pet) | **POST** /echo/body/Pet | Test body parameter(s) |
| [**test_echo_body_pet_response_string**](BodyApi.md#test_echo_body_pet_response_string) | **POST** /echo/body/Pet/response_string | Test empty response body |
| [**test_echo_body_tag_response_string**](BodyApi.md#test_echo_body_tag_response_string) | **POST** /echo/body/Tag/response_string | Test empty json (request body) |


## test_binary_gif

> File test_binary_gif

Test binary (gif) response body

Test binary (gif) response body

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::BodyApi.new

begin
  # Test binary (gif) response body
  result = api_instance.test_binary_gif
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling BodyApi->test_binary_gif: #{e}"
end
```

#### Using the test_binary_gif_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(File, Integer, Hash)> test_binary_gif_with_http_info

```ruby
begin
  # Test binary (gif) response body
  data, status_code, headers = api_instance.test_binary_gif_with_http_info
  p status_code # => 2xx
  p headers # => { ... }
  p data # => File
rescue OpenapiClient::ApiError => e
  puts "Error when calling BodyApi->test_binary_gif_with_http_info: #{e}"
end
```

### Parameters

This endpoint does not need any parameter.

### Return type

**File**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: image/gif


## test_body_application_octetstream_binary

> String test_body_application_octetstream_binary(opts)

Test body parameter(s)

Test body parameter(s)

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::BodyApi.new
opts = {
  body: File.new('/path/to/some/file') # File | 
}

begin
  # Test body parameter(s)
  result = api_instance.test_body_application_octetstream_binary(opts)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling BodyApi->test_body_application_octetstream_binary: #{e}"
end
```

#### Using the test_body_application_octetstream_binary_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_body_application_octetstream_binary_with_http_info(opts)

```ruby
begin
  # Test body parameter(s)
  data, status_code, headers = api_instance.test_body_application_octetstream_binary_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling BodyApi->test_body_application_octetstream_binary_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **body** | **File** |  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/octet-stream
- **Accept**: text/plain


## test_body_multipart_formdata_array_of_binary

> String test_body_multipart_formdata_array_of_binary(files)

Test array of binary in multipart mime

Test array of binary in multipart mime

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::BodyApi.new
files = [File.new('/path/to/some/file')] # Array<File> | 

begin
  # Test array of binary in multipart mime
  result = api_instance.test_body_multipart_formdata_array_of_binary(files)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling BodyApi->test_body_multipart_formdata_array_of_binary: #{e}"
end
```

#### Using the test_body_multipart_formdata_array_of_binary_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_body_multipart_formdata_array_of_binary_with_http_info(files)

```ruby
begin
  # Test array of binary in multipart mime
  data, status_code, headers = api_instance.test_body_multipart_formdata_array_of_binary_with_http_info(files)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling BodyApi->test_body_multipart_formdata_array_of_binary_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **files** | **Array&lt;File&gt;** |  |  |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: text/plain


## test_echo_body_free_form_object_response_string

> String test_echo_body_free_form_object_response_string(opts)

Test free form object

Test free form object

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::BodyApi.new
opts = {
  body: { ... } # Object | Free form object
}

begin
  # Test free form object
  result = api_instance.test_echo_body_free_form_object_response_string(opts)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling BodyApi->test_echo_body_free_form_object_response_string: #{e}"
end
```

#### Using the test_echo_body_free_form_object_response_string_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_echo_body_free_form_object_response_string_with_http_info(opts)

```ruby
begin
  # Test free form object
  data, status_code, headers = api_instance.test_echo_body_free_form_object_response_string_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling BodyApi->test_echo_body_free_form_object_response_string_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **body** | **Object** | Free form object | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: text/plain


## test_echo_body_pet

> <Pet> test_echo_body_pet(opts)

Test body parameter(s)

Test body parameter(s)

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::BodyApi.new
opts = {
  pet: OpenapiClient::Pet.new({name: 'doggie', photo_urls: ['photo_urls_example']}) # Pet | Pet object that needs to be added to the store
}

begin
  # Test body parameter(s)
  result = api_instance.test_echo_body_pet(opts)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling BodyApi->test_echo_body_pet: #{e}"
end
```

#### Using the test_echo_body_pet_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(<Pet>, Integer, Hash)> test_echo_body_pet_with_http_info(opts)

```ruby
begin
  # Test body parameter(s)
  data, status_code, headers = api_instance.test_echo_body_pet_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => <Pet>
rescue OpenapiClient::ApiError => e
  puts "Error when calling BodyApi->test_echo_body_pet_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **pet** | [**Pet**](Pet.md) | Pet object that needs to be added to the store | [optional] |

### Return type

[**Pet**](Pet.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## test_echo_body_pet_response_string

> String test_echo_body_pet_response_string(opts)

Test empty response body

Test empty response body

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::BodyApi.new
opts = {
  pet: OpenapiClient::Pet.new({name: 'doggie', photo_urls: ['photo_urls_example']}) # Pet | Pet object that needs to be added to the store
}

begin
  # Test empty response body
  result = api_instance.test_echo_body_pet_response_string(opts)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling BodyApi->test_echo_body_pet_response_string: #{e}"
end
```

#### Using the test_echo_body_pet_response_string_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_echo_body_pet_response_string_with_http_info(opts)

```ruby
begin
  # Test empty response body
  data, status_code, headers = api_instance.test_echo_body_pet_response_string_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling BodyApi->test_echo_body_pet_response_string_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **pet** | [**Pet**](Pet.md) | Pet object that needs to be added to the store | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: text/plain


## test_echo_body_tag_response_string

> String test_echo_body_tag_response_string(opts)

Test empty json (request body)

Test empty json (request body)

### Examples

```ruby
require 'time'
require 'openapi_client'

api_instance = OpenapiClient::BodyApi.new
opts = {
  tag: OpenapiClient::Tag.new # Tag | Tag object
}

begin
  # Test empty json (request body)
  result = api_instance.test_echo_body_tag_response_string(opts)
  p result
rescue OpenapiClient::ApiError => e
  puts "Error when calling BodyApi->test_echo_body_tag_response_string: #{e}"
end
```

#### Using the test_echo_body_tag_response_string_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> test_echo_body_tag_response_string_with_http_info(opts)

```ruby
begin
  # Test empty json (request body)
  data, status_code, headers = api_instance.test_echo_body_tag_response_string_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue OpenapiClient::ApiError => e
  puts "Error when calling BodyApi->test_echo_body_tag_response_string_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **tag** | [**Tag**](Tag.md) | Tag object | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: text/plain

