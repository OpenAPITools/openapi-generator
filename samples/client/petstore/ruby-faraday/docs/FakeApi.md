# Petstore::FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
| ------ | ------------ | ----------- |
| [**fake_health_get**](FakeApi.md#fake_health_get) | **GET** /fake/health | Health check endpoint |
| [**fake_http_signature_test**](FakeApi.md#fake_http_signature_test) | **GET** /fake/http-signature-test | test http signature authentication |
| [**fake_outer_boolean_serialize**](FakeApi.md#fake_outer_boolean_serialize) | **POST** /fake/outer/boolean |  |
| [**fake_outer_composite_serialize**](FakeApi.md#fake_outer_composite_serialize) | **POST** /fake/outer/composite |  |
| [**fake_outer_number_serialize**](FakeApi.md#fake_outer_number_serialize) | **POST** /fake/outer/number |  |
| [**fake_outer_string_serialize**](FakeApi.md#fake_outer_string_serialize) | **POST** /fake/outer/string |  |
| [**fake_property_enum_integer_serialize**](FakeApi.md#fake_property_enum_integer_serialize) | **POST** /fake/property/enum-int |  |
| [**test_body_with_binary**](FakeApi.md#test_body_with_binary) | **PUT** /fake/body-with-binary |  |
| [**test_body_with_file_schema**](FakeApi.md#test_body_with_file_schema) | **PUT** /fake/body-with-file-schema |  |
| [**test_body_with_query_params**](FakeApi.md#test_body_with_query_params) | **PUT** /fake/body-with-query-params |  |
| [**test_client_model**](FakeApi.md#test_client_model) | **PATCH** /fake | To test \&quot;client\&quot; model |
| [**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트  |
| [**test_enum_parameters**](FakeApi.md#test_enum_parameters) | **GET** /fake | To test enum parameters |
| [**test_group_parameters**](FakeApi.md#test_group_parameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional) |
| [**test_inline_additional_properties**](FakeApi.md#test_inline_additional_properties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties |
| [**test_json_form_data**](FakeApi.md#test_json_form_data) | **GET** /fake/jsonFormData | test json serialization of form data |
| [**test_query_parameter_collection_format**](FakeApi.md#test_query_parameter_collection_format) | **PUT** /fake/test-query-parameters |  |


## fake_health_get

> <HealthCheckResult> fake_health_get

Health check endpoint

### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::FakeApi.new

begin
  # Health check endpoint
  result = api_instance.fake_health_get
  p result
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->fake_health_get: #{e}"
end
```

#### Using the fake_health_get_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(<HealthCheckResult>, Integer, Hash)> fake_health_get_with_http_info

```ruby
begin
  # Health check endpoint
  data, status_code, headers = api_instance.fake_health_get_with_http_info
  p status_code # => 2xx
  p headers # => { ... }
  p data # => <HealthCheckResult>
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->fake_health_get_with_http_info: #{e}"
end
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**HealthCheckResult**](HealthCheckResult.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## fake_http_signature_test

> fake_http_signature_test(pet, opts)

test http signature authentication

### Examples

```ruby
require 'time'
require 'petstore'
# setup authorization
Petstore.configure do |config|
end

api_instance = Petstore::FakeApi.new
pet = Petstore::Pet.new({name: 'doggie', photo_urls: ['photo_urls_example']}) # Pet | Pet object that needs to be added to the store
opts = {
  query_1: 'query_1_example', # String | query parameter
  header_1: 'header_1_example' # String | header parameter
}

begin
  # test http signature authentication
  api_instance.fake_http_signature_test(pet, opts)
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->fake_http_signature_test: #{e}"
end
```

#### Using the fake_http_signature_test_with_http_info variant

This returns an Array which contains the response data (`nil` in this case), status code and headers.

> <Array(nil, Integer, Hash)> fake_http_signature_test_with_http_info(pet, opts)

```ruby
begin
  # test http signature authentication
  data, status_code, headers = api_instance.fake_http_signature_test_with_http_info(pet, opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => nil
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->fake_http_signature_test_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **pet** | [**Pet**](Pet.md) | Pet object that needs to be added to the store |  |
| **query_1** | **String** | query parameter | [optional] |
| **header_1** | **String** | header parameter | [optional] |

### Return type

nil (empty response body)

### Authorization

[http_signature_test](../README.md#http_signature_test)

### HTTP request headers

- **Content-Type**: application/json, application/xml
- **Accept**: Not defined


## fake_outer_boolean_serialize

> Boolean fake_outer_boolean_serialize(opts)



Test serialization of outer boolean types

### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::FakeApi.new
opts = {
  body: true # Boolean | Input boolean as post body
}

begin
  
  result = api_instance.fake_outer_boolean_serialize(opts)
  p result
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->fake_outer_boolean_serialize: #{e}"
end
```

#### Using the fake_outer_boolean_serialize_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(Boolean, Integer, Hash)> fake_outer_boolean_serialize_with_http_info(opts)

```ruby
begin
  
  data, status_code, headers = api_instance.fake_outer_boolean_serialize_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => Boolean
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->fake_outer_boolean_serialize_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **body** | **Boolean** | Input boolean as post body | [optional] |

### Return type

**Boolean**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: */*


## fake_outer_composite_serialize

> <OuterComposite> fake_outer_composite_serialize(opts)



Test serialization of object with outer number type

### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::FakeApi.new
opts = {
  outer_composite: Petstore::OuterComposite.new # OuterComposite | Input composite as post body
}

begin
  
  result = api_instance.fake_outer_composite_serialize(opts)
  p result
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->fake_outer_composite_serialize: #{e}"
end
```

#### Using the fake_outer_composite_serialize_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(<OuterComposite>, Integer, Hash)> fake_outer_composite_serialize_with_http_info(opts)

```ruby
begin
  
  data, status_code, headers = api_instance.fake_outer_composite_serialize_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => <OuterComposite>
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->fake_outer_composite_serialize_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **outer_composite** | [**OuterComposite**](OuterComposite.md) | Input composite as post body | [optional] |

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: */*


## fake_outer_number_serialize

> Float fake_outer_number_serialize(opts)



Test serialization of outer number types

### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::FakeApi.new
opts = {
  body: 8.14 # Float | Input number as post body
}

begin
  
  result = api_instance.fake_outer_number_serialize(opts)
  p result
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->fake_outer_number_serialize: #{e}"
end
```

#### Using the fake_outer_number_serialize_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(Float, Integer, Hash)> fake_outer_number_serialize_with_http_info(opts)

```ruby
begin
  
  data, status_code, headers = api_instance.fake_outer_number_serialize_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => Float
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->fake_outer_number_serialize_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **body** | **Float** | Input number as post body | [optional] |

### Return type

**Float**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: */*


## fake_outer_string_serialize

> String fake_outer_string_serialize(opts)



Test serialization of outer string types

### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::FakeApi.new
opts = {
  body: 'body_example' # String | Input string as post body
}

begin
  
  result = api_instance.fake_outer_string_serialize(opts)
  p result
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->fake_outer_string_serialize: #{e}"
end
```

#### Using the fake_outer_string_serialize_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(String, Integer, Hash)> fake_outer_string_serialize_with_http_info(opts)

```ruby
begin
  
  data, status_code, headers = api_instance.fake_outer_string_serialize_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => String
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->fake_outer_string_serialize_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **body** | **String** | Input string as post body | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: */*


## fake_property_enum_integer_serialize

> <OuterObjectWithEnumProperty> fake_property_enum_integer_serialize(outer_object_with_enum_property)



Test serialization of enum (int) properties with examples

### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::FakeApi.new
outer_object_with_enum_property = Petstore::OuterObjectWithEnumProperty.new({value: Petstore::OuterEnumInteger::N0}) # OuterObjectWithEnumProperty | Input enum (int) as post body

begin
  
  result = api_instance.fake_property_enum_integer_serialize(outer_object_with_enum_property)
  p result
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->fake_property_enum_integer_serialize: #{e}"
end
```

#### Using the fake_property_enum_integer_serialize_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(<OuterObjectWithEnumProperty>, Integer, Hash)> fake_property_enum_integer_serialize_with_http_info(outer_object_with_enum_property)

```ruby
begin
  
  data, status_code, headers = api_instance.fake_property_enum_integer_serialize_with_http_info(outer_object_with_enum_property)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => <OuterObjectWithEnumProperty>
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->fake_property_enum_integer_serialize_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **outer_object_with_enum_property** | [**OuterObjectWithEnumProperty**](OuterObjectWithEnumProperty.md) | Input enum (int) as post body |  |

### Return type

[**OuterObjectWithEnumProperty**](OuterObjectWithEnumProperty.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: */*


## test_body_with_binary

> test_body_with_binary(body)



For this test, the body has to be a binary file.

### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::FakeApi.new
body = File.new('/path/to/some/file') # File | image to upload

begin
  
  api_instance.test_body_with_binary(body)
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_body_with_binary: #{e}"
end
```

#### Using the test_body_with_binary_with_http_info variant

This returns an Array which contains the response data (`nil` in this case), status code and headers.

> <Array(nil, Integer, Hash)> test_body_with_binary_with_http_info(body)

```ruby
begin
  
  data, status_code, headers = api_instance.test_body_with_binary_with_http_info(body)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => nil
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_body_with_binary_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **body** | **File** | image to upload |  |

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: image/png
- **Accept**: Not defined


## test_body_with_file_schema

> test_body_with_file_schema(file_schema_test_class)



For this test, the body for this request must reference a schema named `File`.

### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::FakeApi.new
file_schema_test_class = Petstore::FileSchemaTestClass.new # FileSchemaTestClass | 

begin
  
  api_instance.test_body_with_file_schema(file_schema_test_class)
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_body_with_file_schema: #{e}"
end
```

#### Using the test_body_with_file_schema_with_http_info variant

This returns an Array which contains the response data (`nil` in this case), status code and headers.

> <Array(nil, Integer, Hash)> test_body_with_file_schema_with_http_info(file_schema_test_class)

```ruby
begin
  
  data, status_code, headers = api_instance.test_body_with_file_schema_with_http_info(file_schema_test_class)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => nil
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_body_with_file_schema_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **file_schema_test_class** | [**FileSchemaTestClass**](FileSchemaTestClass.md) |  |  |

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined


## test_body_with_query_params

> test_body_with_query_params(query, user)



### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::FakeApi.new
query = 'query_example' # String | 
user = Petstore::User.new # User | 

begin
  
  api_instance.test_body_with_query_params(query, user)
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_body_with_query_params: #{e}"
end
```

#### Using the test_body_with_query_params_with_http_info variant

This returns an Array which contains the response data (`nil` in this case), status code and headers.

> <Array(nil, Integer, Hash)> test_body_with_query_params_with_http_info(query, user)

```ruby
begin
  
  data, status_code, headers = api_instance.test_body_with_query_params_with_http_info(query, user)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => nil
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_body_with_query_params_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **query** | **String** |  |  |
| **user** | [**User**](User.md) |  |  |

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined


## test_client_model

> <Client> test_client_model(client)

To test \"client\" model

To test \"client\" model

### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::FakeApi.new
client = Petstore::Client.new # Client | client model

begin
  # To test \"client\" model
  result = api_instance.test_client_model(client)
  p result
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_client_model: #{e}"
end
```

#### Using the test_client_model_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(<Client>, Integer, Hash)> test_client_model_with_http_info(client)

```ruby
begin
  # To test \"client\" model
  data, status_code, headers = api_instance.test_client_model_with_http_info(client)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => <Client>
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_client_model_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **client** | [**Client**](Client.md) | client model |  |

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## test_endpoint_parameters

> test_endpoint_parameters(number, double, pattern_without_delimiter, byte, opts)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Examples

```ruby
require 'time'
require 'petstore'
# setup authorization
Petstore.configure do |config|
  # Configure HTTP basic authorization: http_basic_test
  config.username = 'YOUR USERNAME'
  config.password = 'YOUR PASSWORD'
end

api_instance = Petstore::FakeApi.new
number = 8.14 # Float | None
double = 1.2 # Float | None
pattern_without_delimiter = 'pattern_without_delimiter_example' # String | None
byte = 'BYTE_ARRAY_DATA_HERE' # String | None
opts = {
  integer: 56, # Integer | None
  int32: 56, # Integer | None
  int64: 789, # Integer | None
  float: 3.4, # Float | None
  string: 'string_example', # String | None
  binary: File.new('/path/to/some/file'), # File | None
  date: Date.parse('2013-10-20'), # Date | None
  date_time: Time.parse('2013-10-20T19:20:30+01:00'), # Time | None
  password: 'password_example', # String | None
  callback: 'callback_example' # String | None
}

begin
  # Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
  api_instance.test_endpoint_parameters(number, double, pattern_without_delimiter, byte, opts)
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_endpoint_parameters: #{e}"
end
```

#### Using the test_endpoint_parameters_with_http_info variant

This returns an Array which contains the response data (`nil` in this case), status code and headers.

> <Array(nil, Integer, Hash)> test_endpoint_parameters_with_http_info(number, double, pattern_without_delimiter, byte, opts)

```ruby
begin
  # Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
  data, status_code, headers = api_instance.test_endpoint_parameters_with_http_info(number, double, pattern_without_delimiter, byte, opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => nil
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_endpoint_parameters_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **number** | **Float** | None |  |
| **double** | **Float** | None |  |
| **pattern_without_delimiter** | **String** | None |  |
| **byte** | **String** | None |  |
| **integer** | **Integer** | None | [optional] |
| **int32** | **Integer** | None | [optional] |
| **int64** | **Integer** | None | [optional] |
| **float** | **Float** | None | [optional] |
| **string** | **String** | None | [optional] |
| **binary** | **File** | None | [optional] |
| **date** | **Date** | None | [optional] |
| **date_time** | **Time** | None | [optional] |
| **password** | **String** | None | [optional] |
| **callback** | **String** | None | [optional] |

### Return type

nil (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined


## test_enum_parameters

> test_enum_parameters(opts)

To test enum parameters

To test enum parameters

### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::FakeApi.new
opts = {
  enum_header_string_array: ['>'], # Array<String> | Header parameter enum test (string array)
  enum_header_string: '_abc', # String | Header parameter enum test (string)
  enum_query_string_array: ['>'], # Array<String> | Query parameter enum test (string array)
  enum_query_string: '_abc', # String | Query parameter enum test (string)
  enum_query_integer: 1, # Integer | Query parameter enum test (double)
  enum_query_double: 1.1, # Float | Query parameter enum test (double)
  enum_form_string_array: ['>'], # Array<String> | Form parameter enum test (string array)
  enum_form_string: '_abc' # String | Form parameter enum test (string)
}

begin
  # To test enum parameters
  api_instance.test_enum_parameters(opts)
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_enum_parameters: #{e}"
end
```

#### Using the test_enum_parameters_with_http_info variant

This returns an Array which contains the response data (`nil` in this case), status code and headers.

> <Array(nil, Integer, Hash)> test_enum_parameters_with_http_info(opts)

```ruby
begin
  # To test enum parameters
  data, status_code, headers = api_instance.test_enum_parameters_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => nil
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_enum_parameters_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **enum_header_string_array** | [**Array&lt;String&gt;**](String.md) | Header parameter enum test (string array) | [optional] |
| **enum_header_string** | **String** | Header parameter enum test (string) | [optional][default to &#39;-efg&#39;] |
| **enum_query_string_array** | [**Array&lt;String&gt;**](String.md) | Query parameter enum test (string array) | [optional] |
| **enum_query_string** | **String** | Query parameter enum test (string) | [optional][default to &#39;-efg&#39;] |
| **enum_query_integer** | **Integer** | Query parameter enum test (double) | [optional] |
| **enum_query_double** | **Float** | Query parameter enum test (double) | [optional] |
| **enum_form_string_array** | [**Array&lt;String&gt;**](String.md) | Form parameter enum test (string array) | [optional][default to &#39;$&#39;] |
| **enum_form_string** | **String** | Form parameter enum test (string) | [optional][default to &#39;-efg&#39;] |

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined


## test_group_parameters

> test_group_parameters(opts)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Examples

```ruby
require 'time'
require 'petstore'
# setup authorization
Petstore.configure do |config|
  # Configure Bearer authorization (JWT): bearer_test
  config.access_token = 'YOUR_BEARER_TOKEN'
end

api_instance = Petstore::FakeApi.new
opts = {
    required_string_group: 56, # Integer | Required String in group parameters (required)
    required_boolean_group: true, # Boolean | Required Boolean in group parameters (required)
    required_int64_group: 789, # Integer | Required Integer in group parameters (required)
    string_group: 56, # Integer | String in group parameters
    boolean_group: true, # Boolean | Boolean in group parameters
    int64_group: 789, # Integer | Integer in group parameters
}

begin
  # Fake endpoint to test group parameters (optional)
  api_instance.test_group_parameters(opts)
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_group_parameters: #{e}"
end
```

#### Using the test_group_parameters_with_http_info variant

This returns an Array which contains the response data (`nil` in this case), status code and headers.

> <Array(nil, Integer, Hash)> test_group_parameters_with_http_info(opts)

```ruby
begin
  # Fake endpoint to test group parameters (optional)
  data, status_code, headers = api_instance.test_group_parameters_with_http_info(opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => nil
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_group_parameters_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **required_string_group** | **Integer** | Required String in group parameters |  |
| **required_boolean_group** | **Boolean** | Required Boolean in group parameters |  |
| **required_int64_group** | **Integer** | Required Integer in group parameters |  |
| **string_group** | **Integer** | String in group parameters | [optional] |
| **boolean_group** | **Boolean** | Boolean in group parameters | [optional] |
| **int64_group** | **Integer** | Integer in group parameters | [optional] |

### Return type

nil (empty response body)

### Authorization

[bearer_test](../README.md#bearer_test)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


## test_inline_additional_properties

> test_inline_additional_properties(request_body)

test inline additionalProperties



### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::FakeApi.new
request_body = { key: 'inner_example'} # Hash<String, String> | request body

begin
  # test inline additionalProperties
  api_instance.test_inline_additional_properties(request_body)
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_inline_additional_properties: #{e}"
end
```

#### Using the test_inline_additional_properties_with_http_info variant

This returns an Array which contains the response data (`nil` in this case), status code and headers.

> <Array(nil, Integer, Hash)> test_inline_additional_properties_with_http_info(request_body)

```ruby
begin
  # test inline additionalProperties
  data, status_code, headers = api_instance.test_inline_additional_properties_with_http_info(request_body)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => nil
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_inline_additional_properties_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **request_body** | [**Hash&lt;String, String&gt;**](String.md) | request body |  |

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined


## test_json_form_data

> test_json_form_data(param, param2)

test json serialization of form data



### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::FakeApi.new
param = 'param_example' # String | field1
param2 = 'param2_example' # String | field2

begin
  # test json serialization of form data
  api_instance.test_json_form_data(param, param2)
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_json_form_data: #{e}"
end
```

#### Using the test_json_form_data_with_http_info variant

This returns an Array which contains the response data (`nil` in this case), status code and headers.

> <Array(nil, Integer, Hash)> test_json_form_data_with_http_info(param, param2)

```ruby
begin
  # test json serialization of form data
  data, status_code, headers = api_instance.test_json_form_data_with_http_info(param, param2)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => nil
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_json_form_data_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **param** | **String** | field1 |  |
| **param2** | **String** | field2 |  |

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined


## test_query_parameter_collection_format

> test_query_parameter_collection_format(pipe, ioutil, http, url, context, allow_empty, opts)



To test the collection format in query parameters

### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::FakeApi.new
pipe = ['inner_example'] # Array<String> | 
ioutil = ['inner_example'] # Array<String> | 
http = ['inner_example'] # Array<String> | 
url = ['inner_example'] # Array<String> | 
context = ['inner_example'] # Array<String> | 
allow_empty = 'allow_empty_example' # String | 
opts = {
  language: { key: 'inner_example'} # Hash<String, String> | 
}

begin
  
  api_instance.test_query_parameter_collection_format(pipe, ioutil, http, url, context, allow_empty, opts)
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_query_parameter_collection_format: #{e}"
end
```

#### Using the test_query_parameter_collection_format_with_http_info variant

This returns an Array which contains the response data (`nil` in this case), status code and headers.

> <Array(nil, Integer, Hash)> test_query_parameter_collection_format_with_http_info(pipe, ioutil, http, url, context, allow_empty, opts)

```ruby
begin
  
  data, status_code, headers = api_instance.test_query_parameter_collection_format_with_http_info(pipe, ioutil, http, url, context, allow_empty, opts)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => nil
rescue Petstore::ApiError => e
  puts "Error when calling FakeApi->test_query_parameter_collection_format_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **pipe** | [**Array&lt;String&gt;**](String.md) |  |  |
| **ioutil** | [**Array&lt;String&gt;**](String.md) |  |  |
| **http** | [**Array&lt;String&gt;**](String.md) |  |  |
| **url** | [**Array&lt;String&gt;**](String.md) |  |  |
| **context** | [**Array&lt;String&gt;**](String.md) |  |  |
| **allow_empty** | **String** |  |  |
| **language** | [**Hash&lt;String, String&gt;**](String.md) |  | [optional] |

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

