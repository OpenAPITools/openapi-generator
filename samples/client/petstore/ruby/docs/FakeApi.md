# Petstore::FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fake_health_get**](FakeApi.md#fake_health_get) | **GET** /fake/health | Health check endpoint
[**fake_http_signature_test**](FakeApi.md#fake_http_signature_test) | **GET** /fake/http-signature-test | test http signature authentication
[**fake_outer_boolean_serialize**](FakeApi.md#fake_outer_boolean_serialize) | **POST** /fake/outer/boolean | 
[**fake_outer_composite_serialize**](FakeApi.md#fake_outer_composite_serialize) | **POST** /fake/outer/composite | 
[**fake_outer_number_serialize**](FakeApi.md#fake_outer_number_serialize) | **POST** /fake/outer/number | 
[**fake_outer_string_serialize**](FakeApi.md#fake_outer_string_serialize) | **POST** /fake/outer/string | 
[**test_body_with_file_schema**](FakeApi.md#test_body_with_file_schema) | **PUT** /fake/body-with-file-schema | 
[**test_body_with_query_params**](FakeApi.md#test_body_with_query_params) | **PUT** /fake/body-with-query-params | 
[**test_client_model**](FakeApi.md#test_client_model) | **PATCH** /fake | To test \&quot;client\&quot; model
[**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**test_enum_parameters**](FakeApi.md#test_enum_parameters) | **GET** /fake | To test enum parameters
[**test_group_parameters**](FakeApi.md#test_group_parameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**test_inline_additional_properties**](FakeApi.md#test_inline_additional_properties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**test_json_form_data**](FakeApi.md#test_json_form_data) | **GET** /fake/jsonFormData | test json serialization of form data
[**test_query_parameter_collection_format**](FakeApi.md#test_query_parameter_collection_format) | **PUT** /fake/test-query-paramters | 



## fake_health_get

> HealthCheckResult fake_health_get

Health check endpoint

### Example

```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new

begin
  #Health check endpoint
  result = api_instance.fake_health_get
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->fake_health_get: #{e}"
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

### Example

```ruby
# load the gem
require 'petstore'
# setup authorization
Petstore.configure do |config|
end

api_instance = Petstore::FakeApi.new
pet = Petstore::Pet.new # Pet | Pet object that needs to be added to the store
opts = {
  query_1: 'query_1_example', # String | query parameter
  header_1: 'header_1_example' # String | header parameter
}

begin
  #test http signature authentication
  api_instance.fake_http_signature_test(pet, opts)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->fake_http_signature_test: #{e}"
end
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 
 **query_1** | **String**| query parameter | [optional] 
 **header_1** | **String**| header parameter | [optional] 

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

### Example

```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new
opts = {
  body: true # Boolean | Input boolean as post body
}

begin
  result = api_instance.fake_outer_boolean_serialize(opts)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->fake_outer_boolean_serialize: #{e}"
end
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **Boolean**| Input boolean as post body | [optional] 

### Return type

**Boolean**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: */*


## fake_outer_composite_serialize

> OuterComposite fake_outer_composite_serialize(opts)



Test serialization of object with outer number type

### Example

```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new
opts = {
  outer_composite: Petstore::OuterComposite.new # OuterComposite | Input composite as post body
}

begin
  result = api_instance.fake_outer_composite_serialize(opts)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->fake_outer_composite_serialize: #{e}"
end
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outer_composite** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional] 

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

### Example

```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new
opts = {
  body: 3.4 # Float | Input number as post body
}

begin
  result = api_instance.fake_outer_number_serialize(opts)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->fake_outer_number_serialize: #{e}"
end
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **Float**| Input number as post body | [optional] 

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

### Example

```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new
opts = {
  body: 'body_example' # String | Input string as post body
}

begin
  result = api_instance.fake_outer_string_serialize(opts)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->fake_outer_string_serialize: #{e}"
end
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **String**| Input string as post body | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: */*


## test_body_with_file_schema

> test_body_with_file_schema(file_schema_test_class)



For this test, the body for this request much reference a schema named `File`.

### Example

```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new
file_schema_test_class = Petstore::FileSchemaTestClass.new # FileSchemaTestClass | 

begin
  api_instance.test_body_with_file_schema(file_schema_test_class)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_body_with_file_schema: #{e}"
end
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **file_schema_test_class** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  | 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined


## test_body_with_query_params

> test_body_with_query_params(query, user)



### Example

```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new
query = 'query_example' # String | 
user = Petstore::User.new # User | 

begin
  api_instance.test_body_with_query_params(query, user)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_body_with_query_params: #{e}"
end
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **String**|  | 
 **user** | [**User**](User.md)|  | 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined


## test_client_model

> Client test_client_model(client)

To test \"client\" model

To test \"client\" model

### Example

```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new
client = Petstore::Client.new # Client | client model

begin
  #To test \"client\" model
  result = api_instance.test_client_model(client)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_client_model: #{e}"
end
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | [**Client**](Client.md)| client model | 

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

### Example

```ruby
# load the gem
require 'petstore'
# setup authorization
Petstore.configure do |config|
  # Configure HTTP basic authorization: http_basic_test
  config.username = 'YOUR USERNAME'
  config.password = 'YOUR PASSWORD'
end

api_instance = Petstore::FakeApi.new
number = 3.4 # Float | None
double = 3.4 # Float | None
pattern_without_delimiter = 'pattern_without_delimiter_example' # String | None
byte = 'byte_example' # String | None
opts = {
  integer: 56, # Integer | None
  int32: 56, # Integer | None
  int64: 56, # Integer | None
  float: 3.4, # Float | None
  string: 'string_example', # String | None
  binary: File.new('/path/to/file'), # File | None
  date: Date.parse('2013-10-20'), # Date | None
  date_time: DateTime.parse('2013-10-20T19:20:30+01:00'), # DateTime | None
  password: 'password_example', # String | None
  callback: 'callback_example' # String | None
}

begin
  #Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
  api_instance.test_endpoint_parameters(number, double, pattern_without_delimiter, byte, opts)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_endpoint_parameters: #{e}"
end
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **Float**| None | 
 **double** | **Float**| None | 
 **pattern_without_delimiter** | **String**| None | 
 **byte** | **String**| None | 
 **integer** | **Integer**| None | [optional] 
 **int32** | **Integer**| None | [optional] 
 **int64** | **Integer**| None | [optional] 
 **float** | **Float**| None | [optional] 
 **string** | **String**| None | [optional] 
 **binary** | **File**| None | [optional] 
 **date** | **Date**| None | [optional] 
 **date_time** | **DateTime**| None | [optional] 
 **password** | **String**| None | [optional] 
 **callback** | **String**| None | [optional] 

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

### Example

```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new
opts = {
  enum_header_string_array: ['enum_header_string_array_example'], # Array<String> | Header parameter enum test (string array)
  enum_header_string: '-efg', # String | Header parameter enum test (string)
  enum_query_string_array: ['enum_query_string_array_example'], # Array<String> | Query parameter enum test (string array)
  enum_query_string: '-efg', # String | Query parameter enum test (string)
  enum_query_integer: 56, # Integer | Query parameter enum test (double)
  enum_query_double: 3.4, # Float | Query parameter enum test (double)
  enum_form_string_array: '$', # Array<String> | Form parameter enum test (string array)
  enum_form_string: '-efg' # String | Form parameter enum test (string)
}

begin
  #To test enum parameters
  api_instance.test_enum_parameters(opts)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_enum_parameters: #{e}"
end
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_header_string_array** | [**Array&lt;String&gt;**](String.md)| Header parameter enum test (string array) | [optional] 
 **enum_header_string** | **String**| Header parameter enum test (string) | [optional] [default to &#39;-efg&#39;]
 **enum_query_string_array** | [**Array&lt;String&gt;**](String.md)| Query parameter enum test (string array) | [optional] 
 **enum_query_string** | **String**| Query parameter enum test (string) | [optional] [default to &#39;-efg&#39;]
 **enum_query_integer** | **Integer**| Query parameter enum test (double) | [optional] 
 **enum_query_double** | **Float**| Query parameter enum test (double) | [optional] 
 **enum_form_string_array** | [**Array&lt;String&gt;**](String.md)| Form parameter enum test (string array) | [optional] [default to &#39;$&#39;]
 **enum_form_string** | **String**| Form parameter enum test (string) | [optional] [default to &#39;-efg&#39;]

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined


## test_group_parameters

> test_group_parameters(required_string_group, required_boolean_group, required_int64_group, opts)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example

```ruby
# load the gem
require 'petstore'
# setup authorization
Petstore.configure do |config|
  # Configure Bearer authorization (JWT): bearer_test
  config.access_token = 'YOUR_BEARER_TOKEN'
end

api_instance = Petstore::FakeApi.new
required_string_group = 56 # Integer | Required String in group parameters
required_boolean_group = true # Boolean | Required Boolean in group parameters
required_int64_group = 56 # Integer | Required Integer in group parameters
opts = {
  string_group: 56, # Integer | String in group parameters
  boolean_group: true, # Boolean | Boolean in group parameters
  int64_group: 56 # Integer | Integer in group parameters
}

begin
  #Fake endpoint to test group parameters (optional)
  api_instance.test_group_parameters(required_string_group, required_boolean_group, required_int64_group, opts)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_group_parameters: #{e}"
end
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **required_string_group** | **Integer**| Required String in group parameters | 
 **required_boolean_group** | **Boolean**| Required Boolean in group parameters | 
 **required_int64_group** | **Integer**| Required Integer in group parameters | 
 **string_group** | **Integer**| String in group parameters | [optional] 
 **boolean_group** | **Boolean**| Boolean in group parameters | [optional] 
 **int64_group** | **Integer**| Integer in group parameters | [optional] 

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

### Example

```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new
request_body = {'key' => 'request_body_example'} # Hash<String, String> | request body

begin
  #test inline additionalProperties
  api_instance.test_inline_additional_properties(request_body)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_inline_additional_properties: #{e}"
end
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **request_body** | [**Hash&lt;String, String&gt;**](String.md)| request body | 

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

### Example

```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new
param = 'param_example' # String | field1
param2 = 'param2_example' # String | field2

begin
  #test json serialization of form data
  api_instance.test_json_form_data(param, param2)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_json_form_data: #{e}"
end
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **String**| field1 | 
 **param2** | **String**| field2 | 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined


## test_query_parameter_collection_format

> test_query_parameter_collection_format(pipe, ioutil, http, url, context)



To test the collection format in query parameters

### Example

```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new
pipe = ['pipe_example'] # Array<String> | 
ioutil = ['ioutil_example'] # Array<String> | 
http = ['http_example'] # Array<String> | 
url = ['url_example'] # Array<String> | 
context = ['context_example'] # Array<String> | 

begin
  api_instance.test_query_parameter_collection_format(pipe, ioutil, http, url, context)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_query_parameter_collection_format: #{e}"
end
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**Array&lt;String&gt;**](String.md)|  | 
 **ioutil** | [**Array&lt;String&gt;**](String.md)|  | 
 **http** | [**Array&lt;String&gt;**](String.md)|  | 
 **url** | [**Array&lt;String&gt;**](String.md)|  | 
 **context** | [**Array&lt;String&gt;**](String.md)|  | 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

