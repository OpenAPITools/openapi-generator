# Petstore::FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fake_outer_boolean_serialize**](FakeApi.md#fake_outer_boolean_serialize) | **POST** /fake/outer/boolean | 
[**fake_outer_composite_serialize**](FakeApi.md#fake_outer_composite_serialize) | **POST** /fake/outer/composite | 
[**fake_outer_number_serialize**](FakeApi.md#fake_outer_number_serialize) | **POST** /fake/outer/number | 
[**fake_outer_string_serialize**](FakeApi.md#fake_outer_string_serialize) | **POST** /fake/outer/string | 
[**test_client_model**](FakeApi.md#test_client_model) | **PATCH** /fake | To test \&quot;client\&quot; model
[**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**test_enum_parameters**](FakeApi.md#test_enum_parameters) | **GET** /fake | To test enum parameters


# **fake_outer_boolean_serialize**
> OuterBoolean fake_outer_boolean_serialize(opts)



Test serialization of outer boolean types

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new

opts = { 
  body: Petstore::OuterBoolean.new # OuterBoolean | Input boolean as post body
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
 **body** | [**OuterBoolean**](OuterBoolean.md)| Input boolean as post body | [optional] 

### Return type

[**OuterBoolean**](OuterBoolean.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined



# **fake_outer_composite_serialize**
> OuterComposite fake_outer_composite_serialize(opts)



Test serialization of object with outer number type

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new

opts = { 
  body: Petstore::OuterComposite.new # OuterComposite | Input composite as post body
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
 **body** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional] 

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined



# **fake_outer_number_serialize**
> OuterNumber fake_outer_number_serialize(opts)



Test serialization of outer number types

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new

opts = { 
  body: Petstore::OuterNumber.new # OuterNumber | Input number as post body
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
 **body** | [**OuterNumber**](OuterNumber.md)| Input number as post body | [optional] 

### Return type

[**OuterNumber**](OuterNumber.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined



# **fake_outer_string_serialize**
> OuterString fake_outer_string_serialize(opts)



Test serialization of outer string types

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new

opts = { 
  body: Petstore::OuterString.new # OuterString | Input string as post body
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
 **body** | [**OuterString**](OuterString.md)| Input string as post body | [optional] 

### Return type

[**OuterString**](OuterString.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined



# **test_client_model**
> Client test_client_model(body)

To test \"client\" model

To test \"client\" model

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new

body = Petstore::Client.new # Client | client model


begin
  #To test \"client\" model
  result = api_instance.test_client_model(body)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_client_model: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md)| client model | 

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json



# **test_endpoint_parameters**
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

double = 1.2 # Float | None

pattern_without_delimiter = "pattern_without_delimiter_example" # String | None

byte = "B" # String | None

opts = { 
  integer: 56, # Integer | None
  int32: 56, # Integer | None
  int64: 789, # Integer | None
  float: 3.4, # Float | None
  string: "string_example", # String | None
  binary: "B", # String | None
  date: Date.parse("2013-10-20"), # Date | None
  date_time: DateTime.parse("2013-10-20T19:20:30+01:00"), # DateTime | None
  password: "password_example", # String | None
  callback: "callback_example" # String | None
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
 **binary** | **String**| None | [optional] 
 **date** | **Date**| None | [optional] 
 **date_time** | **DateTime**| None | [optional] 
 **password** | **String**| None | [optional] 
 **callback** | **String**| None | [optional] 

### Return type

nil (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: application/xml; charset=utf-8, application/json; charset=utf-8



# **test_enum_parameters**
> test_enum_parameters(opts)

To test enum parameters

To test enum parameters

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new

opts = { 
  enum_form_string_array: ["enum_form_string_array_example"], # Array<String> | Form parameter enum test (string array)
  enum_form_string: "-efg", # String | Form parameter enum test (string)
  enum_header_string_array: ["enum_header_string_array_example"], # Array<String> | Header parameter enum test (string array)
  enum_header_string: "-efg", # String | Header parameter enum test (string)
  enum_query_string_array: ["enum_query_string_array_example"], # Array<String> | Query parameter enum test (string array)
  enum_query_string: "-efg", # String | Query parameter enum test (string)
  enum_query_integer: 56, # Integer | Query parameter enum test (double)
  enum_query_double: 1.2 # Float | Query parameter enum test (double)
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
 **enum_form_string_array** | [**Array&lt;String&gt;**](String.md)| Form parameter enum test (string array) | [optional] 
 **enum_form_string** | **String**| Form parameter enum test (string) | [optional] [default to -efg]
 **enum_header_string_array** | [**Array&lt;String&gt;**](String.md)| Header parameter enum test (string array) | [optional] 
 **enum_header_string** | **String**| Header parameter enum test (string) | [optional] [default to -efg]
 **enum_query_string_array** | [**Array&lt;String&gt;**](String.md)| Query parameter enum test (string array) | [optional] 
 **enum_query_string** | **String**| Query parameter enum test (string) | [optional] [default to -efg]
 **enum_query_integer** | **Integer**| Query parameter enum test (double) | [optional] 
 **enum_query_double** | **Float**| Query parameter enum test (double) | [optional] 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*



