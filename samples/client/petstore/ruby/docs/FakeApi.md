# Petstore::FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_code_inject__end**](FakeApi.md#test_code_inject__end) | **PUT** /fake | To test code injection */ 
[**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**test_enum_query_parameters**](FakeApi.md#test_enum_query_parameters) | **GET** /fake | To test enum query parameters


# **test_code_inject__end**
> test_code_inject__end(opts)

To test code injection */ 

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new

opts = { 
  test_code_inject__end: "test_code_inject__end_example" # String | To test code injection */ 
}

begin
  #To test code injection */ 
  api_instance.test_code_inject__end(opts)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_code_inject__end: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **test_code_inject__end** | **String**| To test code injection */  | [optional] 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, */ =end));(phpinfo(
 - **Accept**: application/json, */ end



# **test_endpoint_parameters**
> test_endpoint_parameters(number, double, string, byte, opts)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new

number = 3.4 # Float | None

double = 1.2 # Float | None

string = "string_example" # String | None

byte = "B" # String | None

opts = { 
  integer: 56, # Integer | None
  int32: 56, # Integer | None
  int64: 789, # Integer | None
  float: 3.4, # Float | None
  binary: "B", # String | None
  date: Date.parse("2013-10-20"), # Date | None
  date_time: DateTime.parse("2013-10-20T19:20:30+01:00"), # DateTime | None
  password: "password_example" # String | None
}

begin
  #Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
  api_instance.test_endpoint_parameters(number, double, string, byte, opts)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_endpoint_parameters: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **Float**| None | 
 **double** | **Float**| None | 
 **string** | **String**| None | 
 **byte** | **String**| None | 
 **integer** | **Integer**| None | [optional] 
 **int32** | **Integer**| None | [optional] 
 **int64** | **Integer**| None | [optional] 
 **float** | **Float**| None | [optional] 
 **binary** | **String**| None | [optional] 
 **date** | **Date**| None | [optional] 
 **date_time** | **DateTime**| None | [optional] 
 **password** | **String**| None | [optional] 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: application/xml; charset=utf-8, application/json; charset=utf-8



# **test_enum_query_parameters**
> test_enum_query_parameters(opts)

To test enum query parameters

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new

opts = { 
  enum_query_string: "-efg", # String | Query parameter enum test (string)
  enum_query_integer: 3.4, # Float | Query parameter enum test (double)
  enum_query_double: 1.2 # Float | Query parameter enum test (double)
}

begin
  #To test enum query parameters
  api_instance.test_enum_query_parameters(opts)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_enum_query_parameters: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_query_string** | **String**| Query parameter enum test (string) | [optional] [default to -efg]
 **enum_query_integer** | **Float**| Query parameter enum test (double) | [optional] 
 **enum_query_double** | **Float**| Query parameter enum test (double) | [optional] 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json



