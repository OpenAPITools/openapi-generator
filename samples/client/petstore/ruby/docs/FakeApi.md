# Petstore::FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_client_model**](FakeApi.md#test_client_model) | **PATCH** /fake | To test \&quot;client\&quot; model
[**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**test_enum_query_parameters**](FakeApi.md#test_enum_query_parameters) | **GET** /fake | To test enum query parameters


# **test_client_model**
> Client test_client_model(body)

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



