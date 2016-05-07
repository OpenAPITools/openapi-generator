# Petstore::FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters


# **test_endpoint_parameters**
> test_endpoint_parameters(number, double, string, byte, opts)

Fake endpoint for testing various parameters

Fake endpoint for testing various parameters

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
  #Fake endpoint for testing various parameters
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

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json



