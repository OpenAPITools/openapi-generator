# Petstore::FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fake_outer_boolean_serialize**](FakeApi.md#fake_outer_boolean_serialize) | **POST** /fake/outer/boolean | 
[**fake_outer_composite_serialize**](FakeApi.md#fake_outer_composite_serialize) | **POST** /fake/outer/composite | 
[**fake_outer_number_serialize**](FakeApi.md#fake_outer_number_serialize) | **POST** /fake/outer/number | 
[**fake_outer_string_serialize**](FakeApi.md#fake_outer_string_serialize) | **POST** /fake/outer/string | 
[**test_body_with_query_params**](FakeApi.md#test_body_with_query_params) | **PUT** /fake/body-with-query-params | 
[**test_client_model**](FakeApi.md#test_client_model) | **PATCH** /fake | To test \&quot;client\&quot; model
[**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**test_enum_parameters**](FakeApi.md#test_enum_parameters) | **GET** /fake | To test enum parameters
[**test_inline_additional_properties**](FakeApi.md#test_inline_additional_properties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**test_json_form_data**](FakeApi.md#test_json_form_data) | **GET** /fake/jsonFormData | test json serialization of form data


# **fake_outer_boolean_serialize**
> OuterBoolean fake_outer_boolean_serialize()



Test serialization of outer boolean types

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new


begin
  result = api_instance.fake_outer_boolean_serialize()
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->fake_outer_boolean_serialize: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **UNKNOWN_PARAM_NAME** | [****](.md)| Input boolean as post body | [optional] 

### Return type

[**OuterBoolean**](OuterBoolean.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*



# **fake_outer_composite_serialize**
> OuterComposite fake_outer_composite_serialize()



Test serialization of object with outer number type

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new


begin
  result = api_instance.fake_outer_composite_serialize()
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

 - **Content-Type**: */*
 - **Accept**: */*



# **fake_outer_number_serialize**
> OuterNumber fake_outer_number_serialize()



Test serialization of outer number types

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new


begin
  result = api_instance.fake_outer_number_serialize()
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->fake_outer_number_serialize: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **UNKNOWN_PARAM_NAME** | [****](.md)| Input number as post body | [optional] 

### Return type

[**OuterNumber**](OuterNumber.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*



# **fake_outer_string_serialize**
> OuterString fake_outer_string_serialize()



Test serialization of outer string types

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new


begin
  result = api_instance.fake_outer_string_serialize()
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->fake_outer_string_serialize: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **UNKNOWN_PARAM_NAME** | [****](.md)| Input string as post body | [optional] 

### Return type

[**OuterString**](OuterString.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*



# **test_body_with_query_params**
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



# **test_client_model**
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



# **test_endpoint_parameters**
> test_endpoint_parameters(UNKNOWN_PARAM_NAME)

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

UNKNOWN_PARAM_NAME = Petstore::null.new #  | 


begin
  #Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
  api_instance.test_endpoint_parameters(UNKNOWN_PARAM_NAME)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_endpoint_parameters: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **UNKNOWN_PARAM_NAME** | [****](.md)|  | 

### Return type

nil (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: Not defined



# **test_enum_parameters**
> test_enum_parameters()

To test enum parameters

To test enum parameters

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new


begin
  #To test enum parameters
  api_instance.test_enum_parameters()
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_enum_parameters: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_header_string_array** | [**Array&lt;String&gt;**](String.md)| Header parameter enum test (string array) | [optional] 
 **enum_header_string** | **String**| Header parameter enum test (string) | [optional] 
 **enum_query_string_array** | [**Array&lt;String&gt;**](String.md)| Query parameter enum test (string array) | [optional] 
 **enum_query_string** | **String**| Query parameter enum test (string) | [optional] 
 **enum_query_integer** | **Integer**| Query parameter enum test (double) | [optional] 
 **enum_query_double** | **Float**| Query parameter enum test (double) | [optional] 
 **UNKNOWN_PARAM_NAME** | [****](.md)|  | [optional] 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: Not defined



# **test_inline_additional_properties**
> test_inline_additional_properties(UNKNOWN_PARAM_NAME)

test inline additionalProperties

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new

UNKNOWN_PARAM_NAME = Petstore::null.new #  | request body


begin
  #test inline additionalProperties
  api_instance.test_inline_additional_properties(UNKNOWN_PARAM_NAME)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_inline_additional_properties: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **UNKNOWN_PARAM_NAME** | [****](.md)| request body | 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined



# **test_json_form_data**
> test_json_form_data(UNKNOWN_PARAM_NAME)

test json serialization of form data

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new

UNKNOWN_PARAM_NAME = Petstore::null.new #  | 


begin
  #test json serialization of form data
  api_instance.test_json_form_data(UNKNOWN_PARAM_NAME)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_json_form_data: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **UNKNOWN_PARAM_NAME** | [****](.md)|  | 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined



