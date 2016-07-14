# Petstore::FakeApi

All URIs are relative to *https://petstore.swagger.io */ &#39; &quot; &#x3D;end/v2 */ &#39; &quot; &#x3D;end*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_code_inject____end**](FakeApi.md#test_code_inject____end) | **PUT** /fake | To test code injection */ &#39; \&quot; 


# **test_code_inject____end**
> test_code_inject____end(opts)

To test code injection */ ' \" 

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new

opts = { 
  test_code_inject____end: "test_code_inject____end_example" # String | To test code injection */ ' \" 
}

begin
  #To test code injection */ ' \" 
  api_instance.test_code_inject____end(opts)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_code_inject____end: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **test_code_inject____end** | **String**| To test code injection */ &#39; \&quot;  | [optional] 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, */  " =end
 - **Accept**: application/json, */  " =end



