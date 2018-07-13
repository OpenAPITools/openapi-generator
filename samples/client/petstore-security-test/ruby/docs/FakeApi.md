# Petstore::FakeApi

All URIs are relative to *http://petstore.swagger.io */ &#39; \&quot; &#x3D;_end -- \\r\\n \\n \\r/v2 */ &#39; \&quot; &#x3D;_end -- \\r\\n \\n \\r*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_code_inject____end__rn_n_r**](FakeApi.md#test_code_inject____end__rn_n_r) | **PUT** /fake | To test code injection */ &#39; \&quot; &#x3D;_end -- \\r\\n \\n \\r


# **test_code_inject____end__rn_n_r**
> test_code_inject____end__rn_n_r(opts)

To test code injection */ ' \" =_end -- \\r\\n \\n \\r

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeApi.new
opts = {
  unknown_base_type: Petstore::UNKNOWN_BASE_TYPE.new # Object | 
}

begin
  #To test code injection */ ' \" =_end -- \\r\\n \\n \\r
  api_instance.test_code_inject____end__rn_n_r(opts)
rescue Petstore::ApiError => e
  puts "Exception when calling FakeApi->test_code_inject____end__rn_n_r: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **unknown_base_type** | [**Object**](UNKNOWN_BASE_TYPE.md)|  | [optional] 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, */  \" =_end --       
 - **Accept**: Not defined



