# Petstore::FakeClassnameTags123Api

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_classname**](FakeClassnameTags123Api.md#test_classname) | **PATCH** /fake_classname_test | To test class name in snake case


# **test_classname**
> Client test_classname(body)

To test class name in snake case

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::FakeClassnameTags123Api.new

body = Petstore::Client.new # Client | client model


begin
  #To test class name in snake case
  result = api_instance.test_classname(body)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling FakeClassnameTags123Api->test_classname: #{e}"
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



