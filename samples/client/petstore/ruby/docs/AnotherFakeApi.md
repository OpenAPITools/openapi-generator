# Petstore::AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_special_tags**](AnotherFakeApi.md#test_special_tags) | **PATCH** /another-fake/dummy | To test special tags


# **test_special_tags**
> Client test_special_tags(body)

To test special tags

To test special tags

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::AnotherFakeApi.new

body = Petstore::Client.new # Client | client model


begin
  #To test special tags
  result = api_instance.test_special_tags(body)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling AnotherFakeApi->test_special_tags: #{e}"
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



