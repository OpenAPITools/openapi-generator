# Petstore::FakeClassnameTags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_classname**](FakeClassnameTags123Api.md#test_classname) | **PATCH** /fake_classname_test | To test class name in snake case


# **test_classname**
> Client test_classname(body)

To test class name in snake case

To test class name in snake case

### Example
```ruby
# load the gem
require 'petstore'
# setup authorization
Petstore.configure do |config|
  # Configure API key authorization: api_key_query
  config.api_key['api_key_query'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'Bearer' (defaults to nil)
  #config.api_key_prefix['api_key_query'] = 'Bearer'
end

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

[api_key_query](../README.md#api_key_query)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json



