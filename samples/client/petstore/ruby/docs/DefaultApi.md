# Petstore::DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**foo_get**](DefaultApi.md#foo_get) | **GET** /foo | 



## foo_get

> InlineResponseDefault foo_get



### Example

```ruby
# load the gem
require 'petstore'

api_instance = Petstore::DefaultApi.new

begin
  result = api_instance.foo_get
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling DefaultApi->foo_get: #{e}"
end
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**InlineResponseDefault**](InlineResponseDefault.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

