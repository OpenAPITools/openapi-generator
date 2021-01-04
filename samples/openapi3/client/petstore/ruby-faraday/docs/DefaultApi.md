# OpenapiClient::DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**foo_get**](DefaultApi.md#foo_get) | **GET** /foo | 



## foo_get

> InlineResponseDefault foo_get



### Example

```ruby
# load the gem
require 'openapi_client'

api_instance = OpenapiClient::DefaultApi.new

begin
  result = api_instance.foo_get
  p result
rescue OpenapiClient::ApiError => e
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

