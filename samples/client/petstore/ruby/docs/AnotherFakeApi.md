# Petstore::AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
| ------ | ------------ | ----------- |
| [**call_123_test_special_tags**](AnotherFakeApi.md#call_123_test_special_tags) | **PATCH** /another-fake/dummy | To test special tags |


## call_123_test_special_tags

> <Client> call_123_test_special_tags(client)

To test special tags

To test special tags and operation ID starting with number

### Examples

```ruby
require 'time'
require 'petstore'

api_instance = Petstore::AnotherFakeApi.new
client = Petstore::Client.new # Client | client model

begin
  # To test special tags
  result = api_instance.call_123_test_special_tags(client)
  p result
rescue Petstore::ApiError => e
  puts "Error when calling AnotherFakeApi->call_123_test_special_tags: #{e}"
end
```

#### Using the call_123_test_special_tags_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(<Client>, Integer, Hash)> call_123_test_special_tags_with_http_info(client)

```ruby
begin
  # To test special tags
  data, status_code, headers = api_instance.call_123_test_special_tags_with_http_info(client)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => <Client>
rescue Petstore::ApiError => e
  puts "Error when calling AnotherFakeApi->call_123_test_special_tags_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **client** | [**Client**](Client.md) | client model |  |

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

