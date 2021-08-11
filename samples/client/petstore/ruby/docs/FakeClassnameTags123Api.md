# Petstore::FakeClassnameTags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
| ------ | ------------ | ----------- |
| [**test_classname**](FakeClassnameTags123Api.md#test_classname) | **PATCH** /fake_classname_test | To test class name in snake case |


## test_classname

> <Client> test_classname(client)

To test class name in snake case

To test class name in snake case

### Examples

```ruby
require 'time'
require 'petstore'
# setup authorization
Petstore.configure do |config|
  # Configure API key authorization: api_key_query
  config.api_key['api_key_query'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'Bearer' (defaults to nil)
  # config.api_key_prefix['api_key_query'] = 'Bearer'
end

api_instance = Petstore::FakeClassnameTags123Api.new
client = Petstore::Client.new # Client | client model

begin
  # To test class name in snake case
  result = api_instance.test_classname(client)
  p result
rescue Petstore::ApiError => e
  puts "Error when calling FakeClassnameTags123Api->test_classname: #{e}"
end
```

#### Using the test_classname_with_http_info variant

This returns an Array which contains the response data, status code and headers.

> <Array(<Client>, Integer, Hash)> test_classname_with_http_info(client)

```ruby
begin
  # To test class name in snake case
  data, status_code, headers = api_instance.test_classname_with_http_info(client)
  p status_code # => 2xx
  p headers # => { ... }
  p data # => <Client>
rescue Petstore::ApiError => e
  puts "Error when calling FakeClassnameTags123Api->test_classname_with_http_info: #{e}"
end
```

### Parameters

| Name | Type | Description | Notes |
| ---- | ---- | ----------- | ----- |
| **client** | [**Client**](Client.md) | client model |  |

### Return type

[**Client**](Client.md)

### Authorization

[api_key_query](../README.md#api_key_query)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

