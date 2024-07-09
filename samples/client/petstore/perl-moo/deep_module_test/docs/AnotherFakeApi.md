# Something::Deep::AnotherFakeApi

## Load the API package
```perl
use Something::Deep::Object::AnotherFakeApi;
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**call_123_test_special_tags**](AnotherFakeApi.md#call_123_test_special_tags) | **PATCH** /another-fake/dummy | To test special tags


# **call_123_test_special_tags**
> Client call_123_test_special_tags(uuid_test => $uuid_test, body => $body)

To test special tags

To test special tags and operation ID starting with number

### Example
```perl
use Data::Dumper;
use Something::Deep::AnotherFakeApi;
my $api_instance = Something::Deep::AnotherFakeApi->new(
);

my $uuid_test = "uuid_test_example"; # string | to test uuid example value
my $body = Something::Deep::Object::Client->new(); # Client | client model

eval {
    my $result = $api_instance->call_123_test_special_tags(uuid_test => $uuid_test, body => $body);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling AnotherFakeApi->call_123_test_special_tags: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **uuid_test** | **string**| to test uuid example value | 
 **body** | [**Client**](Client.md)| client model | 

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

