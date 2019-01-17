# WWW::OpenAPIClient::AnotherFakeApi

## Load the API package
```perl
use WWW::OpenAPIClient::Object::AnotherFakeApi;
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**call_123_test_special_tags**](AnotherFakeApi.md#call_123_test_special_tags) | **PATCH** /another-fake/dummy | To test special tags


# **call_123_test_special_tags**
> Client call_123_test_special_tags(body => $body)

To test special tags

To test special tags and operation ID starting with number

### Example 
```perl
use Data::Dumper;
use WWW::OpenAPIClient::AnotherFakeApi;
my $api_instance = WWW::OpenAPIClient::AnotherFakeApi->new(
);

my $body = WWW::OpenAPIClient::Object::Client->new(); # Client | client model

eval { 
    my $result = $api_instance->call_123_test_special_tags(body => $body);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling AnotherFakeApi->call_123_test_special_tags: $@\n";
}
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

