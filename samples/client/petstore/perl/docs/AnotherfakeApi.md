# WWW::OpenAPIClient::AnotherFakeApi

## Load the API package
```perl
use WWW::OpenAPIClient::Object::AnotherFakeApi;
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_special_tags**](AnotherFakeApi.md#test_special_tags) | **PATCH** /another-fake/dummy | To test special tags


# **test_special_tags**
> Client test_special_tags(client => $client)

To test special tags

To test special tags

### Example 
```perl
use Data::Dumper;
use WWW::OpenAPIClient::AnotherFakeApi;
my $api_instance = WWW::OpenAPIClient::AnotherFakeApi->new(
);

my $client = WWW::OpenAPIClient::Object::Client->new(); # Client | client model

eval { 
    my $result = $api_instance->test_special_tags(client => $client);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling AnotherFakeApi->test_special_tags: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | [**Client**](Client.md)| client model | 

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

