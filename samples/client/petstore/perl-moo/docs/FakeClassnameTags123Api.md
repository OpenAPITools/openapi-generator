# WWW::OpenAPIClient::FakeClassnameTags123Api

## Load the API package
```perl
use WWW::OpenAPIClient::Object::FakeClassnameTags123Api;
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_classname**](FakeClassnameTags123Api.md#test_classname) | **PATCH** /fake_classname_test | To test class name in snake case


# **test_classname**
> Client test_classname(client => $client)

To test class name in snake case

To test class name in snake case

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeClassnameTags123Api;
my $api_instance = WWW::OpenAPIClient::FakeClassnameTags123Api->new(

    # Configure API key authorization: api_key_query
    api_key => {'api_key_query' => 'YOUR_API_KEY'},
    # uncomment below to setup prefix (e.g. Bearer) for API key, if needed
    #api_key_prefix => {'api_key_query' => 'Bearer'},
);

my $client = WWW::OpenAPIClient::Object::Client->new(); # Client | client model

eval {
    my $result = $api_instance->test_classname(client => $client);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling FakeClassnameTags123Api->test_classname: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | [**Client**](Client.md)| client model | 

### Return type

[**Client**](Client.md)

### Authorization

[api_key_query](../README.md#api_key_query)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

