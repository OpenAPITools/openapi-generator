# WWW::SwaggerClient::FakeClassnameTags123Api

## Load the API package
```perl
use WWW::SwaggerClient::Object::FakeClassnameTags123Api;
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_classname**](FakeClassnameTags123Api.md#test_classname) | **PATCH** /fake_classname_test | To test class name in snake case


# **test_classname**
> Client test_classname(body => $body)

To test class name in snake case

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::FakeClassnameTags123Api;
my $api_instance = WWW::SwaggerClient::FakeClassnameTags123Api->new(
);

my $body = WWW::SwaggerClient::Object::Client->new(); # Client | client model

eval { 
    my $result = $api_instance->test_classname(body => $body);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling FakeClassnameTags123Api->test_classname: $@\n";
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

