# WWW::SwaggerClient::FakeApi

## Load the API package
```perl
use WWW::SwaggerClient::Object::FakeApi;
```

All URIs are relative to *https://petstore.swagger.io */ &#39; &quot; &#x3D;end/v2 */ &#39; &quot; &#x3D;end*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_code_inject____end**](FakeApi.md#test_code_inject____end) | **PUT** /fake | To test code injection */ &#39; \&quot; 


# **test_code_inject____end**
> test_code_inject____end(test code inject */ &#39; &quot; &#x3D;end => $test code inject */ &#39; &quot; &#x3D;end)

To test code injection */ ' \" 

### Example 
```perl
use Data::Dumper;

my $api_instance = WWW::SwaggerClient::FakeApi->new();
my $test code inject */ &#39; &quot; &#x3D;end = 'test code inject */ ' " =end_example'; # string | To test code injection */ ' \" 

eval { 
    $api_instance->test_code_inject____end(test code inject */ &#39; &quot; &#x3D;end => $test code inject */ &#39; &quot; &#x3D;end);
};
if ($@) {
    warn "Exception when calling FakeApi->test_code_inject____end: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **test code inject */ &#39; &quot; &#x3D;end** | **string**| To test code injection */ &#39; \&quot;  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, */  " =end
 - **Accept**: application/json, */  " =end

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

