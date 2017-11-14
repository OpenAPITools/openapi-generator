# WWW::SwaggerClient::FakeApi

## Load the API package
```perl
use WWW::SwaggerClient::Object::FakeApi;
```

All URIs are relative to *https://petstore.swagger.io */ &#39; \&quot; &#x3D;_end -- \\r\\n \\n \\r/v2 */ &#39; \&quot; &#x3D;_end -- \\r\\n \\n \\r*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_code_inject____end__rn_n_r**](FakeApi.md#test_code_inject____end__rn_n_r) | **PUT** /fake | To test code injection */ &#39; \&quot; &#x3D;_end -- \\r\\n \\n \\r


# **test_code_inject____end__rn_n_r**
> test_code_inject____end__rn_n_r(test_code_inject_*/_&#39;_&quot;_&#x3D;end____\r\n_\n_\r => $test_code_inject_*/_&#39;_&quot;_&#x3D;end____\r\n_\n_\r)

To test code injection */ ' \" =_end -- \\r\\n \\n \\r

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::FakeApi;
my $api_instance = WWW::SwaggerClient::FakeApi->new(
);

my $test_code_inject_*/_&#39;_&quot;_&#x3D;end____\r\n_\n_\r = 'test_code_inject_*/_'_"_=end____\r\n_\n_\r_example'; # string | To test code injection */ ' \" =_end -- \\r\\n \\n \\r

eval { 
    $api_instance->test_code_inject____end__rn_n_r(test_code_inject_*/_&#39;_&quot;_&#x3D;end____\r\n_\n_\r => $test_code_inject_*/_&#39;_&quot;_&#x3D;end____\r\n_\n_\r);
};
if ($@) {
    warn "Exception when calling FakeApi->test_code_inject____end__rn_n_r: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **test_code_inject_*/_&#39;_&quot;_&#x3D;end____\r\n_\n_\r** | **string**| To test code injection */ &#39; \&quot; &#x3D;_end -- \\r\\n \\n \\r | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, */  \" =_end --       
 - **Accept**: application/json, */  \" =_end --       

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

