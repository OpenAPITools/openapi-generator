# Something::Deep::FakeApi

## Load the API package
```perl
use Something::Deep::Object::FakeApi;
```

All URIs are relative to *petstore.swagger.io */ &#39; \&quot; &#x3D;_end -- \\r\\n \\n \\r/v2 */ &#39; \&quot; &#x3D;_end -- \\r\\n \\n \\r*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_code_inject____end__rn_n_r**](FakeApi.md#test_code_inject____end__rn_n_r) | **PUT** /fake | To test code injection */ &#39; \&quot; &#x3D;_end -- \\r\\n \\n \\r


# **test_code_inject____end__rn_n_r**
> test_code_inject____end__rn_n_r(unknown_base_type => $unknown_base_type)

To test code injection */ ' \" =_end -- \\r\\n \\n \\r

### Example 
```perl
use Data::Dumper;
use Something::Deep::FakeApi;
my $api_instance = Something::Deep::FakeApi->new(
);

my $unknown_base_type = Something::Deep::Object::object->new(); # object | 

eval { 
    $api_instance->test_code_inject____end__rn_n_r(unknown_base_type => $unknown_base_type);
};
if ($@) {
    warn "Exception when calling FakeApi->test_code_inject____end__rn_n_r: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **unknown_base_type** | [**object**](UNKNOWN_BASE_TYPE.md)|  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, */  \" =_end --       
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

