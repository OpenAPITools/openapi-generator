# WWW::SwaggerClient::FakeApi

## Load the API package
```perl
use WWW::SwaggerClient::Object::FakeApi;
```

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_code_inject__end**](FakeApi.md#test_code_inject__end) | **PUT** /fake | To test code injection */ 
[**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**test_enum_query_parameters**](FakeApi.md#test_enum_query_parameters) | **GET** /fake | To test enum query parameters


# **test_code_inject__end**
> test_code_inject__end(test code inject */ &#x3D;end => $test code inject */ &#x3D;end)

To test code injection */ 

### Example 
```perl
use Data::Dumper;

my $api_instance = WWW::SwaggerClient::FakeApi->new();
my $test code inject */ &#x3D;end = 'test code inject */ =end_example'; # string | To test code injection */ 

eval { 
    $api_instance->test_code_inject__end(test code inject */ &#x3D;end => $test code inject */ &#x3D;end);
};
if ($@) {
    warn "Exception when calling FakeApi->test_code_inject__end: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **test code inject */ &#x3D;end** | **string**| To test code injection */  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, */ =end));(phpinfo(
 - **Accept**: application/json, */ end

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_endpoint_parameters**
> test_endpoint_parameters(number => $number, double => $double, string => $string, byte => $byte, integer => $integer, int32 => $int32, int64 => $int64, float => $float, binary => $binary, date => $date, date_time => $date_time, password => $password)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example 
```perl
use Data::Dumper;

my $api_instance = WWW::SwaggerClient::FakeApi->new();
my $number = 3.4; # Number | None
my $double = 1.2; # double | None
my $string = 'string_example'; # string | None
my $byte = 'B'; # string | None
my $integer = 56; # int | None
my $int32 = 56; # int | None
my $int64 = 789; # int | None
my $float = 3.4; # double | None
my $binary = 'B'; # string | None
my $date = DateTime->from_epoch(epoch => str2time('2013-10-20')); # DateTime | None
my $date_time = DateTime->from_epoch(epoch => str2time('2013-10-20T19:20:30+01:00')); # DateTime | None
my $password = 'password_example'; # string | None

eval { 
    $api_instance->test_endpoint_parameters(number => $number, double => $double, string => $string, byte => $byte, integer => $integer, int32 => $int32, int64 => $int64, float => $float, binary => $binary, date => $date, date_time => $date_time, password => $password);
};
if ($@) {
    warn "Exception when calling FakeApi->test_endpoint_parameters: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **Number**| None | 
 **double** | **double**| None | 
 **string** | **string**| None | 
 **byte** | **string**| None | 
 **integer** | **int**| None | [optional] 
 **int32** | **int**| None | [optional] 
 **int64** | **int**| None | [optional] 
 **float** | **double**| None | [optional] 
 **binary** | **string**| None | [optional] 
 **date** | **DateTime**| None | [optional] 
 **date_time** | **DateTime**| None | [optional] 
 **password** | **string**| None | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: application/xml; charset=utf-8, application/json; charset=utf-8

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_enum_query_parameters**
> test_enum_query_parameters(enum_query_string => $enum_query_string, enum_query_integer => $enum_query_integer, enum_query_double => $enum_query_double)

To test enum query parameters

### Example 
```perl
use Data::Dumper;

my $api_instance = WWW::SwaggerClient::FakeApi->new();
my $enum_query_string = 'enum_query_string_example'; # string | Query parameter enum test (string)
my $enum_query_integer = 3.4; # Number | Query parameter enum test (double)
my $enum_query_double = 1.2; # double | Query parameter enum test (double)

eval { 
    $api_instance->test_enum_query_parameters(enum_query_string => $enum_query_string, enum_query_integer => $enum_query_integer, enum_query_double => $enum_query_double);
};
if ($@) {
    warn "Exception when calling FakeApi->test_enum_query_parameters: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_query_string** | **string**| Query parameter enum test (string) | [optional] [default to -efg]
 **enum_query_integer** | **Number**| Query parameter enum test (double) | [optional] 
 **enum_query_double** | **double**| Query parameter enum test (double) | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

