# WWW::SwaggerClient::FakeApi

## Load the API package
```perl
use WWW::SwaggerClient::Object::FakeApi;
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_client_model**](FakeApi.md#test_client_model) | **PATCH** /fake | To test \&quot;client\&quot; model
[**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**test_enum_parameters**](FakeApi.md#test_enum_parameters) | **GET** /fake | To test enum parameters


# **test_client_model**
> Client test_client_model(body => $body)

To test \"client\" model

To test \"client\" model

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::Configuration;
use WWW::SwaggerClient::FakeApi;

my $api_instance = WWW::SwaggerClient::FakeApi->new();
my $body = WWW::SwaggerClient::Object::Client->new(); # Client | client model

eval { 
    my $result = $api_instance->test_client_model(body => $body);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling FakeApi->test_client_model: $@\n";
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

# **test_endpoint_parameters**
> test_endpoint_parameters(number => $number, double => $double, pattern_without_delimiter => $pattern_without_delimiter, byte => $byte, integer => $integer, int32 => $int32, int64 => $int64, float => $float, string => $string, binary => $binary, date => $date, date_time => $date_time, password => $password, callback => $callback)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::Configuration;
use WWW::SwaggerClient::FakeApi;

# Configure HTTP basic authorization: http_basic_test
$WWW::SwaggerClient::Configuration::username = 'YOUR_USERNAME';
$WWW::SwaggerClient::Configuration::password = 'YOUR_PASSWORD';

my $api_instance = WWW::SwaggerClient::FakeApi->new();
my $number = 3.4; # Number | None
my $double = 1.2; # double | None
my $pattern_without_delimiter = 'pattern_without_delimiter_example'; # string | None
my $byte = 'B'; # string | None
my $integer = 56; # int | None
my $int32 = 56; # int | None
my $int64 = 789; # int | None
my $float = 3.4; # double | None
my $string = 'string_example'; # string | None
my $binary = 'B'; # string | None
my $date = DateTime->from_epoch(epoch => str2time('2013-10-20')); # DateTime | None
my $date_time = DateTime->from_epoch(epoch => str2time('2013-10-20T19:20:30+01:00')); # DateTime | None
my $password = 'password_example'; # string | None
my $callback = 'callback_example'; # string | None

eval { 
    $api_instance->test_endpoint_parameters(number => $number, double => $double, pattern_without_delimiter => $pattern_without_delimiter, byte => $byte, integer => $integer, int32 => $int32, int64 => $int64, float => $float, string => $string, binary => $binary, date => $date, date_time => $date_time, password => $password, callback => $callback);
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
 **pattern_without_delimiter** | **string**| None | 
 **byte** | **string**| None | 
 **integer** | **int**| None | [optional] 
 **int32** | **int**| None | [optional] 
 **int64** | **int**| None | [optional] 
 **float** | **double**| None | [optional] 
 **string** | **string**| None | [optional] 
 **binary** | **string**| None | [optional] 
 **date** | **DateTime**| None | [optional] 
 **date_time** | **DateTime**| None | [optional] 
 **password** | **string**| None | [optional] 
 **callback** | **string**| None | [optional] 

### Return type

void (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: application/xml; charset=utf-8, application/json; charset=utf-8

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_enum_parameters**
> test_enum_parameters(enum_form_string_array => $enum_form_string_array, enum_form_string => $enum_form_string, enum_header_string_array => $enum_header_string_array, enum_header_string => $enum_header_string, enum_query_string_array => $enum_query_string_array, enum_query_string => $enum_query_string, enum_query_integer => $enum_query_integer, enum_query_double => $enum_query_double)

To test enum parameters

To test enum parameters

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::Configuration;
use WWW::SwaggerClient::FakeApi;

my $api_instance = WWW::SwaggerClient::FakeApi->new();
my $enum_form_string_array = []; # ARRAY[string] | Form parameter enum test (string array)
my $enum_form_string = 'enum_form_string_example'; # string | Form parameter enum test (string)
my $enum_header_string_array = []; # ARRAY[string] | Header parameter enum test (string array)
my $enum_header_string = 'enum_header_string_example'; # string | Header parameter enum test (string)
my $enum_query_string_array = []; # ARRAY[string] | Query parameter enum test (string array)
my $enum_query_string = 'enum_query_string_example'; # string | Query parameter enum test (string)
my $enum_query_integer = 56; # int | Query parameter enum test (double)
my $enum_query_double = 1.2; # double | Query parameter enum test (double)

eval { 
    $api_instance->test_enum_parameters(enum_form_string_array => $enum_form_string_array, enum_form_string => $enum_form_string, enum_header_string_array => $enum_header_string_array, enum_header_string => $enum_header_string, enum_query_string_array => $enum_query_string_array, enum_query_string => $enum_query_string, enum_query_integer => $enum_query_integer, enum_query_double => $enum_query_double);
};
if ($@) {
    warn "Exception when calling FakeApi->test_enum_parameters: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_form_string_array** | [**ARRAY[string]**](string.md)| Form parameter enum test (string array) | [optional] 
 **enum_form_string** | **string**| Form parameter enum test (string) | [optional] [default to -efg]
 **enum_header_string_array** | [**ARRAY[string]**](string.md)| Header parameter enum test (string array) | [optional] 
 **enum_header_string** | **string**| Header parameter enum test (string) | [optional] [default to -efg]
 **enum_query_string_array** | [**ARRAY[string]**](string.md)| Query parameter enum test (string array) | [optional] 
 **enum_query_string** | **string**| Query parameter enum test (string) | [optional] [default to -efg]
 **enum_query_integer** | **int**| Query parameter enum test (double) | [optional] 
 **enum_query_double** | **double**| Query parameter enum test (double) | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

