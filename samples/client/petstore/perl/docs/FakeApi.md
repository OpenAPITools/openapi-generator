# WWW::SwaggerClient::FakeApi

## Load the API package
```perl
use WWW::SwaggerClient::Object::FakeApi;
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fake_outer_boolean_serialize**](FakeApi.md#fake_outer_boolean_serialize) | **POST** /fake/outer/boolean | 
[**fake_outer_composite_serialize**](FakeApi.md#fake_outer_composite_serialize) | **POST** /fake/outer/composite | 
[**fake_outer_number_serialize**](FakeApi.md#fake_outer_number_serialize) | **POST** /fake/outer/number | 
[**fake_outer_string_serialize**](FakeApi.md#fake_outer_string_serialize) | **POST** /fake/outer/string | 
[**test_client_model**](FakeApi.md#test_client_model) | **PATCH** /fake | To test \&quot;client\&quot; model
[**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**test_enum_parameters**](FakeApi.md#test_enum_parameters) | **GET** /fake | To test enum parameters
[**test_json_form_data**](FakeApi.md#test_json_form_data) | **GET** /fake/jsonFormData | test json serialization of form data


# **fake_outer_boolean_serialize**
> OuterBoolean fake_outer_boolean_serialize(body => $body)



Test serialization of outer boolean types

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::FakeApi;
my $api_instance = WWW::SwaggerClient::FakeApi->new(
);

my $body = WWW::SwaggerClient::Object::OuterBoolean->new(); # OuterBoolean | Input boolean as post body

eval { 
    my $result = $api_instance->fake_outer_boolean_serialize(body => $body);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling FakeApi->fake_outer_boolean_serialize: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterBoolean**](OuterBoolean.md)| Input boolean as post body | [optional] 

### Return type

[**OuterBoolean**](OuterBoolean.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_composite_serialize**
> OuterComposite fake_outer_composite_serialize(body => $body)



Test serialization of object with outer number type

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::FakeApi;
my $api_instance = WWW::SwaggerClient::FakeApi->new(
);

my $body = WWW::SwaggerClient::Object::OuterComposite->new(); # OuterComposite | Input composite as post body

eval { 
    my $result = $api_instance->fake_outer_composite_serialize(body => $body);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling FakeApi->fake_outer_composite_serialize: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional] 

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_number_serialize**
> OuterNumber fake_outer_number_serialize(body => $body)



Test serialization of outer number types

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::FakeApi;
my $api_instance = WWW::SwaggerClient::FakeApi->new(
);

my $body = WWW::SwaggerClient::Object::OuterNumber->new(); # OuterNumber | Input number as post body

eval { 
    my $result = $api_instance->fake_outer_number_serialize(body => $body);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling FakeApi->fake_outer_number_serialize: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterNumber**](OuterNumber.md)| Input number as post body | [optional] 

### Return type

[**OuterNumber**](OuterNumber.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_string_serialize**
> OuterString fake_outer_string_serialize(body => $body)



Test serialization of outer string types

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::FakeApi;
my $api_instance = WWW::SwaggerClient::FakeApi->new(
);

my $body = WWW::SwaggerClient::Object::OuterString->new(); # OuterString | Input string as post body

eval { 
    my $result = $api_instance->fake_outer_string_serialize(body => $body);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling FakeApi->fake_outer_string_serialize: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterString**](OuterString.md)| Input string as post body | [optional] 

### Return type

[**OuterString**](OuterString.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_client_model**
> Client test_client_model(body => $body)

To test \"client\" model

To test \"client\" model

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::FakeApi;
my $api_instance = WWW::SwaggerClient::FakeApi->new(
);

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
use WWW::SwaggerClient::FakeApi;
my $api_instance = WWW::SwaggerClient::FakeApi->new(

    # Configure HTTP basic authorization: http_basic_test
    username => 'YOUR_USERNAME',
    password => 'YOUR_PASSWORD',
);

my $number = 3.4; # Number | None
my $double = 1.2; # double | None
my $pattern_without_delimiter = 'pattern_without_delimiter_example'; # string | None
my $byte = 'byte_example'; # string | None
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
use WWW::SwaggerClient::FakeApi;
my $api_instance = WWW::SwaggerClient::FakeApi->new(
);

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

# **test_json_form_data**
> test_json_form_data(param => $param, param2 => $param2)

test json serialization of form data



### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::FakeApi;
my $api_instance = WWW::SwaggerClient::FakeApi->new(
);

my $param = 'param_example'; # string | field1
my $param2 = 'param2_example'; # string | field2

eval { 
    $api_instance->test_json_form_data(param => $param, param2 => $param2);
};
if ($@) {
    warn "Exception when calling FakeApi->test_json_form_data: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **string**| field1 | 
 **param2** | **string**| field2 | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

