# WWW::OpenAPIClient::FakeApi

## Load the API package
```perl
use WWW::OpenAPIClient::Object::FakeApi;
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fake_health_get**](FakeApi.md#fake_health_get) | **GET** /fake/health | Health check endpoint
[**fake_http_signature_test**](FakeApi.md#fake_http_signature_test) | **GET** /fake/http-signature-test | test http signature authentication
[**fake_outer_boolean_serialize**](FakeApi.md#fake_outer_boolean_serialize) | **POST** /fake/outer/boolean | 
[**fake_outer_composite_serialize**](FakeApi.md#fake_outer_composite_serialize) | **POST** /fake/outer/composite | 
[**fake_outer_number_serialize**](FakeApi.md#fake_outer_number_serialize) | **POST** /fake/outer/number | 
[**fake_outer_string_serialize**](FakeApi.md#fake_outer_string_serialize) | **POST** /fake/outer/string | 
[**fake_property_enum_integer_serialize**](FakeApi.md#fake_property_enum_integer_serialize) | **POST** /fake/property/enum-int | 
[**test_body_with_binary**](FakeApi.md#test_body_with_binary) | **PUT** /fake/body-with-binary | 
[**test_body_with_file_schema**](FakeApi.md#test_body_with_file_schema) | **PUT** /fake/body-with-file-schema | 
[**test_body_with_query_params**](FakeApi.md#test_body_with_query_params) | **PUT** /fake/body-with-query-params | 
[**test_client_model**](FakeApi.md#test_client_model) | **PATCH** /fake | To test \&quot;client\&quot; model
[**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**test_enum_parameters**](FakeApi.md#test_enum_parameters) | **GET** /fake | To test enum parameters
[**test_group_parameters**](FakeApi.md#test_group_parameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**test_inline_additional_properties**](FakeApi.md#test_inline_additional_properties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**test_json_form_data**](FakeApi.md#test_json_form_data) | **GET** /fake/jsonFormData | test json serialization of form data
[**test_query_parameter_collection_format**](FakeApi.md#test_query_parameter_collection_format) | **PUT** /fake/test-query-parameters | 


# **fake_health_get**
> HealthCheckResult fake_health_get()

Health check endpoint

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(
);


eval {
    my $result = $api_instance->fake_health_get();
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling FakeApi->fake_health_get: $@\n";
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**HealthCheckResult**](HealthCheckResult.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_http_signature_test**
> fake_http_signature_test(pet => $pet, query_1 => $query_1, header_1 => $header_1)

test http signature authentication

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(

    # Configure HTTP basic authorization: http_signature_test
    
);

my $pet = WWW::OpenAPIClient::Object::Pet->new(); # Pet | Pet object that needs to be added to the store
my $query_1 = "query_1_example"; # string | query parameter
my $header_1 = "header_1_example"; # string | header parameter

eval {
    $api_instance->fake_http_signature_test(pet => $pet, query_1 => $query_1, header_1 => $header_1);
};
if ($@) {
    warn "Exception when calling FakeApi->fake_http_signature_test: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 
 **query_1** | **string**| query parameter | [optional] 
 **header_1** | **string**| header parameter | [optional] 

### Return type

void (empty response body)

### Authorization

[http_signature_test](../README.md#http_signature_test)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_boolean_serialize**
> boolean fake_outer_boolean_serialize(body => $body)



Test serialization of outer boolean types

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(
);

my $body = WWW::OpenAPIClient::Object::boolean->new(); # boolean | Input boolean as post body

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
 **body** | **boolean**| Input boolean as post body | [optional] 

### Return type

**boolean**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_composite_serialize**
> OuterComposite fake_outer_composite_serialize(outer_composite => $outer_composite)



Test serialization of object with outer number type

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(
);

my $outer_composite = WWW::OpenAPIClient::Object::OuterComposite->new(); # OuterComposite | Input composite as post body

eval {
    my $result = $api_instance->fake_outer_composite_serialize(outer_composite => $outer_composite);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling FakeApi->fake_outer_composite_serialize: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outer_composite** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional] 

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_number_serialize**
> double fake_outer_number_serialize(body => $body)



Test serialization of outer number types

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(
);

my $body = WWW::OpenAPIClient::Object::double->new(); # double | Input number as post body

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
 **body** | **double**| Input number as post body | [optional] 

### Return type

**double**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_string_serialize**
> string fake_outer_string_serialize(body => $body)



Test serialization of outer string types

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(
);

my $body = WWW::OpenAPIClient::Object::string->new(); # string | Input string as post body

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
 **body** | **string**| Input string as post body | [optional] 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_property_enum_integer_serialize**
> OuterObjectWithEnumProperty fake_property_enum_integer_serialize(outer_object_with_enum_property => $outer_object_with_enum_property)



Test serialization of enum (int) properties with examples

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(
);

my $outer_object_with_enum_property = WWW::OpenAPIClient::Object::OuterObjectWithEnumProperty->new(); # OuterObjectWithEnumProperty | Input enum (int) as post body

eval {
    my $result = $api_instance->fake_property_enum_integer_serialize(outer_object_with_enum_property => $outer_object_with_enum_property);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling FakeApi->fake_property_enum_integer_serialize: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outer_object_with_enum_property** | [**OuterObjectWithEnumProperty**](OuterObjectWithEnumProperty.md)| Input enum (int) as post body | 

### Return type

[**OuterObjectWithEnumProperty**](OuterObjectWithEnumProperty.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_body_with_binary**
> test_body_with_binary(body => $body)



For this test, the body has to be a binary file.

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(
);

my $body = WWW::OpenAPIClient::Object::string->new(); # string | image to upload

eval {
    $api_instance->test_body_with_binary(body => $body);
};
if ($@) {
    warn "Exception when calling FakeApi->test_body_with_binary: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **string****string**| image to upload | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: image/png
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_body_with_file_schema**
> test_body_with_file_schema(file_schema_test_class => $file_schema_test_class)



For this test, the body for this request must reference a schema named `File`.

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(
);

my $file_schema_test_class = WWW::OpenAPIClient::Object::FileSchemaTestClass->new(); # FileSchemaTestClass | 

eval {
    $api_instance->test_body_with_file_schema(file_schema_test_class => $file_schema_test_class);
};
if ($@) {
    warn "Exception when calling FakeApi->test_body_with_file_schema: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **file_schema_test_class** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_body_with_query_params**
> test_body_with_query_params(query => $query, user => $user)



### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(
);

my $query = "query_example"; # string | 
my $user = WWW::OpenAPIClient::Object::User->new(); # User | 

eval {
    $api_instance->test_body_with_query_params(query => $query, user => $user);
};
if ($@) {
    warn "Exception when calling FakeApi->test_body_with_query_params: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **string**|  | 
 **user** | [**User**](User.md)|  | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_client_model**
> Client test_client_model(client => $client)

To test \"client\" model

To test \"client\" model

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(
);

my $client = WWW::OpenAPIClient::Object::Client->new(); # Client | client model

eval {
    my $result = $api_instance->test_client_model(client => $client);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling FakeApi->test_client_model: $@\n";
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

# **test_endpoint_parameters**
> test_endpoint_parameters(number => $number, double => $double, pattern_without_delimiter => $pattern_without_delimiter, byte => $byte, integer => $integer, int32 => $int32, int64 => $int64, float => $float, string => $string, binary => $binary, date => $date, date_time => $date_time, password => $password, callback => $callback)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(

    # Configure HTTP basic authorization: http_basic_test
    username => 'YOUR_USERNAME',
    password => 'YOUR_PASSWORD',
    
);

my $number = 3.4; # double | None
my $double = 3.4; # double | None
my $pattern_without_delimiter = "pattern_without_delimiter_example"; # string | None
my $byte = "YmFzZSA2NCBkYXRh"; # string | None
my $integer = 56; # int | None
my $int32 = 56; # int | None
my $int64 = 789; # int | None
my $float = 3.4; # double | None
my $string = "string_example"; # string | None
my $binary = "/path/to/file"; # string | None
my $date = DateTime->from_epoch(epoch => str2time('null')); # DateTime | None
my $date_time = DateTime->from_epoch(epoch => str2time('null')); # DateTime | None
my $password = "password_example"; # string | None
my $callback = "callback_example"; # string | None

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
 **number** | **double**| None | 
 **double** | **double**| None | 
 **pattern_without_delimiter** | **string**| None | 
 **byte** | **string**| None | 
 **integer** | **int**| None | [optional] 
 **int32** | **int**| None | [optional] 
 **int64** | **int**| None | [optional] 
 **float** | **double**| None | [optional] 
 **string** | **string**| None | [optional] 
 **binary** | **string****string**| None | [optional] 
 **date** | **DateTime**| None | [optional] 
 **date_time** | **DateTime**| None | [optional] 
 **password** | **string**| None | [optional] 
 **callback** | **string**| None | [optional] 

### Return type

void (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_enum_parameters**
> test_enum_parameters(enum_header_string_array => $enum_header_string_array, enum_header_string => $enum_header_string, enum_query_string_array => $enum_query_string_array, enum_query_string => $enum_query_string, enum_query_integer => $enum_query_integer, enum_query_double => $enum_query_double, enum_form_string_array => $enum_form_string_array, enum_form_string => $enum_form_string)

To test enum parameters

To test enum parameters

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(
);

my $enum_header_string_array = [("'$'")]; # ARRAY[string] | Header parameter enum test (string array)
my $enum_header_string = '-efg'; # string | Header parameter enum test (string)
my $enum_query_string_array = [("'$'")]; # ARRAY[string] | Query parameter enum test (string array)
my $enum_query_string = '-efg'; # string | Query parameter enum test (string)
my $enum_query_integer = 56; # int | Query parameter enum test (double)
my $enum_query_double = 3.4; # double | Query parameter enum test (double)
my $enum_form_string_array = ['$']; # ARRAY[string] | Form parameter enum test (string array)
my $enum_form_string = '-efg'; # string | Form parameter enum test (string)

eval {
    $api_instance->test_enum_parameters(enum_header_string_array => $enum_header_string_array, enum_header_string => $enum_header_string, enum_query_string_array => $enum_query_string_array, enum_query_string => $enum_query_string, enum_query_integer => $enum_query_integer, enum_query_double => $enum_query_double, enum_form_string_array => $enum_form_string_array, enum_form_string => $enum_form_string);
};
if ($@) {
    warn "Exception when calling FakeApi->test_enum_parameters: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_header_string_array** | [**ARRAY[string]**](string.md)| Header parameter enum test (string array) | [optional] 
 **enum_header_string** | **string**| Header parameter enum test (string) | [optional] [default to &#39;-efg&#39;]
 **enum_query_string_array** | [**ARRAY[string]**](string.md)| Query parameter enum test (string array) | [optional] 
 **enum_query_string** | **string**| Query parameter enum test (string) | [optional] [default to &#39;-efg&#39;]
 **enum_query_integer** | **int**| Query parameter enum test (double) | [optional] 
 **enum_query_double** | **double**| Query parameter enum test (double) | [optional] 
 **enum_form_string_array** | [**ARRAY[string]**](string.md)| Form parameter enum test (string array) | [optional] [default to &#39;$&#39;]
 **enum_form_string** | **string**| Form parameter enum test (string) | [optional] [default to &#39;-efg&#39;]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_group_parameters**
> test_group_parameters(required_string_group => $required_string_group, required_boolean_group => $required_boolean_group, required_int64_group => $required_int64_group, string_group => $string_group, boolean_group => $boolean_group, int64_group => $int64_group)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(

    # Configure HTTP basic authorization: bearer_test
    # Configure bearer access token for authorization: bearer_test
    access_token => 'YOUR_BEARER_TOKEN',
    
);

my $required_string_group = 56; # int | Required String in group parameters
my $required_boolean_group = null; # boolean | Required Boolean in group parameters
my $required_int64_group = 789; # int | Required Integer in group parameters
my $string_group = 56; # int | String in group parameters
my $boolean_group = null; # boolean | Boolean in group parameters
my $int64_group = 789; # int | Integer in group parameters

eval {
    $api_instance->test_group_parameters(required_string_group => $required_string_group, required_boolean_group => $required_boolean_group, required_int64_group => $required_int64_group, string_group => $string_group, boolean_group => $boolean_group, int64_group => $int64_group);
};
if ($@) {
    warn "Exception when calling FakeApi->test_group_parameters: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **required_string_group** | **int**| Required String in group parameters | 
 **required_boolean_group** | **boolean**| Required Boolean in group parameters | 
 **required_int64_group** | **int**| Required Integer in group parameters | 
 **string_group** | **int**| String in group parameters | [optional] 
 **boolean_group** | **boolean**| Boolean in group parameters | [optional] 
 **int64_group** | **int**| Integer in group parameters | [optional] 

### Return type

void (empty response body)

### Authorization

[bearer_test](../README.md#bearer_test)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_inline_additional_properties**
> test_inline_additional_properties(request_body => $request_body)

test inline additionalProperties

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(
);

my $request_body = WWW::OpenAPIClient::Object::HASH[string,string]->new(); # HASH[string,string] | request body

eval {
    $api_instance->test_inline_additional_properties(request_body => $request_body);
};
if ($@) {
    warn "Exception when calling FakeApi->test_inline_additional_properties: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **request_body** | [**HASH[string,string]**](string.md)| request body | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_json_form_data**
> test_json_form_data(param => $param, param2 => $param2)

test json serialization of form data

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(
);

my $param = "param_example"; # string | field1
my $param2 = "param2_example"; # string | field2

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

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_query_parameter_collection_format**
> test_query_parameter_collection_format(pipe => $pipe, ioutil => $ioutil, http => $http, url => $url, context => $context, allow_empty => $allow_empty, language => $language)



To test the collection format in query parameters

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::FakeApi;
my $api_instance = WWW::OpenAPIClient::FakeApi->new(
);

my $pipe = [("null")]; # ARRAY[string] | 
my $ioutil = [("null")]; # ARRAY[string] | 
my $http = [("null")]; # ARRAY[string] | 
my $url = [("null")]; # ARRAY[string] | 
my $context = [("null")]; # ARRAY[string] | 
my $allow_empty = "allow_empty_example"; # string | 
my $language = ('key' =>  "null"}; # HASH[string,string] | 

eval {
    $api_instance->test_query_parameter_collection_format(pipe => $pipe, ioutil => $ioutil, http => $http, url => $url, context => $context, allow_empty => $allow_empty, language => $language);
};
if ($@) {
    warn "Exception when calling FakeApi->test_query_parameter_collection_format: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**ARRAY[string]**](string.md)|  | 
 **ioutil** | [**ARRAY[string]**](string.md)|  | 
 **http** | [**ARRAY[string]**](string.md)|  | 
 **url** | [**ARRAY[string]**](string.md)|  | 
 **context** | [**ARRAY[string]**](string.md)|  | 
 **allow_empty** | **string**|  | 
 **language** | [**HASH[string,string]**](string.md)|  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

