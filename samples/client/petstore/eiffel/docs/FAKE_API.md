# FAKE_API

All URIs are relative to *http://petstore.swagger.io:80/v2*

Feature | HTTP request | Description
------------- | ------------- | -------------
[**fake_outer_boolean_serialize**](FAKE_API.md#fake_outer_boolean_serialize) | **Post** /fake/outer/boolean | 
[**fake_outer_composite_serialize**](FAKE_API.md#fake_outer_composite_serialize) | **Post** /fake/outer/composite | 
[**fake_outer_number_serialize**](FAKE_API.md#fake_outer_number_serialize) | **Post** /fake/outer/number | 
[**fake_outer_string_serialize**](FAKE_API.md#fake_outer_string_serialize) | **Post** /fake/outer/string | 
[**test_body_with_query_params**](FAKE_API.md#test_body_with_query_params) | **Put** /fake/body-with-query-params | 
[**test_client_model**](FAKE_API.md#test_client_model) | **Patch** /fake | To test \&quot;client\&quot; model
[**test_endpoint_parameters**](FAKE_API.md#test_endpoint_parameters) | **Post** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**test_enum_parameters**](FAKE_API.md#test_enum_parameters) | **Get** /fake | To test enum parameters
[**test_inline_additional_properties**](FAKE_API.md#test_inline_additional_properties) | **Post** /fake/inline-additionalProperties | test inline additionalProperties
[**test_json_form_data**](FAKE_API.md#test_json_form_data) | **Get** /fake/jsonFormData | test json serialization of form data


# **fake_outer_boolean_serialize**
> fake_outer_boolean_serialize (body:  detachable BOOLEAN ): detachable BOOLEAN
	



Test serialization of outer boolean types


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **BOOLEAN**| Input boolean as post body | [optional] 

### Return type

**BOOLEAN**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_composite_serialize**
> fake_outer_composite_serialize (outer_composite:  detachable OUTER_COMPOSITE ): detachable OUTER_COMPOSITE
	



Test serialization of object with outer number type


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outer_composite** | [**OUTER_COMPOSITE**](OUTER_COMPOSITE.md)| Input composite as post body | [optional] 

### Return type

[**OUTER_COMPOSITE**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_number_serialize**
> fake_outer_number_serialize (body:  detachable REAL_32 ): detachable REAL_32
	



Test serialization of outer number types


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **REAL_32**| Input number as post body | [optional] 

### Return type

**REAL_32**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_string_serialize**
> fake_outer_string_serialize (body:  detachable STRING_32 ): detachable STRING_32
	



Test serialization of outer string types


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **STRING_32**| Input string as post body | [optional] 

### Return type

[**STRING_32**](STRING_32.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_body_with_query_params**
> test_body_with_query_params (query: STRING_32 ; user: USER )
	




### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **STRING_32**|  | 
 **user** | [**USER**](USER.md)|  | 

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_client_model**
> test_client_model (client: CLIENT ): detachable CLIENT
	

To test \"client\" model

To test \"client\" model


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | [**CLIENT**](CLIENT.md)| client model | 

### Return type

[**CLIENT**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_endpoint_parameters**
> test_endpoint_parameters (number: REAL_32 ; double: REAL_64 ; pattern_without_delimiter: STRING_32 ; byte: ARRAY [NATURAL_8] ; integer:  detachable INTEGER_32 ; int32:  detachable INTEGER_32 ; int64:  detachable INTEGER_64 ; float:  detachable REAL_32 ; string:  detachable STRING_32 ; binary:  detachable FILE ; date:  detachable DATE ; date_time:  detachable DATE_TIME ; password:  detachable STRING_32 ; callback:  detachable STRING_32 )
	

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **REAL_32**| None | [default to null]
 **double** | **REAL_64**| None | [default to null]
 **pattern_without_delimiter** | **STRING_32**| None | [default to null]
 **byte** | **ARRAY [NATURAL_8]**| None | [default to null]
 **integer** | **INTEGER_32**| None | [optional] [default to null]
 **int32** | **INTEGER_32**| None | [optional] [default to null]
 **int64** | **INTEGER_64**| None | [optional] [default to null]
 **float** | **REAL_32**| None | [optional] [default to null]
 **string** | **STRING_32**| None | [optional] [default to null]
 **binary** | **FILE**| None | [optional] [default to null]
 **date** | **DATE**| None | [optional] [default to null]
 **date_time** | **DATE_TIME**| None | [optional] [default to null]
 **password** | **STRING_32**| None | [optional] [default to null]
 **callback** | **STRING_32**| None | [optional] [default to null]

### Return type

{empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_enum_parameters**
> test_enum_parameters (enum_header_string_array:  detachable LIST [STRING_32] ; enum_header_string:  detachable STRING_32 ; enum_query_string_array:  detachable LIST [STRING_32] ; enum_query_string:  detachable STRING_32 ; enum_query_integer:  detachable INTEGER_32 ; enum_query_double:  detachable REAL_64 ; enum_form_string_array:  detachable LIST [STRING_32] ; enum_form_string:  detachable STRING_32 )
	

To test enum parameters

To test enum parameters


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_header_string_array** | [**LIST [STRING_32]**](STRING_32.md)| Header parameter enum test (string array) | [optional] 
 **enum_header_string** | **STRING_32**| Header parameter enum test (string) | [optional] [default to -efg]
 **enum_query_string_array** | [**LIST [STRING_32]**](STRING_32.md)| Query parameter enum test (string array) | [optional] 
 **enum_query_string** | **STRING_32**| Query parameter enum test (string) | [optional] [default to -efg]
 **enum_query_integer** | **INTEGER_32**| Query parameter enum test (double) | [optional] 
 **enum_query_double** | **REAL_64**| Query parameter enum test (double) | [optional] 
 **enum_form_string_array** | **LIST [STRING_32]**| Form parameter enum test (string array) | [optional] [default to $]
 **enum_form_string** | **STRING_32**| Form parameter enum test (string) | [optional] [default to -efg]

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_inline_additional_properties**
> test_inline_additional_properties (request_body: STRING_TABLE[STRING_32] )
	

test inline additionalProperties


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **request_body** | [**STRING_TABLE[STRING_32]**](STRING_32.md)| request body | 

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_json_form_data**
> test_json_form_data (param: STRING_32 ; param2: STRING_32 )
	

test json serialization of form data


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **STRING_32**| field1 | [default to null]
 **param2** | **STRING_32**| field2 | [default to null]

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

