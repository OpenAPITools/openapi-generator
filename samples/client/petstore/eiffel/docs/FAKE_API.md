# FAKE_API

All URIs are relative to *http://petstore.swagger.io:80/v2*

Feature | HTTP request | Description
------------- | ------------- | -------------
[**fake_outer_boolean_serialize**](FAKE_API.md#fake_outer_boolean_serialize) | **Post** /fake/outer/boolean | 
[**fake_outer_composite_serialize**](FAKE_API.md#fake_outer_composite_serialize) | **Post** /fake/outer/composite | 
[**fake_outer_number_serialize**](FAKE_API.md#fake_outer_number_serialize) | **Post** /fake/outer/number | 
[**fake_outer_string_serialize**](FAKE_API.md#fake_outer_string_serialize) | **Post** /fake/outer/string | 
[**test_client_model**](FAKE_API.md#test_client_model) | **Patch** /fake | To test \&quot;client\&quot; model
[**test_endpoint_parameters**](FAKE_API.md#test_endpoint_parameters) | **Post** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**test_enum_parameters**](FAKE_API.md#test_enum_parameters) | **Get** /fake | To test enum parameters
[**test_json_form_data**](FAKE_API.md#test_json_form_data) | **Get** /fake/jsonFormData | test json serialization of form data


# **fake_outer_boolean_serialize**
> fake_outer_boolean_serialize (body:  detachable OUTER_BOOLEAN ): detachable OUTER_BOOLEAN
	



Test serialization of outer boolean types


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OUTER_BOOLEAN**](OUTER_BOOLEAN.md)| Input boolean as post body | [optional] 

### Return type

[**OUTER_BOOLEAN**](OuterBoolean.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_composite_serialize**
> fake_outer_composite_serialize (body:  detachable OUTER_COMPOSITE ): detachable OUTER_COMPOSITE
	



Test serialization of object with outer number type


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OUTER_COMPOSITE**](OUTER_COMPOSITE.md)| Input composite as post body | [optional] 

### Return type

[**OUTER_COMPOSITE**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_number_serialize**
> fake_outer_number_serialize (body:  detachable OUTER_NUMBER ): detachable OUTER_NUMBER
	



Test serialization of outer number types


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OUTER_NUMBER**](OUTER_NUMBER.md)| Input number as post body | [optional] 

### Return type

[**OUTER_NUMBER**](OuterNumber.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_string_serialize**
> fake_outer_string_serialize (body:  detachable OUTER_STRING ): detachable OUTER_STRING
	



Test serialization of outer string types


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OUTER_STRING**](OUTER_STRING.md)| Input string as post body | [optional] 

### Return type

[**OUTER_STRING**](OuterString.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_client_model**
> test_client_model (body: CLIENT ): detachable CLIENT
	

To test \"client\" model

To test \"client\" model


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**CLIENT**](CLIENT.md)| client model | 

### Return type

[**CLIENT**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_endpoint_parameters**
> test_endpoint_parameters (number: REAL_32 ; double: REAL_64 ; pattern_without_delimiter: STRING_32 ; byte: ARRAY [NATURAL_8] ; integer:  detachable INTEGER_32 ; int32:  detachable INTEGER_32 ; int64:  detachable INTEGER_64 ; float:  detachable REAL_32 ; string:  detachable STRING_32 ; binary:  detachable STRING_32 ; date:  detachable DATE ; date_time:  detachable DATE_TIME ; password:  detachable STRING_32 ; callback:  detachable STRING_32 )
	

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **REAL_32**| None | 
 **double** | **REAL_64**| None | 
 **pattern_without_delimiter** | **STRING_32**| None | 
 **byte** | **ARRAY [NATURAL_8]**| None | 
 **integer** | **INTEGER_32**| None | [optional] 
 **int32** | **INTEGER_32**| None | [optional] 
 **int64** | **INTEGER_64**| None | [optional] 
 **float** | **REAL_32**| None | [optional] 
 **string** | **STRING_32**| None | [optional] 
 **binary** | **STRING_32**| None | [optional] 
 **date** | **DATE**| None | [optional] 
 **date_time** | **DATE_TIME**| None | [optional] 
 **password** | **STRING_32**| None | [optional] 
 **callback** | **STRING_32**| None | [optional] 

### Return type

{empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: application/xml; charset=utf-8, application/json; charset=utf-8

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_enum_parameters**
> test_enum_parameters (enum_form_string_array:  detachable LIST [STRING_32] ; enum_form_string:  detachable STRING_32 ; enum_header_string_array:  detachable LIST [STRING_32] ; enum_header_string:  detachable STRING_32 ; enum_query_string_array:  detachable LIST [STRING_32] ; enum_query_string:  detachable STRING_32 ; enum_query_integer:  detachable INTEGER_32 ; enum_query_double:  detachable REAL_64 )
	

To test enum parameters

To test enum parameters


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_form_string_array** | [**LIST [STRING_32]**](STRING_32.md)| Form parameter enum test (string array) | [optional] 
 **enum_form_string** | **STRING_32**| Form parameter enum test (string) | [optional] [default to -efg]
 **enum_header_string_array** | [**LIST [STRING_32]**](STRING_32.md)| Header parameter enum test (string array) | [optional] 
 **enum_header_string** | **STRING_32**| Header parameter enum test (string) | [optional] [default to -efg]
 **enum_query_string_array** | [**LIST [STRING_32]**](STRING_32.md)| Query parameter enum test (string array) | [optional] 
 **enum_query_string** | **STRING_32**| Query parameter enum test (string) | [optional] [default to -efg]
 **enum_query_integer** | **INTEGER_32**| Query parameter enum test (double) | [optional] 
 **enum_query_double** | **REAL_64**| Query parameter enum test (double) | [optional] 

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_json_form_data**
> test_json_form_data (param: STRING_32 ; param2: STRING_32 )
	

test json serialization of form data




### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **STRING_32**| field1 | 
 **param2** | **STRING_32**| field2 | 

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

