# FAKE_API

All URIs are relative to *http://petstore.swagger.io:80/v2*

Feature | HTTP request | Description
------------- | ------------- | -------------
[**create_xml_item**](FAKE_API.md#create_xml_item) | **Post** /fake/create_xml_item | creates an XmlItem
[**fake_outer_boolean_serialize**](FAKE_API.md#fake_outer_boolean_serialize) | **Post** /fake/outer/boolean | 
[**fake_outer_composite_serialize**](FAKE_API.md#fake_outer_composite_serialize) | **Post** /fake/outer/composite | 
[**fake_outer_number_serialize**](FAKE_API.md#fake_outer_number_serialize) | **Post** /fake/outer/number | 
[**fake_outer_string_serialize**](FAKE_API.md#fake_outer_string_serialize) | **Post** /fake/outer/string | 
[**test_body_with_file_schema**](FAKE_API.md#test_body_with_file_schema) | **Put** /fake/body-with-file-schema | 
[**test_body_with_query_params**](FAKE_API.md#test_body_with_query_params) | **Put** /fake/body-with-query-params | 
[**test_client_model**](FAKE_API.md#test_client_model) | **Patch** /fake | To test \&quot;client\&quot; model
[**test_endpoint_parameters**](FAKE_API.md#test_endpoint_parameters) | **Post** /fake | Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
[**test_enum_parameters**](FAKE_API.md#test_enum_parameters) | **Get** /fake | To test enum parameters
[**test_group_parameters**](FAKE_API.md#test_group_parameters) | **Delete** /fake | Fake endpoint to test group parameters (optional)
[**test_inline_additional_properties**](FAKE_API.md#test_inline_additional_properties) | **Post** /fake/inline-additionalProperties | test inline additionalProperties
[**test_json_form_data**](FAKE_API.md#test_json_form_data) | **Get** /fake/jsonFormData | test json serialization of form data
[**test_query_parameter_collection_format**](FAKE_API.md#test_query_parameter_collection_format) | **Put** /fake/test-query-parameters | 


# **create_xml_item**
> create_xml_item (xml_item: XML_ITEM )
	

creates an XmlItem

this route creates an XmlItem


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xml_item** | [**XML_ITEM**](XML_ITEM.md)| XmlItem Body | 

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml, application/xml; charset=utf-8, application/xml; charset=utf-16, text/xml, text/xml; charset=utf-8, text/xml; charset=utf-16
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

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

# **test_body_with_file_schema**
> test_body_with_file_schema (body: FILE_SCHEMA_TEST_CLASS )
	



For this test, the body for this request much reference a schema named `File`.


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**FILE_SCHEMA_TEST_CLASS**](FILE_SCHEMA_TEST_CLASS.md)|  | 

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_body_with_query_params**
> test_body_with_query_params (query: STRING_32 ; body: USER )
	




### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **STRING_32**|  | [default to null]
 **body** | [**USER**](USER.md)|  | 

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
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
> test_endpoint_parameters (number: REAL_32 ; double: REAL_64 ; pattern_without_delimiter: STRING_32 ; byte: ARRAY [NATURAL_8] ; integer:  detachable INTEGER_32 ; int32:  detachable INTEGER_32 ; int64:  detachable INTEGER_64 ; float:  detachable REAL_32 ; string:  detachable STRING_32 ; binary:  detachable FILE ; date:  detachable DATE ; date_time:  detachable DATE_TIME ; password:  detachable STRING ; callback:  detachable STRING_32 )
	

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트


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
 **password** | **STRING**| None | [optional] [default to null]
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
 **enum_header_string_array** | [**LIST [STRING_32]**](STRING_32.md)| Header parameter enum test (string array) | [optional] [default to null]
 **enum_header_string** | **STRING_32**| Header parameter enum test (string) | [optional] [default to -efg]
 **enum_query_string_array** | [**LIST [STRING_32]**](STRING_32.md)| Query parameter enum test (string array) | [optional] [default to null]
 **enum_query_string** | **STRING_32**| Query parameter enum test (string) | [optional] [default to -efg]
 **enum_query_integer** | **INTEGER_32**| Query parameter enum test (double) | [optional] [default to null]
 **enum_query_double** | **REAL_64**| Query parameter enum test (double) | [optional] [default to null]
 **enum_form_string_array** | [**LIST [STRING_32]**](STRING_32.md)| Form parameter enum test (string array) | [optional] [default to $]
 **enum_form_string** | **STRING_32**| Form parameter enum test (string) | [optional] [default to -efg]

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_group_parameters**
> test_group_parameters (required_string_group: INTEGER_32 ; required_boolean_group: BOOLEAN ; required_int64_group: INTEGER_64 ; string_group:  detachable INTEGER_32 ; boolean_group:  detachable BOOLEAN ; int64_group:  detachable INTEGER_64 )
	

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **required_string_group** | **INTEGER_32**| Required String in group parameters | [default to null]
 **required_boolean_group** | **BOOLEAN**| Required Boolean in group parameters | [default to null]
 **required_int64_group** | **INTEGER_64**| Required Integer in group parameters | [default to null]
 **string_group** | **INTEGER_32**| String in group parameters | [optional] [default to null]
 **boolean_group** | **BOOLEAN**| Boolean in group parameters | [optional] [default to null]
 **int64_group** | **INTEGER_64**| Integer in group parameters | [optional] [default to null]

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_inline_additional_properties**
> test_inline_additional_properties (param: STRING_TABLE [STRING_32] )
	

test inline additionalProperties


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | [**STRING_TABLE [STRING_32]**](STRING_32.md)| request body | 

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

# **test_query_parameter_collection_format**
> test_query_parameter_collection_format (pipe: LIST [STRING_32] ; ioutil: LIST [STRING_32] ; http: LIST [STRING_32] ; url: LIST [STRING_32] ; context: LIST [STRING_32] )
	



To test the collection format in query parameters


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**LIST [STRING_32]**](STRING_32.md)|  | [default to null]
 **ioutil** | [**LIST [STRING_32]**](STRING_32.md)|  | [default to null]
 **http** | [**LIST [STRING_32]**](STRING_32.md)|  | [default to null]
 **url** | [**LIST [STRING_32]**](STRING_32.md)|  | [default to null]
 **context** | [**LIST [STRING_32]**](STRING_32.md)|  | [default to null]

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

