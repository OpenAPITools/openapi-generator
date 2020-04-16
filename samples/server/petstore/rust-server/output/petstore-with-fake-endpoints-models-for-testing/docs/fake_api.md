# fake_api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
**Call123example**](fake_api.md#Call123example) | **GET** /fake/operation-with-numeric-id | 
**FakeOuterBooleanSerialize**](fake_api.md#FakeOuterBooleanSerialize) | **POST** /fake/outer/boolean | 
**FakeOuterCompositeSerialize**](fake_api.md#FakeOuterCompositeSerialize) | **POST** /fake/outer/composite | 
**FakeOuterNumberSerialize**](fake_api.md#FakeOuterNumberSerialize) | **POST** /fake/outer/number | 
**FakeOuterStringSerialize**](fake_api.md#FakeOuterStringSerialize) | **POST** /fake/outer/string | 
**FakeResponseWithNumericalDescription**](fake_api.md#FakeResponseWithNumericalDescription) | **GET** /fake/response-with-numerical-description | 
**HyphenParam**](fake_api.md#HyphenParam) | **GET** /fake/hyphenParam/{hyphen-param} | 
**TestBodyWithQueryParams**](fake_api.md#TestBodyWithQueryParams) | **PUT** /fake/body-with-query-params | 
**TestClientModel**](fake_api.md#TestClientModel) | **PATCH** /fake | To test \"client\" model
**TestEndpointParameters**](fake_api.md#TestEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
**TestEnumParameters**](fake_api.md#TestEnumParameters) | **GET** /fake | To test enum parameters
**TestInlineAdditionalProperties**](fake_api.md#TestInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
**TestJsonFormData**](fake_api.md#TestJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data


# **Call123example**
> Call123example()


### Required Parameters
This endpoint does not need any parameter.

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **FakeOuterBooleanSerialize**
> bool FakeOuterBooleanSerialize(optional)


Test serialization of outer boolean types

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**boolean**](boolean.md)| Input boolean as post body | 

### Return type

[**bool**](boolean.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **FakeOuterCompositeSerialize**
> models::OuterComposite FakeOuterCompositeSerialize(optional)


Test serialization of object with outer number type

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | 

### Return type

[**models::OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **FakeOuterNumberSerialize**
> f64 FakeOuterNumberSerialize(optional)


Test serialization of outer number types

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**number**](number.md)| Input number as post body | 

### Return type

[**f64**](number.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **FakeOuterStringSerialize**
> String FakeOuterStringSerialize(optional)


Test serialization of outer string types

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**string**](string.md)| Input string as post body | 

### Return type

[**String**](string.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **FakeResponseWithNumericalDescription**
> FakeResponseWithNumericalDescription()


### Required Parameters
This endpoint does not need any parameter.

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **HyphenParam**
> HyphenParam(hyphen_param)


To test hyphen in path parameter name

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **hyphen_param** | **String**| Parameter with hyphen in name | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestBodyWithQueryParams**
> TestBodyWithQueryParams(query, body)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **query** | **String**|  | 
  **body** | [**User**](User.md)|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestClientModel**
> models::Client TestClientModel(body)
To test \"client\" model

To test \"client\" model

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **body** | [**Client**](Client.md)| client model | 

### Return type

[**models::Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestEndpointParameters**
> TestEndpointParameters(ctx, number, double, pattern_without_delimiter, byte, optional)
Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context containing the authentication | nil if no authentication
  **number** | **f64**| None | 
  **double** | **f64**| None | 
  **pattern_without_delimiter** | **String**| None | 
  **byte** | **swagger::ByteArray**| None | 
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **f64**| None | 
 **double** | **f64**| None | 
 **pattern_without_delimiter** | **String**| None | 
 **byte** | **swagger::ByteArray**| None | 
 **integer** | **i32**| None | 
 **int32** | **i32**| None | 
 **int64** | **i64**| None | 
 **float** | **f32**| None | 
 **string** | **String**| None | 
 **binary** | **swagger::ByteArray**| None | 
 **date** | **chrono::DateTime::<chrono::Utc>**| None | 
 **date_time** | **chrono::DateTime::<chrono::Utc>**| None | 
 **password** | **String**| None | 
 **callback** | **String**| None | 

### Return type

 (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestEnumParameters**
> TestEnumParameters(optional)
To test enum parameters

To test enum parameters

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_header_string_array** | [**String**](String.md)| Header parameter enum test (string array) | 
 **enum_header_string** | **String**| Header parameter enum test (string) | [default to "-efg".to_string()]
 **enum_query_string_array** | [**String**](String.md)| Query parameter enum test (string array) | 
 **enum_query_string** | **String**| Query parameter enum test (string) | [default to "-efg".to_string()]
 **enum_query_integer** | **i32**| Query parameter enum test (double) | 
 **enum_query_double** | **f64**| Query parameter enum test (double) | 
 **enum_form_string** | **String**| Form parameter enum test (string) | [default to "-efg".to_string()]

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestInlineAdditionalProperties**
> TestInlineAdditionalProperties(param)
test inline additionalProperties

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **param** | [**string**](string.md)| request body | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestJsonFormData**
> TestJsonFormData(param, param2)
test json serialization of form data

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **param** | **String**| field1 | 
  **param2** | **String**| field2 | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

