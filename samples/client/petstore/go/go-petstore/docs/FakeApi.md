# \FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestClientModel**](FakeApi.md#TestClientModel) | **Patch** /fake | To test \&quot;client\&quot; model
[**TestEndpointParameters**](FakeApi.md#TestEndpointParameters) | **Post** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**TestEnumParameters**](FakeApi.md#TestEnumParameters) | **Get** /fake | To test enum parameters


# **TestClientModel**
> Client TestClientModel(body)
To test \"client\" model

To test \"client\" model

### Required Parameters

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

# **TestEndpointParameters**
> TestEndpointParameters(ctx, number, double, patternWithoutDelimiter, byte_, optional)
Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context containing the authentication | nil if no authentication
  **number** | **float32**| None | 
  **double** | **float64**| None | 
  **patternWithoutDelimiter** | **string**| None | 
  **byte_** | **string**| None | 
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **float32**| None | 
 **double** | **float64**| None | 
 **patternWithoutDelimiter** | **string**| None | 
 **byte_** | **string**| None | 
 **integer** | **int32**| None | 
 **int32_** | **int32**| None | 
 **int64_** | **int64**| None | 
 **float** | **float32**| None | 
 **string_** | **string**| None | 
 **binary** | **string**| None | 
 **date** | **string**| None | 
 **dateTime** | **time.Time**| None | 
 **password** | **string**| None | 
 **callback** | **string**| None | 

### Return type

 (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: application/xml; charset=utf-8, application/json; charset=utf-8

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
 **enumFormStringArray** | [**[]string**](string.md)| Form parameter enum test (string array) | 
 **enumFormString** | **string**| Form parameter enum test (string) | [default to -efg]
 **enumHeaderStringArray** | [**[]string**](string.md)| Header parameter enum test (string array) | 
 **enumHeaderString** | **string**| Header parameter enum test (string) | [default to -efg]
 **enumQueryStringArray** | [**[]string**](string.md)| Query parameter enum test (string array) | 
 **enumQueryString** | **string**| Query parameter enum test (string) | [default to -efg]
 **enumQueryInteger** | **int32**| Query parameter enum test (double) | 
 **enumQueryDouble** | **float64**| Query parameter enum test (double) | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

