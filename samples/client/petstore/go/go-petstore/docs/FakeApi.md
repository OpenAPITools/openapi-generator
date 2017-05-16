# \FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**FakeOuterBooleanSerialize**](FakeApi.md#FakeOuterBooleanSerialize) | **Post** /fake/outer/boolean | 
[**FakeOuterCompositeSerialize**](FakeApi.md#FakeOuterCompositeSerialize) | **Post** /fake/outer/composite | 
[**FakeOuterNumberSerialize**](FakeApi.md#FakeOuterNumberSerialize) | **Post** /fake/outer/number | 
[**FakeOuterStringSerialize**](FakeApi.md#FakeOuterStringSerialize) | **Post** /fake/outer/string | 
[**TestClientModel**](FakeApi.md#TestClientModel) | **Patch** /fake | To test \&quot;client\&quot; model
[**TestEndpointParameters**](FakeApi.md#TestEndpointParameters) | **Post** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**TestEnumParameters**](FakeApi.md#TestEnumParameters) | **Get** /fake | To test enum parameters


# **FakeOuterBooleanSerialize**
> OuterBoolean FakeOuterBooleanSerialize($body)



Test serialization of outer boolean types


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

# **FakeOuterCompositeSerialize**
> OuterComposite FakeOuterCompositeSerialize($body)



Test serialization of object with outer number type


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

# **FakeOuterNumberSerialize**
> OuterNumber FakeOuterNumberSerialize($body)



Test serialization of outer number types


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

# **FakeOuterStringSerialize**
> OuterString FakeOuterStringSerialize($body)



Test serialization of outer string types


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

# **TestClientModel**
> Client TestClientModel($body)

To test \"client\" model

To test \"client\" model


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

# **TestEndpointParameters**
> TestEndpointParameters($number, $double, $patternWithoutDelimiter, $byte_, $integer, $int32_, $int64_, $float, $string_, $binary, $date, $dateTime, $password, $callback)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **float32**| None | 
 **double** | **float64**| None | 
 **patternWithoutDelimiter** | **string**| None | 
 **byte_** | **string**| None | 
 **integer** | **int32**| None | [optional] 
 **int32_** | **int32**| None | [optional] 
 **int64_** | **int64**| None | [optional] 
 **float** | **float32**| None | [optional] 
 **string_** | **string**| None | [optional] 
 **binary** | **string**| None | [optional] 
 **date** | **time.Time**| None | [optional] 
 **dateTime** | **time.Time**| None | [optional] 
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

# **TestEnumParameters**
> TestEnumParameters($enumFormStringArray, $enumFormString, $enumHeaderStringArray, $enumHeaderString, $enumQueryStringArray, $enumQueryString, $enumQueryInteger, $enumQueryDouble)

To test enum parameters

To test enum parameters


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumFormStringArray** | [**[]string**](string.md)| Form parameter enum test (string array) | [optional] 
 **enumFormString** | **string**| Form parameter enum test (string) | [optional] [default to -efg]
 **enumHeaderStringArray** | [**[]string**](string.md)| Header parameter enum test (string array) | [optional] 
 **enumHeaderString** | **string**| Header parameter enum test (string) | [optional] [default to -efg]
 **enumQueryStringArray** | [**[]string**](string.md)| Query parameter enum test (string array) | [optional] 
 **enumQueryString** | **string**| Query parameter enum test (string) | [optional] [default to -efg]
 **enumQueryInteger** | **int32**| Query parameter enum test (double) | [optional] 
 **enumQueryDouble** | **float64**| Query parameter enum test (double) | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

