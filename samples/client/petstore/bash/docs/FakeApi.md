# FakeApi

All URIs are relative to */v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** /fake/outer/boolean | 
[**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** /fake/outer/composite | 
[**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **POST** /fake/outer/number | 
[**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **POST** /fake/outer/string | 
[**testClientModel**](FakeApi.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters
假端點
偽のエンドポイント
가짜 엔드 포인트
[**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** /fake | To test enum parameters
[**testInlineAdditionalProperties**](FakeApi.md#testInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData**](FakeApi.md#testJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data


## **fakeOuterBooleanSerialize**



Test serialization of outer boolean types

### Example
```bash
petstore-cli fakeOuterBooleanSerialize
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterBoolean**](OuterBoolean.md) | Input boolean as post body | [optional]

### Return type

[**OuterBoolean**](OuterBoolean.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not Applicable
 - **Accept**: Not Applicable

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **fakeOuterCompositeSerialize**



Test serialization of object with outer number type

### Example
```bash
petstore-cli fakeOuterCompositeSerialize
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterComposite**](OuterComposite.md) | Input composite as post body | [optional]

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not Applicable
 - **Accept**: Not Applicable

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **fakeOuterNumberSerialize**



Test serialization of outer number types

### Example
```bash
petstore-cli fakeOuterNumberSerialize
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterNumber**](OuterNumber.md) | Input number as post body | [optional]

### Return type

[**OuterNumber**](OuterNumber.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not Applicable
 - **Accept**: Not Applicable

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **fakeOuterStringSerialize**



Test serialization of outer string types

### Example
```bash
petstore-cli fakeOuterStringSerialize
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterString**](OuterString.md) | Input string as post body | [optional]

### Return type

[**OuterString**](OuterString.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not Applicable
 - **Accept**: Not Applicable

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **testClientModel**

To test \"client\" model

To test \"client\" model

### Example
```bash
petstore-cli testClientModel
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md) | client model |

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **testEndpointParameters**

Fake endpoint for testing various parameters
假端點
偽のエンドポイント
가짜 엔드 포인트

Fake endpoint for testing various parameters
假端點
偽のエンドポイント
가짜 엔드 포인트

### Example
```bash
petstore-cli testEndpointParameters
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **integer** | None |
 **double** | **float** | None |
 **patternWithoutDelimiter** | **string** | None |
 **byte** | **string** | None |
 **integer** | **integer** | None | [optional]
 **int32** | **integer** | None | [optional]
 **int64** | **integer** | None | [optional]
 **float** | **float** | None | [optional]
 **string** | **string** | None | [optional]
 **binary** | **binary** | None | [optional]
 **date** | **string** | None | [optional]
 **dateTime** | **string** | None | [optional]
 **password** | **string** | None | [optional]
 **callback** | **string** | None | [optional]

### Return type

(empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: application/xml; charset=utf-8, application/json; charset=utf-8

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **testEnumParameters**

To test enum parameters

To test enum parameters

### Example
```bash
petstore-cli testEnumParameters enum_header_string_array:value enum_header_string:value  Specify as:   enum_query_string=value  enum_query_integer=value
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumFormStringArray** | [**array[string]**](string.md) | Form parameter enum test (string array) | [optional]
 **enumFormString** | **string** | Form parameter enum test (string) | [optional] [default to -efg]
 **enumHeaderStringArray** | [**array[string]**](string.md) | Header parameter enum test (string array) | [optional]
 **enumHeaderString** | **string** | Header parameter enum test (string) | [optional] [default to -efg]
 **enumQueryStringArray** | [**array[string]**](string.md) | Query parameter enum test (string array) | [optional]
 **enumQueryString** | **string** | Query parameter enum test (string) | [optional] [default to -efg]
 **enumQueryInteger** | **integer** | Query parameter enum test (double) | [optional]
 **enumQueryDouble** | **float** | Query parameter enum test (double) | [optional]

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **testInlineAdditionalProperties**

test inline additionalProperties



### Example
```bash
petstore-cli testInlineAdditionalProperties
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **map** | request body |

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not Applicable

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

## **testJsonFormData**

test json serialization of form data



### Example
```bash
petstore-cli testJsonFormData
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **string** | field1 |
 **param2** | **string** | field2 |

### Return type

(empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not Applicable

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

