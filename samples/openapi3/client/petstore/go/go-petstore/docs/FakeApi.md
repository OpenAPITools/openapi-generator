# \FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**FakeHealthGet**](FakeApi.md#FakeHealthGet) | **Get** /fake/health | Health check endpoint
[**FakeOuterBooleanSerialize**](FakeApi.md#FakeOuterBooleanSerialize) | **Post** /fake/outer/boolean | 
[**FakeOuterCompositeSerialize**](FakeApi.md#FakeOuterCompositeSerialize) | **Post** /fake/outer/composite | 
[**FakeOuterNumberSerialize**](FakeApi.md#FakeOuterNumberSerialize) | **Post** /fake/outer/number | 
[**FakeOuterStringSerialize**](FakeApi.md#FakeOuterStringSerialize) | **Post** /fake/outer/string | 
[**TestBodyWithFileSchema**](FakeApi.md#TestBodyWithFileSchema) | **Put** /fake/body-with-file-schema | 
[**TestBodyWithQueryParams**](FakeApi.md#TestBodyWithQueryParams) | **Put** /fake/body-with-query-params | 
[**TestClientModel**](FakeApi.md#TestClientModel) | **Patch** /fake | To test \&quot;client\&quot; model
[**TestEndpointParameters**](FakeApi.md#TestEndpointParameters) | **Post** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**TestEnumParameters**](FakeApi.md#TestEnumParameters) | **Get** /fake | To test enum parameters
[**TestGroupParameters**](FakeApi.md#TestGroupParameters) | **Delete** /fake | Fake endpoint to test group parameters (optional)
[**TestInlineAdditionalProperties**](FakeApi.md#TestInlineAdditionalProperties) | **Post** /fake/inline-additionalProperties | test inline additionalProperties
[**TestJsonFormData**](FakeApi.md#TestJsonFormData) | **Get** /fake/jsonFormData | test json serialization of form data


# **FakeHealthGet**
> HealthCheckResult FakeHealthGet(ctx, )
Health check endpoint

### Required Parameters
This endpoint does not need any parameter.

### Return type

[**HealthCheckResult**](HealthCheckResult.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **FakeOuterBooleanSerialize**
> bool FakeOuterBooleanSerialize(ctx, optional)


Test serialization of outer boolean types

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
 **optional** | ***FakeOuterBooleanSerializeOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a FakeOuterBooleanSerializeOpts struct

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **optional.Bool**| Input boolean as post body | 

### Return type

**bool**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **FakeOuterCompositeSerialize**
> OuterComposite FakeOuterCompositeSerialize(ctx, optional)


Test serialization of object with outer number type

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
 **optional** | ***FakeOuterCompositeSerializeOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a FakeOuterCompositeSerializeOpts struct

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outerComposite** | [**optional.Interface of OuterComposite**](OuterComposite.md)| Input composite as post body | 

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **FakeOuterNumberSerialize**
> float32 FakeOuterNumberSerialize(ctx, optional)


Test serialization of outer number types

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
 **optional** | ***FakeOuterNumberSerializeOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a FakeOuterNumberSerializeOpts struct

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **optional.Float32**| Input number as post body | 

### Return type

**float32**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **FakeOuterStringSerialize**
> string FakeOuterStringSerialize(ctx, optional)


Test serialization of outer string types

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
 **optional** | ***FakeOuterStringSerializeOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a FakeOuterStringSerializeOpts struct

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **optional.String**| Input string as post body | 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestBodyWithFileSchema**
> TestBodyWithFileSchema(ctx, fileSchemaTestClass)


For this test, the body for this request much reference a schema named `File`.

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **fileSchemaTestClass** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestBodyWithQueryParams**
> TestBodyWithQueryParams(ctx, query, user)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **query** | **string**|  | 
  **user** | [**User**](User.md)|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestClientModel**
> Client TestClientModel(ctx, client)
To test \"client\" model

To test \"client\" model

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **client** | [**Client**](Client.md)| client model | 

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
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **number** | **float32**| None | 
  **double** | **float64**| None | 
  **patternWithoutDelimiter** | **string**| None | 
  **byte_** | **string**| None | 
 **optional** | ***TestEndpointParametersOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a TestEndpointParametersOpts struct

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------




 **integer** | **optional.Int32**| None | 
 **int32_** | **optional.Int32**| None | 
 **int64_** | **optional.Int64**| None | 
 **float** | **optional.Float32**| None | 
 **string_** | **optional.String**| None | 
 **binary** | **optional.Interface of *os.File****optional.*os.File**| None | 
 **date** | **optional.String**| None | 
 **dateTime** | **optional.Time**| None | 
 **password** | **optional.String**| None | 
 **callback** | **optional.String**| None | 

### Return type

 (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestEnumParameters**
> TestEnumParameters(ctx, optional)
To test enum parameters

To test enum parameters

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
 **optional** | ***TestEnumParametersOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a TestEnumParametersOpts struct

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | [**optional.Interface of []string**](string.md)| Header parameter enum test (string array) | 
 **enumHeaderString** | **optional.String**| Header parameter enum test (string) | [default to -efg]
 **enumQueryStringArray** | [**optional.Interface of []string**](string.md)| Query parameter enum test (string array) | 
 **enumQueryString** | **optional.String**| Query parameter enum test (string) | [default to -efg]
 **enumQueryInteger** | **optional.Int32**| Query parameter enum test (double) | 
 **enumQueryDouble** | **optional.Float64**| Query parameter enum test (double) | 
 **enumFormStringArray** | [**optional.Interface of []string**](string.md)| Form parameter enum test (string array) | [default to $]
 **enumFormString** | **optional.String**| Form parameter enum test (string) | [default to -efg]

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestGroupParameters**
> TestGroupParameters(ctx, requiredStringGroup, requiredBooleanGroup, requiredInt64Group, optional)
Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **requiredStringGroup** | **int32**| Required String in group parameters | 
  **requiredBooleanGroup** | **bool**| Required Boolean in group parameters | 
  **requiredInt64Group** | **int64**| Required Integer in group parameters | 
 **optional** | ***TestGroupParametersOpts** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a pointer to a TestGroupParametersOpts struct

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------



 **stringGroup** | **optional.Int32**| String in group parameters | 
 **booleanGroup** | **optional.Bool**| Boolean in group parameters | 
 **int64Group** | **optional.Int64**| Integer in group parameters | 

### Return type

 (empty response body)

### Authorization

[bearer_test](../README.md#bearer_test)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestInlineAdditionalProperties**
> TestInlineAdditionalProperties(ctx, requestBody)
test inline additionalProperties

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **requestBody** | [**map[string]string**](string.md)| request body | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **TestJsonFormData**
> TestJsonFormData(ctx, param, param2)
test json serialization of form data

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
  **param** | **string**| field1 | 
  **param2** | **string**| field2 | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

