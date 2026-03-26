# Org.OpenAPITools.Api.FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**FakeHealthGet**](FakeApi.md#fakehealthget) | **GET** /fake/health | Health check endpoint |
| [**FakeOuterBooleanSerialize**](FakeApi.md#fakeouterbooleanserialize) | **POST** /fake/outer/boolean |  |
| [**FakeOuterCompositeSerialize**](FakeApi.md#fakeoutercompositeserialize) | **POST** /fake/outer/composite |  |
| [**FakeOuterNumberSerialize**](FakeApi.md#fakeouternumberserialize) | **POST** /fake/outer/number |  |
| [**FakeOuterStringSerialize**](FakeApi.md#fakeouterstringserialize) | **POST** /fake/outer/string |  |
| [**GetArrayOfEnums**](FakeApi.md#getarrayofenums) | **GET** /fake/array-of-enums | Array of Enums |
| [**GetMixedAnyOf**](FakeApi.md#getmixedanyof) | **GET** /fake/mixed/anyOf | Test mixed type anyOf deserialization |
| [**GetMixedOneOf**](FakeApi.md#getmixedoneof) | **GET** /fake/mixed/oneOf | Test mixed type oneOf deserialization |
| [**TestAdditionalPropertiesReference**](FakeApi.md#testadditionalpropertiesreference) | **POST** /fake/additionalProperties-reference | test referenced additionalProperties |
| [**TestBodyWithFileSchema**](FakeApi.md#testbodywithfileschema) | **PUT** /fake/body-with-file-schema |  |
| [**TestBodyWithQueryParams**](FakeApi.md#testbodywithqueryparams) | **PUT** /fake/body-with-query-params |  |
| [**TestClientModel**](FakeApi.md#testclientmodel) | **PATCH** /fake | To test \&quot;client\&quot; model |
| [**TestEndpointParameters**](FakeApi.md#testendpointparameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트  |
| [**TestEnumParameters**](FakeApi.md#testenumparameters) | **GET** /fake | To test enum parameters |
| [**TestGroupParameters**](FakeApi.md#testgroupparameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional) |
| [**TestInlineAdditionalProperties**](FakeApi.md#testinlineadditionalproperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties |
| [**TestInlineFreeformAdditionalProperties**](FakeApi.md#testinlinefreeformadditionalproperties) | **POST** /fake/inline-freeform-additionalProperties | test inline free-form additionalProperties |
| [**TestJsonFormData**](FakeApi.md#testjsonformdata) | **GET** /fake/jsonFormData | test json serialization of form data |
| [**TestQueryParameterCollectionFormat**](FakeApi.md#testqueryparametercollectionformat) | **PUT** /fake/test-query-parameters |  |
| [**TestStringMapReference**](FakeApi.md#teststringmapreference) | **POST** /fake/stringMap-reference | test referenced string map |

<a id="fakehealthget"></a>
# **FakeHealthGet**
> HealthCheckResult FakeHealthGet ()

Health check endpoint


### Parameters
This endpoint does not need any parameter.
### Return type

[**HealthCheckResult**](HealthCheckResult.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | The instance started successfully |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="fakeouterbooleanserialize"></a>
# **FakeOuterBooleanSerialize**
> bool FakeOuterBooleanSerialize (bool body = null)



Test serialization of outer boolean types


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **body** | **bool** | Input boolean as post body | [optional]  |

### Return type

**bool**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output boolean |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="fakeoutercompositeserialize"></a>
# **FakeOuterCompositeSerialize**
> OuterComposite FakeOuterCompositeSerialize (OuterComposite outerComposite = null)



Test serialization of object with outer number type


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **outerComposite** | [**OuterComposite**](OuterComposite.md) | Input composite as post body | [optional]  |

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output composite |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="fakeouternumberserialize"></a>
# **FakeOuterNumberSerialize**
> decimal FakeOuterNumberSerialize (decimal body = null)



Test serialization of outer number types


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **body** | **decimal** | Input number as post body | [optional]  |

### Return type

**decimal**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output number |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="fakeouterstringserialize"></a>
# **FakeOuterStringSerialize**
> string FakeOuterStringSerialize (Guid requiredStringUuid, string body = null)



Test serialization of outer string types


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **requiredStringUuid** | **Guid** | Required UUID String |  |
| **body** | **string** | Input string as post body | [optional]  |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Output string |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="getarrayofenums"></a>
# **GetArrayOfEnums**
> List&lt;OuterEnum&gt; GetArrayOfEnums ()

Array of Enums


### Parameters
This endpoint does not need any parameter.
### Return type

[**List&lt;OuterEnum&gt;**](OuterEnum.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Got named array of enums |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="getmixedanyof"></a>
# **GetMixedAnyOf**
> MixedAnyOf GetMixedAnyOf ()

Test mixed type anyOf deserialization


### Parameters
This endpoint does not need any parameter.
### Return type

[**MixedAnyOf**](MixedAnyOf.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Got mixed anyOf |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="getmixedoneof"></a>
# **GetMixedOneOf**
> MixedOneOf GetMixedOneOf ()

Test mixed type oneOf deserialization


### Parameters
This endpoint does not need any parameter.
### Return type

[**MixedOneOf**](MixedOneOf.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Got mixed oneOf |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="testadditionalpropertiesreference"></a>
# **TestAdditionalPropertiesReference**
> void TestAdditionalPropertiesReference (Dictionary<string, Object> requestBody)

test referenced additionalProperties


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **requestBody** | [**Dictionary&lt;string, Object&gt;**](Object.md) | request body |  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="testbodywithfileschema"></a>
# **TestBodyWithFileSchema**
> void TestBodyWithFileSchema (FileSchemaTestClass fileSchemaTestClass)



For this test, the body for this request much reference a schema named `File`.


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **fileSchemaTestClass** | [**FileSchemaTestClass**](FileSchemaTestClass.md) |  |  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="testbodywithqueryparams"></a>
# **TestBodyWithQueryParams**
> void TestBodyWithQueryParams (string query, User user)




### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **query** | **string** |  |  |
| **user** | [**User**](User.md) |  |  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="testclientmodel"></a>
# **TestClientModel**
> ModelClient TestClientModel (ModelClient modelClient)

To test \"client\" model

To test \"client\" model


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **modelClient** | [**ModelClient**](ModelClient.md) | client model |  |

### Return type

[**ModelClient**](ModelClient.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="testendpointparameters"></a>
# **TestEndpointParameters**
> void TestEndpointParameters (decimal number, string patternWithoutDelimiter, byte[] varByte, double varDouble, System.IO.Stream binary = null, string callback = null, DateTime date = null, DateTime dateTime = null, int int32 = null, long int64 = null, int integer = null, string password = null, float varFloat = null, string varString = null)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **number** | **decimal** | None |  |
| **patternWithoutDelimiter** | **string** | None |  |
| **varByte** | **byte[]** | None |  |
| **varDouble** | **double** | None |  |
| **binary** | **System.IO.Stream****System.IO.Stream** | None | [optional]  |
| **callback** | **string** | None | [optional]  |
| **date** | **DateTime** | None | [optional]  |
| **dateTime** | **DateTime** | None | [optional] [default to &quot;2010-02-01T10:20:10.111110+01:00&quot;] |
| **int32** | **int** | None | [optional]  |
| **int64** | **long** | None | [optional]  |
| **integer** | **int** | None | [optional]  |
| **password** | **string** | None | [optional]  |
| **varFloat** | **float** | None | [optional]  |
| **varString** | **string** | None | [optional]  |

### Return type

void (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid username supplied |  -  |
| **404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="testenumparameters"></a>
# **TestEnumParameters**
> void TestEnumParameters (TestEnumParametersRequestEnumFormString enumFormString = null, List<TestEnumParametersRequestEnumFormStringArrayInner> enumFormStringArray = null, TestEnumParametersEnumHeaderStringParameter enumHeaderString = null, List<TestEnumParametersRequestEnumFormStringArrayInner> enumHeaderStringArray = null, TestEnumParametersEnumQueryDoubleParameter enumQueryDouble = null, TestEnumParametersEnumQueryIntegerParameter enumQueryInteger = null, TestEnumParametersEnumHeaderStringParameter enumQueryString = null, List<TestEnumParametersRequestEnumFormStringArrayInner> enumQueryStringArray = null)

To test enum parameters

To test enum parameters


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **enumFormString** | **TestEnumParametersRequestEnumFormString** |  | [optional]  |
| **enumFormStringArray** | [**List&lt;TestEnumParametersRequestEnumFormStringArrayInner&gt;**](TestEnumParametersRequestEnumFormStringArrayInner.md) | Form parameter enum test (string array) | [optional]  |
| **enumHeaderString** | **TestEnumParametersEnumHeaderStringParameter** | Header parameter enum test (string) | [optional]  |
| **enumHeaderStringArray** | [**List&lt;TestEnumParametersRequestEnumFormStringArrayInner&gt;**](TestEnumParametersRequestEnumFormStringArrayInner.md) | Header parameter enum test (string array) | [optional]  |
| **enumQueryDouble** | **TestEnumParametersEnumQueryDoubleParameter** | Query parameter enum test (double) | [optional]  |
| **enumQueryInteger** | **TestEnumParametersEnumQueryIntegerParameter** | Query parameter enum test (double) | [optional]  |
| **enumQueryString** | **TestEnumParametersEnumHeaderStringParameter** | Query parameter enum test (string) | [optional]  |
| **enumQueryStringArray** | [**List&lt;TestEnumParametersRequestEnumFormStringArrayInner&gt;**](TestEnumParametersRequestEnumFormStringArrayInner.md) | Query parameter enum test (string array) | [optional]  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid request |  -  |
| **404** | Not found |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="testgroupparameters"></a>
# **TestGroupParameters**
> void TestGroupParameters (bool requiredBooleanGroup, long requiredInt64Group, int requiredStringGroup, bool booleanGroup = null, long int64Group = null, int stringGroup = null)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **requiredBooleanGroup** | **bool** | Required Boolean in group parameters |  |
| **requiredInt64Group** | **long** | Required Integer in group parameters |  |
| **requiredStringGroup** | **int** | Required String in group parameters |  |
| **booleanGroup** | **bool** | Boolean in group parameters | [optional]  |
| **int64Group** | **long** | Integer in group parameters | [optional]  |
| **stringGroup** | **int** | String in group parameters | [optional]  |

### Return type

void (empty response body)

### Authorization

[bearer_test](../README.md#bearer_test)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Something wrong |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="testinlineadditionalproperties"></a>
# **TestInlineAdditionalProperties**
> void TestInlineAdditionalProperties (Dictionary<string, string> requestBody)

test inline additionalProperties


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **requestBody** | [**Dictionary&lt;string, string&gt;**](string.md) | request body |  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="testinlinefreeformadditionalproperties"></a>
# **TestInlineFreeformAdditionalProperties**
> void TestInlineFreeformAdditionalProperties (TestInlineFreeformAdditionalPropertiesRequest testInlineFreeformAdditionalPropertiesRequest)

test inline free-form additionalProperties


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **testInlineFreeformAdditionalPropertiesRequest** | [**TestInlineFreeformAdditionalPropertiesRequest**](TestInlineFreeformAdditionalPropertiesRequest.md) | request body |  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="testjsonformdata"></a>
# **TestJsonFormData**
> void TestJsonFormData (string param, string param2)

test json serialization of form data


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **param** | **string** | field1 |  |
| **param2** | **string** | field2 |  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="testqueryparametercollectionformat"></a>
# **TestQueryParameterCollectionFormat**
> void TestQueryParameterCollectionFormat (List<string> context, List<string> http, List<string> ioutil, List<string> pipe, string requiredNotNullable, List<string> url, string notRequiredNotNullable = null, string notRequiredNullable = null, string requiredNullable)



To test the collection format in query parameters


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **context** | [**List&lt;string&gt;**](string.md) |  |  |
| **http** | [**List&lt;string&gt;**](string.md) |  |  |
| **ioutil** | [**List&lt;string&gt;**](string.md) |  |  |
| **pipe** | [**List&lt;string&gt;**](string.md) |  |  |
| **requiredNotNullable** | **string** |  |  |
| **url** | [**List&lt;string&gt;**](string.md) |  |  |
| **notRequiredNotNullable** | **string** |  | [optional]  |
| **notRequiredNullable** | **string** |  | [optional]  |
| **requiredNullable** | **string** |  |  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="teststringmapreference"></a>
# **TestStringMapReference**
> void TestStringMapReference (Dictionary<string, string> requestBody)

test referenced string map


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **requestBody** | [**Dictionary&lt;string, string&gt;**](string.md) | request body |  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

