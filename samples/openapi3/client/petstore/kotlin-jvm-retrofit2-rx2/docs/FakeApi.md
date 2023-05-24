# FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fakeHealthGet**](FakeApi.md#fakeHealthGet) | **GET** fake/health | Health check endpoint
[**fakeHttpSignatureTest**](FakeApi.md#fakeHttpSignatureTest) | **GET** fake/http-signature-test | test http signature authentication
[**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** fake/outer/boolean | 
[**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** fake/outer/composite | 
[**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **POST** fake/outer/number | 
[**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **POST** fake/outer/string | 
[**testBodyWithFileSchema**](FakeApi.md#testBodyWithFileSchema) | **PUT** fake/body-with-file-schema | 
[**testBodyWithQueryParams**](FakeApi.md#testBodyWithQueryParams) | **PUT** fake/body-with-query-params | 
[**testClientModel**](FakeApi.md#testClientModel) | **PATCH** fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** fake | To test enum parameters
[**testGroupParameters**](FakeApi.md#testGroupParameters) | **DELETE** fake | Fake endpoint to test group parameters (optional)
[**testInlineAdditionalProperties**](FakeApi.md#testInlineAdditionalProperties) | **POST** fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData**](FakeApi.md#testJsonFormData) | **GET** fake/jsonFormData | test json serialization of form data
[**testQueryParameterCollectionFormat**](FakeApi.md#testQueryParameterCollectionFormat) | **PUT** fake/test-query-parameters | 



Health check endpoint

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FakeApi::class.java)

val result : HealthCheckResult = webService.fakeHealthGet()
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


test http signature authentication

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FakeApi::class.java)
val pet : Pet =  // Pet | Pet object that needs to be added to the store
val query1 : kotlin.String = query1_example // kotlin.String | query parameter
val header1 : kotlin.String = header1_example // kotlin.String | header parameter

webService.fakeHttpSignatureTest(pet, query1, header1)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store |
 **query1** | **kotlin.String**| query parameter | [optional]
 **header1** | **kotlin.String**| header parameter | [optional]

### Return type

null (empty response body)

### Authorization



### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined




Test serialization of outer boolean types

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FakeApi::class.java)
val body : kotlin.Boolean = true // kotlin.Boolean | Input boolean as post body

val result : kotlin.Boolean = webService.fakeOuterBooleanSerialize(body)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **kotlin.Boolean**| Input boolean as post body | [optional]

### Return type

**kotlin.Boolean**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*




Test serialization of object with outer number type

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FakeApi::class.java)
val outerComposite : OuterComposite =  // OuterComposite | Input composite as post body

val result : OuterComposite = webService.fakeOuterCompositeSerialize(outerComposite)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outerComposite** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional]

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*




Test serialization of outer number types

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FakeApi::class.java)
val body : java.math.BigDecimal = 8.14 // java.math.BigDecimal | Input number as post body

val result : java.math.BigDecimal = webService.fakeOuterNumberSerialize(body)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **java.math.BigDecimal**| Input number as post body | [optional]

### Return type

[**java.math.BigDecimal**](java.math.BigDecimal.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*




Test serialization of outer string types

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FakeApi::class.java)
val body : kotlin.String = body_example // kotlin.String | Input string as post body

val result : kotlin.String = webService.fakeOuterStringSerialize(body)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **kotlin.String**| Input string as post body | [optional]

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*




For this test, the body for this request much reference a schema named &#x60;File&#x60;.

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FakeApi::class.java)
val fileSchemaTestClass : FileSchemaTestClass =  // FileSchemaTestClass | 

webService.testBodyWithFileSchema(fileSchemaTestClass)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **fileSchemaTestClass** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined




### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FakeApi::class.java)
val query : kotlin.String = query_example // kotlin.String | 
val user : User =  // User | 

webService.testBodyWithQueryParams(query, user)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **kotlin.String**|  |
 **user** | [**User**](User.md)|  |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


To test \&quot;client\&quot; model

To test \&quot;client\&quot; model

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FakeApi::class.java)
val client : Client =  // Client | client model

val result : Client = webService.testClientModel(client)
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


Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
apiClient.setCredentials("USERNAME", "PASSWORD")
val webService = apiClient.createWebservice(FakeApi::class.java)
val number : java.math.BigDecimal = 8.14 // java.math.BigDecimal | None
val double : kotlin.Double = 1.2 // kotlin.Double | None
val patternWithoutDelimiter : kotlin.String = patternWithoutDelimiter_example // kotlin.String | None
val byte : kotlin.ByteArray = BYTE_ARRAY_DATA_HERE // kotlin.ByteArray | None
val integer : kotlin.Int = 56 // kotlin.Int | None
val int32 : kotlin.Int = 56 // kotlin.Int | None
val int64 : kotlin.Long = 789 // kotlin.Long | None
val float : kotlin.Float = 3.4 // kotlin.Float | None
val string : kotlin.String = string_example // kotlin.String | None
val binary : java.io.File = BINARY_DATA_HERE // java.io.File | None
val date : java.time.LocalDate = 2013-10-20 // java.time.LocalDate | None
val dateTime : java.time.OffsetDateTime = 2013-10-20T19:20:30+01:00 // java.time.OffsetDateTime | None
val password : kotlin.String = password_example // kotlin.String | None
val paramCallback : kotlin.String = paramCallback_example // kotlin.String | None

webService.testEndpointParameters(number, double, patternWithoutDelimiter, byte, integer, int32, int64, float, string, binary, date, dateTime, password, paramCallback)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **java.math.BigDecimal**| None |
 **double** | **kotlin.Double**| None |
 **patternWithoutDelimiter** | **kotlin.String**| None |
 **byte** | **kotlin.ByteArray**| None |
 **integer** | **kotlin.Int**| None | [optional]
 **int32** | **kotlin.Int**| None | [optional]
 **int64** | **kotlin.Long**| None | [optional]
 **float** | **kotlin.Float**| None | [optional]
 **string** | **kotlin.String**| None | [optional]
 **binary** | **java.io.File**| None | [optional]
 **date** | **java.time.LocalDate**| None | [optional]
 **dateTime** | **java.time.OffsetDateTime**| None | [optional]
 **password** | **kotlin.String**| None | [optional]
 **paramCallback** | **kotlin.String**| None | [optional]

### Return type

null (empty response body)

### Authorization


Configure http_basic_test:
    ApiClient().setCredentials("USERNAME", "PASSWORD")

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined


To test enum parameters

To test enum parameters

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FakeApi::class.java)
val enumHeaderStringArray : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | Header parameter enum test (string array)
val enumHeaderString : kotlin.String = enumHeaderString_example // kotlin.String | Header parameter enum test (string)
val enumQueryStringArray : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | Query parameter enum test (string array)
val enumQueryString : kotlin.String = enumQueryString_example // kotlin.String | Query parameter enum test (string)
val enumQueryInteger : kotlin.Int = 56 // kotlin.Int | Query parameter enum test (double)
val enumQueryDouble : kotlin.Double = 1.2 // kotlin.Double | Query parameter enum test (double)
val enumFormStringArray : kotlin.collections.List<kotlin.String> = enumFormStringArray_example // kotlin.collections.List<kotlin.String> | Form parameter enum test (string array)
val enumFormString : kotlin.String = enumFormString_example // kotlin.String | Form parameter enum test (string)

webService.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)| Header parameter enum test (string array) | [optional] [enum: >, $]
 **enumHeaderString** | **kotlin.String**| Header parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)]
 **enumQueryStringArray** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)| Query parameter enum test (string array) | [optional] [enum: >, $]
 **enumQueryString** | **kotlin.String**| Query parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)]
 **enumQueryInteger** | **kotlin.Int**| Query parameter enum test (double) | [optional] [enum: 1, -2]
 **enumQueryDouble** | **kotlin.Double**| Query parameter enum test (double) | [optional] [enum: 1.1, -1.2]
 **enumFormStringArray** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)| Form parameter enum test (string array) | [optional] [default to $] [enum: >, $]
 **enumFormString** | **kotlin.String**| Form parameter enum test (string) | [optional] [default to -efg] [enum: _abc, -efg, (xyz)]

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined


Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
apiClient.setBearerToken("TOKEN")
val webService = apiClient.createWebservice(FakeApi::class.java)
val requiredStringGroup : kotlin.Int = 56 // kotlin.Int | Required String in group parameters
val requiredBooleanGroup : kotlin.Boolean = true // kotlin.Boolean | Required Boolean in group parameters
val requiredInt64Group : kotlin.Long = 789 // kotlin.Long | Required Integer in group parameters
val stringGroup : kotlin.Int = 56 // kotlin.Int | String in group parameters
val booleanGroup : kotlin.Boolean = true // kotlin.Boolean | Boolean in group parameters
val int64Group : kotlin.Long = 789 // kotlin.Long | Integer in group parameters

webService.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requiredStringGroup** | **kotlin.Int**| Required String in group parameters |
 **requiredBooleanGroup** | **kotlin.Boolean**| Required Boolean in group parameters |
 **requiredInt64Group** | **kotlin.Long**| Required Integer in group parameters |
 **stringGroup** | **kotlin.Int**| String in group parameters | [optional]
 **booleanGroup** | **kotlin.Boolean**| Boolean in group parameters | [optional]
 **int64Group** | **kotlin.Long**| Integer in group parameters | [optional]

### Return type

null (empty response body)

### Authorization


Configure bearer_test:
    ApiClient().setBearerToken("TOKEN")

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


test inline additionalProperties

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FakeApi::class.java)
val requestBody : kotlin.collections.Map<kotlin.String, kotlin.String> =  // kotlin.collections.Map<kotlin.String, kotlin.String> | request body

webService.testInlineAdditionalProperties(requestBody)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requestBody** | [**kotlin.collections.Map&lt;kotlin.String, kotlin.String&gt;**](kotlin.String.md)| request body |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


test json serialization of form data

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FakeApi::class.java)
val param : kotlin.String = param_example // kotlin.String | field1
val param2 : kotlin.String = param2_example // kotlin.String | field2

webService.testJsonFormData(param, param2)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **kotlin.String**| field1 |
 **param2** | **kotlin.String**| field2 |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined




To test the collection format in query parameters

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FakeApi::class.java)
val pipe : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | 
val ioutil : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | 
val http : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | 
val url : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | 
val context : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | 

webService.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)|  |
 **ioutil** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)|  |
 **http** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)|  |
 **url** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)|  |
 **context** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)|  |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

