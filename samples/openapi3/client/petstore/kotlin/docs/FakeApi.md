# FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fakeHealthGet**](FakeApi.md#fakeHealthGet) | **GET** /fake/health | Health check endpoint
[**fakeHttpSignatureTest**](FakeApi.md#fakeHttpSignatureTest) | **GET** /fake/http-signature-test | test http signature authentication
[**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** /fake/outer/boolean | 
[**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** /fake/outer/composite | 
[**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **POST** /fake/outer/number | 
[**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **POST** /fake/outer/string | 
[**testBodyWithFileSchema**](FakeApi.md#testBodyWithFileSchema) | **PUT** /fake/body-with-file-schema | 
[**testBodyWithQueryParams**](FakeApi.md#testBodyWithQueryParams) | **PUT** /fake/body-with-query-params | 
[**testClientModel**](FakeApi.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** /fake | To test enum parameters
[**testGroupParameters**](FakeApi.md#testGroupParameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**testInlineAdditionalProperties**](FakeApi.md#testInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData**](FakeApi.md#testJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data
[**testQueryParameterCollectionFormat**](FakeApi.md#testQueryParameterCollectionFormat) | **PUT** /fake/test-query-parameters | 


<a name="fakeHealthGet"></a>
# **fakeHealthGet**
> HealthCheckResult fakeHealthGet()

Health check endpoint

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
try {
    val result : HealthCheckResult = apiInstance.fakeHealthGet()
    println(result)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#fakeHealthGet")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#fakeHealthGet")
    e.printStackTrace()
}
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

<a name="fakeHttpSignatureTest"></a>
# **fakeHttpSignatureTest**
> fakeHttpSignatureTest(pet, query1, header1)

test http signature authentication

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val pet : Pet =  // Pet | Pet object that needs to be added to the store
val query1 : kotlin.String = query1_example // kotlin.String | query parameter
val header1 : kotlin.String = header1_example // kotlin.String | header parameter
try {
    apiInstance.fakeHttpSignatureTest(pet, query1, header1)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#fakeHttpSignatureTest")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#fakeHttpSignatureTest")
    e.printStackTrace()
}
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

<a name="fakeOuterBooleanSerialize"></a>
# **fakeOuterBooleanSerialize**
> kotlin.Boolean fakeOuterBooleanSerialize(body)



Test serialization of outer boolean types

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val body : kotlin.Boolean = true // kotlin.Boolean | Input boolean as post body
try {
    val result : kotlin.Boolean = apiInstance.fakeOuterBooleanSerialize(body)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#fakeOuterBooleanSerialize")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#fakeOuterBooleanSerialize")
    e.printStackTrace()
}
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

<a name="fakeOuterCompositeSerialize"></a>
# **fakeOuterCompositeSerialize**
> OuterComposite fakeOuterCompositeSerialize(outerComposite)



Test serialization of object with outer number type

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val outerComposite : OuterComposite =  // OuterComposite | Input composite as post body
try {
    val result : OuterComposite = apiInstance.fakeOuterCompositeSerialize(outerComposite)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#fakeOuterCompositeSerialize")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#fakeOuterCompositeSerialize")
    e.printStackTrace()
}
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

<a name="fakeOuterNumberSerialize"></a>
# **fakeOuterNumberSerialize**
> java.math.BigDecimal fakeOuterNumberSerialize(body)



Test serialization of outer number types

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val body : java.math.BigDecimal = 8.14 // java.math.BigDecimal | Input number as post body
try {
    val result : java.math.BigDecimal = apiInstance.fakeOuterNumberSerialize(body)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#fakeOuterNumberSerialize")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#fakeOuterNumberSerialize")
    e.printStackTrace()
}
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

<a name="fakeOuterStringSerialize"></a>
# **fakeOuterStringSerialize**
> kotlin.String fakeOuterStringSerialize(body)



Test serialization of outer string types

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val body : kotlin.String = body_example // kotlin.String | Input string as post body
try {
    val result : kotlin.String = apiInstance.fakeOuterStringSerialize(body)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#fakeOuterStringSerialize")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#fakeOuterStringSerialize")
    e.printStackTrace()
}
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

<a name="testBodyWithFileSchema"></a>
# **testBodyWithFileSchema**
> testBodyWithFileSchema(fileSchemaTestClass)



For this test, the body for this request much reference a schema named &#x60;File&#x60;.

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val fileSchemaTestClass : FileSchemaTestClass =  // FileSchemaTestClass | 
try {
    apiInstance.testBodyWithFileSchema(fileSchemaTestClass)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testBodyWithFileSchema")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testBodyWithFileSchema")
    e.printStackTrace()
}
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

<a name="testBodyWithQueryParams"></a>
# **testBodyWithQueryParams**
> testBodyWithQueryParams(query, user)



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val query : kotlin.String = query_example // kotlin.String | 
val user : User =  // User | 
try {
    apiInstance.testBodyWithQueryParams(query, user)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testBodyWithQueryParams")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testBodyWithQueryParams")
    e.printStackTrace()
}
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

<a name="testClientModel"></a>
# **testClientModel**
> Client testClientModel(client)

To test \&quot;client\&quot; model

To test \&quot;client\&quot; model

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val client : Client =  // Client | client model
try {
    val result : Client = apiInstance.testClientModel(client)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testClientModel")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testClientModel")
    e.printStackTrace()
}
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

<a name="testEndpointParameters"></a>
# **testEndpointParameters**
> testEndpointParameters(number, double, patternWithoutDelimiter, byte, integer, int32, int64, float, string, binary, date, dateTime, password, paramCallback)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
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
try {
    apiInstance.testEndpointParameters(number, double, patternWithoutDelimiter, byte, integer, int32, int64, float, string, binary, date, dateTime, password, paramCallback)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testEndpointParameters")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testEndpointParameters")
    e.printStackTrace()
}
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
    ApiClient.username = ""
    ApiClient.password = ""

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

<a name="testEnumParameters"></a>
# **testEnumParameters**
> testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString)

To test enum parameters

To test enum parameters

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val enumHeaderStringArray : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | Header parameter enum test (string array)
val enumHeaderString : kotlin.String = enumHeaderString_example // kotlin.String | Header parameter enum test (string)
val enumQueryStringArray : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | Query parameter enum test (string array)
val enumQueryString : kotlin.String = enumQueryString_example // kotlin.String | Query parameter enum test (string)
val enumQueryInteger : kotlin.Int = 56 // kotlin.Int | Query parameter enum test (double)
val enumQueryDouble : kotlin.Double = 1.2 // kotlin.Double | Query parameter enum test (double)
val enumFormStringArray : kotlin.collections.List<kotlin.String> = enumFormStringArray_example // kotlin.collections.List<kotlin.String> | Form parameter enum test (string array)
val enumFormString : kotlin.String = enumFormString_example // kotlin.String | Form parameter enum test (string)
try {
    apiInstance.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumFormStringArray, enumFormString)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testEnumParameters")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testEnumParameters")
    e.printStackTrace()
}
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

<a name="testGroupParameters"></a>
# **testGroupParameters**
> testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val requiredStringGroup : kotlin.Int = 56 // kotlin.Int | Required String in group parameters
val requiredBooleanGroup : kotlin.Boolean = true // kotlin.Boolean | Required Boolean in group parameters
val requiredInt64Group : kotlin.Long = 789 // kotlin.Long | Required Integer in group parameters
val stringGroup : kotlin.Int = 56 // kotlin.Int | String in group parameters
val booleanGroup : kotlin.Boolean = true // kotlin.Boolean | Boolean in group parameters
val int64Group : kotlin.Long = 789 // kotlin.Long | Integer in group parameters
try {
    apiInstance.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, stringGroup, booleanGroup, int64Group)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testGroupParameters")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testGroupParameters")
    e.printStackTrace()
}
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
    ApiClient.accessToken = ""

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="testInlineAdditionalProperties"></a>
# **testInlineAdditionalProperties**
> testInlineAdditionalProperties(requestBody)

test inline additionalProperties

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val requestBody : kotlin.collections.Map<kotlin.String, kotlin.String> =  // kotlin.collections.Map<kotlin.String, kotlin.String> | request body
try {
    apiInstance.testInlineAdditionalProperties(requestBody)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testInlineAdditionalProperties")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testInlineAdditionalProperties")
    e.printStackTrace()
}
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

<a name="testJsonFormData"></a>
# **testJsonFormData**
> testJsonFormData(param, param2)

test json serialization of form data

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val param : kotlin.String = param_example // kotlin.String | field1
val param2 : kotlin.String = param2_example // kotlin.String | field2
try {
    apiInstance.testJsonFormData(param, param2)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testJsonFormData")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testJsonFormData")
    e.printStackTrace()
}
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

<a name="testQueryParameterCollectionFormat"></a>
# **testQueryParameterCollectionFormat**
> testQueryParameterCollectionFormat(pipe, ioutil, http, url, context)



To test the collection format in query parameters

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val pipe : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | 
val ioutil : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | 
val http : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | 
val url : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | 
val context : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | 
try {
    apiInstance.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testQueryParameterCollectionFormat")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testQueryParameterCollectionFormat")
    e.printStackTrace()
}
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

