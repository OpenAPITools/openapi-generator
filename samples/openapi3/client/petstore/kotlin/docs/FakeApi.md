# FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**fakeBigDecimalMap**](FakeApi.md#fakeBigDecimalMap) | **GET** /fake/BigDecimalMap |  |
| [**fakeHealthGet**](FakeApi.md#fakeHealthGet) | **GET** /fake/health | Health check endpoint |
| [**fakeHttpSignatureTest**](FakeApi.md#fakeHttpSignatureTest) | **GET** /fake/http-signature-test | test http signature authentication |
| [**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** /fake/outer/boolean |  |
| [**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** /fake/outer/composite |  |
| [**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **POST** /fake/outer/number |  |
| [**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **POST** /fake/outer/string |  |
| [**fakePropertyEnumIntegerSerialize**](FakeApi.md#fakePropertyEnumIntegerSerialize) | **POST** /fake/property/enum-int |  |
| [**testAdditionalPropertiesReference**](FakeApi.md#testAdditionalPropertiesReference) | **POST** /fake/additionalProperties-reference | test referenced additionalProperties |
| [**testBodyWithBinary**](FakeApi.md#testBodyWithBinary) | **PUT** /fake/body-with-binary |  |
| [**testBodyWithFileSchema**](FakeApi.md#testBodyWithFileSchema) | **PUT** /fake/body-with-file-schema |  |
| [**testBodyWithQueryParams**](FakeApi.md#testBodyWithQueryParams) | **PUT** /fake/body-with-query-params |  |
| [**testClientModel**](FakeApi.md#testClientModel) | **PATCH** /fake | To test "client" model |
| [**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters
假端點
偽のエンドポイント
가짜 엔드 포인트
 |
| [**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** /fake | To test enum parameters |
| [**testGroupParameters**](FakeApi.md#testGroupParameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional) |
| [**testInlineAdditionalProperties**](FakeApi.md#testInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties |
| [**testInlineFreeformAdditionalProperties**](FakeApi.md#testInlineFreeformAdditionalProperties) | **POST** /fake/inline-freeform-additionalProperties | test inline free-form additionalProperties |
| [**testJsonFormData**](FakeApi.md#testJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data |
| [**testNullable**](FakeApi.md#testNullable) | **POST** /fake/nullable | test nullable parent property |
| [**testQueryParameterCollectionFormat**](FakeApi.md#testQueryParameterCollectionFormat) | **PUT** /fake/test-query-parameters |  |
| [**testStringMapReference**](FakeApi.md#testStringMapReference) | **POST** /fake/stringMap-reference | test referenced string map |


<a id="fakeBigDecimalMap"></a>
# **fakeBigDecimalMap**
> FakeBigDecimalMap200Response fakeBigDecimalMap()



for Java apache and Java native, test toUrlQueryString for maps with BegDecimal keys

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
try {
    val result : FakeBigDecimalMap200Response = apiInstance.fakeBigDecimalMap()
    println(result)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#fakeBigDecimalMap")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#fakeBigDecimalMap")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**FakeBigDecimalMap200Response**](FakeBigDecimalMap200Response.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a id="fakeHealthGet"></a>
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

<a id="fakeHttpSignatureTest"></a>
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
| **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | |
| **query1** | **kotlin.String**| query parameter | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **header1** | **kotlin.String**| header parameter | [optional] |

### Return type

null (empty response body)

### Authorization



### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a id="fakeOuterBooleanSerialize"></a>
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
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **body** | **kotlin.Boolean**| Input boolean as post body | [optional] |

### Return type

**kotlin.Boolean**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a id="fakeOuterCompositeSerialize"></a>
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
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **outerComposite** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional] |

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a id="fakeOuterNumberSerialize"></a>
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
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **body** | **java.math.BigDecimal**| Input number as post body | [optional] |

### Return type

[**java.math.BigDecimal**](java.math.BigDecimal.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a id="fakeOuterStringSerialize"></a>
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
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **body** | **kotlin.String**| Input string as post body | [optional] |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a id="fakePropertyEnumIntegerSerialize"></a>
# **fakePropertyEnumIntegerSerialize**
> OuterObjectWithEnumProperty fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty)



Test serialization of enum (int) properties with examples

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val outerObjectWithEnumProperty : OuterObjectWithEnumProperty =  // OuterObjectWithEnumProperty | Input enum (int) as post body
try {
    val result : OuterObjectWithEnumProperty = apiInstance.fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#fakePropertyEnumIntegerSerialize")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#fakePropertyEnumIntegerSerialize")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **outerObjectWithEnumProperty** | [**OuterObjectWithEnumProperty**](OuterObjectWithEnumProperty.md)| Input enum (int) as post body | |

### Return type

[**OuterObjectWithEnumProperty**](OuterObjectWithEnumProperty.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a id="testAdditionalPropertiesReference"></a>
# **testAdditionalPropertiesReference**
> testAdditionalPropertiesReference(requestBody)

test referenced additionalProperties



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val requestBody : kotlin.collections.Map<kotlin.String, kotlin.Any> = Object // kotlin.collections.Map<kotlin.String, kotlin.Any> | request body
try {
    apiInstance.testAdditionalPropertiesReference(requestBody)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testAdditionalPropertiesReference")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testAdditionalPropertiesReference")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **requestBody** | [**kotlin.collections.Map&lt;kotlin.String, kotlin.Any&gt;**](kotlin.Any.md)| request body | |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a id="testBodyWithBinary"></a>
# **testBodyWithBinary**
> testBodyWithBinary(body)



For this test, the body has to be a binary file.

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val body : java.io.File = BINARY_DATA_HERE // java.io.File | image to upload
try {
    apiInstance.testBodyWithBinary(body)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testBodyWithBinary")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testBodyWithBinary")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **body** | **java.io.File**| image to upload | |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a id="testBodyWithFileSchema"></a>
# **testBodyWithFileSchema**
> testBodyWithFileSchema(fileSchemaTestClass)



For this test, the body for this request must reference a schema named `File`.

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
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **fileSchemaTestClass** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  | |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a id="testBodyWithQueryParams"></a>
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
| **query** | **kotlin.String**|  | |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **user** | [**User**](User.md)|  | |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a id="testClientModel"></a>
# **testClientModel**
> Client testClientModel(client)

To test "client" model

To test "client" model

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
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **client** | [**Client**](Client.md)| client model | |

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a id="testEndpointParameters"></a>
# **testEndpointParameters**
> testEndpointParameters(number, double, patternWithoutDelimiter, byte, integer, int32, int64, float, string, binary, date, dateTime, password, paramCallback)

Fake endpoint for testing various parameters
假端點
偽のエンドポイント
가짜 엔드 포인트


Fake endpoint for testing various parameters
假端點
偽のエンドポイント
가짜 엔드 포인트


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
| **number** | **java.math.BigDecimal**| None | |
| **double** | **kotlin.Double**| None | |
| **patternWithoutDelimiter** | **kotlin.String**| None | |
| **byte** | **kotlin.ByteArray**| None | |
| **integer** | **kotlin.Int**| None | [optional] |
| **int32** | **kotlin.Int**| None | [optional] |
| **int64** | **kotlin.Long**| None | [optional] |
| **float** | **kotlin.Float**| None | [optional] |
| **string** | **kotlin.String**| None | [optional] |
| **binary** | **java.io.File**| None | [optional] |
| **date** | **java.time.LocalDate**| None | [optional] |
| **dateTime** | **java.time.OffsetDateTime**| None | [optional] |
| **password** | **kotlin.String**| None | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **paramCallback** | **kotlin.String**| None | [optional] |

### Return type

null (empty response body)

### Authorization


Configure http_basic_test:
    ApiClient.username = ""
    ApiClient.password = ""

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

<a id="testEnumParameters"></a>
# **testEnumParameters**
> testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString)

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
val enumQueryModelArray : kotlin.collections.List<EnumClass> =  // kotlin.collections.List<EnumClass> | 
val enumFormStringArray : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | Form parameter enum test (string array)
val enumFormString : kotlin.String = enumFormString_example // kotlin.String | Form parameter enum test (string)
try {
    apiInstance.testEnumParameters(enumHeaderStringArray, enumHeaderString, enumQueryStringArray, enumQueryString, enumQueryInteger, enumQueryDouble, enumQueryModelArray, enumFormStringArray, enumFormString)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testEnumParameters")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testEnumParameters")
    e.printStackTrace()
}
```

### Parameters
| **enumHeaderStringArray** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)| Header parameter enum test (string array) | [optional] [enum: >, $] |
| **enumHeaderString** | **kotlin.String**| Header parameter enum test (string) | [optional] [default to EnumHeaderString.MinusEfg] [enum: _abc, -efg, (xyz)] |
| **enumQueryStringArray** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)| Query parameter enum test (string array) | [optional] [enum: >, $] |
| **enumQueryString** | **kotlin.String**| Query parameter enum test (string) | [optional] [default to EnumQueryString.MinusEfg] [enum: _abc, -efg, (xyz)] |
| **enumQueryInteger** | **kotlin.Int**| Query parameter enum test (double) | [optional] [enum: 1, -2] |
| **enumQueryDouble** | **kotlin.Double**| Query parameter enum test (double) | [optional] [enum: 1.1, -1.2] |
| **enumQueryModelArray** | [**kotlin.collections.List&lt;EnumClass&gt;**](EnumClass.md)|  | [optional] |
| **enumFormStringArray** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)| Form parameter enum test (string array) | [optional] [default to kotlin.collections.List<EnumFormStringArray>.Dollar] [enum: >, $] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **enumFormString** | **kotlin.String**| Form parameter enum test (string) | [optional] [default to EnumFormString.MinusEfg] [enum: _abc, -efg, (xyz)] |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

<a id="testGroupParameters"></a>
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
| **requiredStringGroup** | **kotlin.Int**| Required String in group parameters | |
| **requiredBooleanGroup** | **kotlin.Boolean**| Required Boolean in group parameters | |
| **requiredInt64Group** | **kotlin.Long**| Required Integer in group parameters | |
| **stringGroup** | **kotlin.Int**| String in group parameters | [optional] |
| **booleanGroup** | **kotlin.Boolean**| Boolean in group parameters | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **int64Group** | **kotlin.Long**| Integer in group parameters | [optional] |

### Return type

null (empty response body)

### Authorization


Configure bearer_test:
    ApiClient.accessToken = ""

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a id="testInlineAdditionalProperties"></a>
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
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **requestBody** | [**kotlin.collections.Map&lt;kotlin.String, kotlin.String&gt;**](kotlin.String.md)| request body | |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a id="testInlineFreeformAdditionalProperties"></a>
# **testInlineFreeformAdditionalProperties**
> testInlineFreeformAdditionalProperties(testInlineFreeformAdditionalPropertiesRequest)

test inline free-form additionalProperties



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val testInlineFreeformAdditionalPropertiesRequest : TestInlineFreeformAdditionalPropertiesRequest =  // TestInlineFreeformAdditionalPropertiesRequest | request body
try {
    apiInstance.testInlineFreeformAdditionalProperties(testInlineFreeformAdditionalPropertiesRequest)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testInlineFreeformAdditionalProperties")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testInlineFreeformAdditionalProperties")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **testInlineFreeformAdditionalPropertiesRequest** | [**TestInlineFreeformAdditionalPropertiesRequest**](TestInlineFreeformAdditionalPropertiesRequest.md)| request body | |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a id="testJsonFormData"></a>
# **testJsonFormData**
> testJsonFormData(`param`, param2)

test json serialization of form data



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val `param` : kotlin.String = `param`_example // kotlin.String | field1
val param2 : kotlin.String = param2_example // kotlin.String | field2
try {
    apiInstance.testJsonFormData(`param`, param2)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testJsonFormData")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testJsonFormData")
    e.printStackTrace()
}
```

### Parameters
| **&#x60;param&#x60;** | **kotlin.String**| field1 | |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **param2** | **kotlin.String**| field2 | |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

<a id="testNullable"></a>
# **testNullable**
> testNullable(childWithNullable)

test nullable parent property



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val childWithNullable : ChildWithNullable =  // ChildWithNullable | request body
try {
    apiInstance.testNullable(childWithNullable)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testNullable")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testNullable")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **childWithNullable** | [**ChildWithNullable**](ChildWithNullable.md)| request body | |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a id="testQueryParameterCollectionFormat"></a>
# **testQueryParameterCollectionFormat**
> testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, language)



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
val allowEmpty : kotlin.String = allowEmpty_example // kotlin.String | 
val language : kotlin.collections.Map<kotlin.String, kotlin.String> =  // kotlin.collections.Map<kotlin.String, kotlin.String> | 
try {
    apiInstance.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, language)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testQueryParameterCollectionFormat")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testQueryParameterCollectionFormat")
    e.printStackTrace()
}
```

### Parameters
| **pipe** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)|  | |
| **ioutil** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)|  | |
| **http** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)|  | |
| **url** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)|  | |
| **context** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)|  | |
| **allowEmpty** | **kotlin.String**|  | |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **language** | [**kotlin.collections.Map&lt;kotlin.String, kotlin.String&gt;**](kotlin.String.md)|  | [optional] |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a id="testStringMapReference"></a>
# **testStringMapReference**
> testStringMapReference(requestBody)

test referenced string map



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val requestBody : kotlin.collections.Map<kotlin.String, kotlin.String> =  // kotlin.collections.Map<kotlin.String, kotlin.String> | request body
try {
    apiInstance.testStringMapReference(requestBody)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#testStringMapReference")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#testStringMapReference")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **requestBody** | [**kotlin.collections.Map&lt;kotlin.String, kotlin.String&gt;**](kotlin.String.md)| request body | |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

