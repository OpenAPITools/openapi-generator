# QueryApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testEnumRefString**](QueryApi.md#testEnumRefString) | **GET** /query/enum_ref_string | Test query parameter(s) |
| [**testQueryDatetimeDateString**](QueryApi.md#testQueryDatetimeDateString) | **GET** /query/datetime/date/string | Test query parameter(s) |
| [**testQueryIntegerBooleanString**](QueryApi.md#testQueryIntegerBooleanString) | **GET** /query/integer/boolean/string | Test query parameter(s) |
| [**testQueryStyleDeepObjectExplodeTrueObject**](QueryApi.md#testQueryStyleDeepObjectExplodeTrueObject) | **GET** /query/style_deepObject/explode_true/object | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueArrayString**](QueryApi.md#testQueryStyleFormExplodeTrueArrayString) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueObject**](QueryApi.md#testQueryStyleFormExplodeTrueObject) | **GET** /query/style_form/explode_true/object | Test query parameter(s) |


<a id="testEnumRefString"></a>
# **testEnumRefString**
> kotlin.String testEnumRefString(enumNonrefStringQuery, enumRefStringQuery)

Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = QueryApi()
val enumNonrefStringQuery : kotlin.String = enumNonrefStringQuery_example // kotlin.String | 
val enumRefStringQuery : ApiStringEnumRef =  // ApiStringEnumRef | 
try {
    val result : kotlin.String = apiInstance.testEnumRefString(enumNonrefStringQuery, enumRefStringQuery)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling QueryApi#testEnumRefString")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling QueryApi#testEnumRefString")
    e.printStackTrace()
}
```

### Parameters
| **enumNonrefStringQuery** | **kotlin.String**|  | [optional] [enum: success, failure, unclassified] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **enumRefStringQuery** | [**ApiStringEnumRef**](.md)|  | [optional] [enum: success, failure, unclassified] |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

<a id="testQueryDatetimeDateString"></a>
# **testQueryDatetimeDateString**
> kotlin.String testQueryDatetimeDateString(datetimeQuery, dateQuery, stringQuery)

Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = QueryApi()
val datetimeQuery : java.time.OffsetDateTime = 2013-10-20T19:20:30+01:00 // java.time.OffsetDateTime | 
val dateQuery : java.time.LocalDate = 2013-10-20 // java.time.LocalDate | 
val stringQuery : kotlin.String = stringQuery_example // kotlin.String | 
try {
    val result : kotlin.String = apiInstance.testQueryDatetimeDateString(datetimeQuery, dateQuery, stringQuery)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling QueryApi#testQueryDatetimeDateString")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling QueryApi#testQueryDatetimeDateString")
    e.printStackTrace()
}
```

### Parameters
| **datetimeQuery** | **java.time.OffsetDateTime**|  | [optional] |
| **dateQuery** | **java.time.LocalDate**|  | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **stringQuery** | **kotlin.String**|  | [optional] |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

<a id="testQueryIntegerBooleanString"></a>
# **testQueryIntegerBooleanString**
> kotlin.String testQueryIntegerBooleanString(integerQuery, booleanQuery, stringQuery)

Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = QueryApi()
val integerQuery : kotlin.Int = 56 // kotlin.Int | 
val booleanQuery : kotlin.Boolean = true // kotlin.Boolean | 
val stringQuery : kotlin.String = stringQuery_example // kotlin.String | 
try {
    val result : kotlin.String = apiInstance.testQueryIntegerBooleanString(integerQuery, booleanQuery, stringQuery)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling QueryApi#testQueryIntegerBooleanString")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling QueryApi#testQueryIntegerBooleanString")
    e.printStackTrace()
}
```

### Parameters
| **integerQuery** | **kotlin.Int**|  | [optional] |
| **booleanQuery** | **kotlin.Boolean**|  | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **stringQuery** | **kotlin.String**|  | [optional] |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

<a id="testQueryStyleDeepObjectExplodeTrueObject"></a>
# **testQueryStyleDeepObjectExplodeTrueObject**
> kotlin.String testQueryStyleDeepObjectExplodeTrueObject(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = QueryApi()
val queryObject : ApiPet =  // ApiPet | 
try {
    val result : kotlin.String = apiInstance.testQueryStyleDeepObjectExplodeTrueObject(queryObject)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling QueryApi#testQueryStyleDeepObjectExplodeTrueObject")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling QueryApi#testQueryStyleDeepObjectExplodeTrueObject")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **queryObject** | [**ApiPet**](.md)|  | [optional] |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

<a id="testQueryStyleFormExplodeTrueArrayString"></a>
# **testQueryStyleFormExplodeTrueArrayString**
> kotlin.String testQueryStyleFormExplodeTrueArrayString(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = QueryApi()
val queryObject : ApiTestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter =  // ApiTestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter | 
try {
    val result : kotlin.String = apiInstance.testQueryStyleFormExplodeTrueArrayString(queryObject)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling QueryApi#testQueryStyleFormExplodeTrueArrayString")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling QueryApi#testQueryStyleFormExplodeTrueArrayString")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **queryObject** | [**ApiTestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter**](.md)|  | [optional] |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

<a id="testQueryStyleFormExplodeTrueObject"></a>
# **testQueryStyleFormExplodeTrueObject**
> kotlin.String testQueryStyleFormExplodeTrueObject(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = QueryApi()
val queryObject : ApiPet =  // ApiPet | 
try {
    val result : kotlin.String = apiInstance.testQueryStyleFormExplodeTrueObject(queryObject)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling QueryApi#testQueryStyleFormExplodeTrueObject")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling QueryApi#testQueryStyleFormExplodeTrueObject")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **queryObject** | [**ApiPet**](.md)|  | [optional] |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

