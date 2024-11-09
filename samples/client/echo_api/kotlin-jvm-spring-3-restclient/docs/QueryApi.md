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
> String testEnumRefString(enumNonrefStringQuery, enumRefStringQuery)

Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = QueryApi()
val enumNonrefStringQuery : String = enumNonrefStringQuery_example // String | 
val enumRefStringQuery : StringEnumRef =  // StringEnumRef | 
try {
    val result : String = apiInstance.testEnumRefString(enumNonrefStringQuery, enumRefStringQuery)
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
| **enumNonrefStringQuery** | **String**|  | [optional] [enum: success, failure, unclassified] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **enumRefStringQuery** | [**StringEnumRef**](.md)|  | [optional] [enum: success, failure, unclassified] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

<a id="testQueryDatetimeDateString"></a>
# **testQueryDatetimeDateString**
> String testQueryDatetimeDateString(datetimeQuery, dateQuery, stringQuery)

Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = QueryApi()
val datetimeQuery : OffsetDateTime = 2013-10-20T19:20:30+01:00 // OffsetDateTime | 
val dateQuery : LocalDate = 2013-10-20 // LocalDate | 
val stringQuery : String = stringQuery_example // String | 
try {
    val result : String = apiInstance.testQueryDatetimeDateString(datetimeQuery, dateQuery, stringQuery)
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
| **datetimeQuery** | **OffsetDateTime**|  | [optional] |
| **dateQuery** | **LocalDate**|  | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **stringQuery** | **String**|  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

<a id="testQueryIntegerBooleanString"></a>
# **testQueryIntegerBooleanString**
> String testQueryIntegerBooleanString(integerQuery, booleanQuery, stringQuery)

Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = QueryApi()
val integerQuery : Int = 56 // Int | 
val booleanQuery : Boolean = true // Boolean | 
val stringQuery : String = stringQuery_example // String | 
try {
    val result : String = apiInstance.testQueryIntegerBooleanString(integerQuery, booleanQuery, stringQuery)
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
| **integerQuery** | **Int**|  | [optional] |
| **booleanQuery** | **Boolean**|  | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **stringQuery** | **String**|  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

<a id="testQueryStyleDeepObjectExplodeTrueObject"></a>
# **testQueryStyleDeepObjectExplodeTrueObject**
> String testQueryStyleDeepObjectExplodeTrueObject(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = QueryApi()
val queryObject : Pet =  // Pet | 
try {
    val result : String = apiInstance.testQueryStyleDeepObjectExplodeTrueObject(queryObject)
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
| **queryObject** | [**Pet**](.md)|  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

<a id="testQueryStyleFormExplodeTrueArrayString"></a>
# **testQueryStyleFormExplodeTrueArrayString**
> String testQueryStyleFormExplodeTrueArrayString(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = QueryApi()
val queryObject : TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter =  // TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter | 
try {
    val result : String = apiInstance.testQueryStyleFormExplodeTrueArrayString(queryObject)
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
| **queryObject** | [**TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter**](.md)|  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

<a id="testQueryStyleFormExplodeTrueObject"></a>
# **testQueryStyleFormExplodeTrueObject**
> String testQueryStyleFormExplodeTrueObject(queryObject)

Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = QueryApi()
val queryObject : Pet =  // Pet | 
try {
    val result : String = apiInstance.testQueryStyleFormExplodeTrueObject(queryObject)
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
| **queryObject** | [**Pet**](.md)|  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

