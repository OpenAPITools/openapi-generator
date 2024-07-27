# QueryApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testEnumRefString**](QueryApi.md#testEnumRefString) | **GET** query/enum_ref_string | Test query parameter(s) |
| [**testQueryDatetimeDateString**](QueryApi.md#testQueryDatetimeDateString) | **GET** query/datetime/date/string | Test query parameter(s) |
| [**testQueryIntegerBooleanString**](QueryApi.md#testQueryIntegerBooleanString) | **GET** query/integer/boolean/string | Test query parameter(s) |
| [**testQueryStyleDeepObjectExplodeTrueObject**](QueryApi.md#testQueryStyleDeepObjectExplodeTrueObject) | **GET** query/style_deepObject/explode_true/object | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueArrayString**](QueryApi.md#testQueryStyleFormExplodeTrueArrayString) | **GET** query/style_form/explode_true/array_string | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueObject**](QueryApi.md#testQueryStyleFormExplodeTrueObject) | **GET** query/style_form/explode_true/object | Test query parameter(s) |



Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(QueryApi::class.java)
val enumNonrefStringQuery : kotlin.String = enumNonrefStringQuery_example // kotlin.String | 
val enumRefStringQuery : ApiStringEnumRef =  // ApiStringEnumRef | 

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testEnumRefString(enumNonrefStringQuery, enumRefStringQuery)
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


Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(QueryApi::class.java)
val datetimeQuery : java.time.OffsetDateTime = 2013-10-20T19:20:30+01:00 // java.time.OffsetDateTime | 
val dateQuery : java.time.LocalDate = 2013-10-20 // java.time.LocalDate | 
val stringQuery : kotlin.String = stringQuery_example // kotlin.String | 

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testQueryDatetimeDateString(datetimeQuery, dateQuery, stringQuery)
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


Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(QueryApi::class.java)
val integerQuery : kotlin.Int = 56 // kotlin.Int | 
val booleanQuery : kotlin.Boolean = true // kotlin.Boolean | 
val stringQuery : kotlin.String = stringQuery_example // kotlin.String | 

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testQueryIntegerBooleanString(integerQuery, booleanQuery, stringQuery)
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


Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(QueryApi::class.java)
val queryObject : ApiPet =  // ApiPet | 

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testQueryStyleDeepObjectExplodeTrueObject(queryObject)
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


Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(QueryApi::class.java)
val queryObject : ApiTestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter =  // ApiTestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter | 

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testQueryStyleFormExplodeTrueArrayString(queryObject)
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


Test query parameter(s)

Test query parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(QueryApi::class.java)
val queryObject : ApiPet =  // ApiPet | 

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testQueryStyleFormExplodeTrueObject(queryObject)
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

