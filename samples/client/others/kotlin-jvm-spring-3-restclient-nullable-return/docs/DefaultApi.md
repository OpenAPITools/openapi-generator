# DefaultApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**returnNothing**](DefaultApi.md#returnNothing) | **GET** /always-empty |  |
| [**returnNullableString**](DefaultApi.md#returnNullableString) | **POST** /nullable-string |  |


<a id="returnNothing"></a>
# **returnNothing**
> returnNothing()



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = DefaultApi()
try {
    apiInstance.returnNothing()
} catch (e: ClientException) {
    println("4xx response calling DefaultApi#returnNothing")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling DefaultApi#returnNothing")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a id="returnNullableString"></a>
# **returnNullableString**
> kotlin.String? returnNullableString(pingRequest)



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = DefaultApi()
val pingRequest : PingRequest =  // PingRequest | 
try {
    val result : kotlin.String? = apiInstance.returnNullableString(pingRequest)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling DefaultApi#returnNullableString")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling DefaultApi#returnNullableString")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **pingRequest** | [**PingRequest**](PingRequest.md)|  | |

### Return type

**kotlin.String?**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/html

