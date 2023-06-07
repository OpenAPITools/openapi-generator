# BirdApi

All URIs are relative to *http://example.org*

Method | HTTP request | Description
------------- | ------------- | -------------
[**getBird**](BirdApi.md#getBird) | **GET** /v1/bird/{id} | 


<a id="getBird"></a>
# **getBird**
> Bird getBird(id)



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = BirdApi()
val id : java.util.UUID = 38400000-8cf0-11bd-b23e-10b96e4ef00d // java.util.UUID | 
try {
    val result : Bird = apiInstance.getBird(id)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling BirdApi#getBird")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling BirdApi#getBird")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **id** | **java.util.UUID**|  |

### Return type

[**Bird**](Bird.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

