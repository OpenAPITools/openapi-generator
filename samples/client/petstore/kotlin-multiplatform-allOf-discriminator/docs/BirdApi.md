# BirdApi

All URIs are relative to *http://example.org*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**getBird**](BirdApi.md#getBird) | **GET** /v1/bird/{id} |  |
| [**uploadBirdWithMetadata**](BirdApi.md#uploadBirdWithMetadata) | **POST** /v1/bird/upload |  |


<a id="getBird"></a>
# **getBird**
> Bird getBird(id)



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = BirdApi()
val id : kotlin.String = 38400000-8cf0-11bd-b23e-10b96e4ef00d // kotlin.String | 
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
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **id** | **kotlin.String**|  | |

### Return type

[**Bird**](Bird.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

<a id="uploadBirdWithMetadata"></a>
# **uploadBirdWithMetadata**
> kotlin.String uploadBirdWithMetadata(metadata, file)



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = BirdApi()
val metadata : Bird =  // Bird | 
val file : io.ktor.client.request.forms.FormPart<io.ktor.client.request.forms.InputProvider> = BINARY_DATA_HERE // io.ktor.client.request.forms.FormPart<io.ktor.client.request.forms.InputProvider> | 
try {
    val result : kotlin.String = apiInstance.uploadBirdWithMetadata(metadata, file)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling BirdApi#uploadBirdWithMetadata")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling BirdApi#uploadBirdWithMetadata")
    e.printStackTrace()
}
```

### Parameters
| **metadata** | [**Bird**](Bird.md)|  | |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **file** | **io.ktor.client.request.forms.FormPart&lt;io.ktor.client.request.forms.InputProvider&gt;**|  | |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: text/plain

