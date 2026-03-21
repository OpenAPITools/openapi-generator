# PetApi

All URIs are relative to *http://example.org*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**getPet**](PetApi.md#getPet) | **GET** /v1/pet/{id} |  |


<a id="getPet"></a>
# **getPet**
> Pet getPet(id)



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PetApi()
val id : kotlin.String = 38400000-8cf0-11bd-b23e-10b96e4ef00d // kotlin.String | 
try {
    val result : Pet = apiInstance.getPet(id)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling PetApi#getPet")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#getPet")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **id** | **kotlin.String**|  | |

### Return type

[**Pet**](Pet.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

