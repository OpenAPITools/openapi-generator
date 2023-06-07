# DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testPost**](DefaultApi.md#testPost) | **POST** /test | 


<a id="testPost"></a>
# **testPost**
> testPost(apa)



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = DefaultApi()
val apa : Apa =  // Apa | 
try {
    apiInstance.testPost(apa)
} catch (e: ClientException) {
    println("4xx response calling DefaultApi#testPost")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling DefaultApi#testPost")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **apa** | [**Apa**](Apa.md)|  |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

