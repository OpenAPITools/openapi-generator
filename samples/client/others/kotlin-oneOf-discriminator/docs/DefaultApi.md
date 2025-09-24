# DefaultApi

All URIs are relative to *http://localhost:8080*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**withAllOfEndpoint**](DefaultApi.md#withAllOfEndpoint) | **GET** /one-of-with-all-of-inheritance |  |
| [**withoutAllOfEndpoint**](DefaultApi.md#withoutAllOfEndpoint) | **GET** /one-of-with-without-all-of-inheritance |  |


<a id="withAllOfEndpoint"></a>
# **withAllOfEndpoint**
> WithAllOfEndpoint200Response withAllOfEndpoint()



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = DefaultApi()
try {
    val result : WithAllOfEndpoint200Response = apiInstance.withAllOfEndpoint()
    println(result)
} catch (e: ClientException) {
    println("4xx response calling DefaultApi#withAllOfEndpoint")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling DefaultApi#withAllOfEndpoint")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**WithAllOfEndpoint200Response**](WithAllOfEndpoint200Response.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

<a id="withoutAllOfEndpoint"></a>
# **withoutAllOfEndpoint**
> WithoutAllOfEndpoint200Response withoutAllOfEndpoint()



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = DefaultApi()
try {
    val result : WithoutAllOfEndpoint200Response = apiInstance.withoutAllOfEndpoint()
    println(result)
} catch (e: ClientException) {
    println("4xx response calling DefaultApi#withoutAllOfEndpoint")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling DefaultApi#withoutAllOfEndpoint")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**WithoutAllOfEndpoint200Response**](WithoutAllOfEndpoint200Response.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

