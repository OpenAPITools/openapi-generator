# StuffApi

All URIs are relative to *https://example.org/v1*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**findStuff**](StuffApi.md#findStuff) | **GET** /foo/* | Finds stuff |


<a id="findStuff"></a>
# **findStuff**
> FindStuff200Response findStuff()

Finds stuff

Finds stuff

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = StuffApi()
try {
    val result : FindStuff200Response = apiInstance.findStuff()
    println(result)
} catch (e: ClientException) {
    println("4xx response calling StuffApi#findStuff")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling StuffApi#findStuff")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**FindStuff200Response**](FindStuff200Response.md)

### Authorization


Configure bearerAuth:
    ApiClient.accessToken = ""

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

