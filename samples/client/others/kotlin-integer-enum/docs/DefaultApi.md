# DefaultApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**doGet**](DefaultApi.md#doGet) | **GET** /do | Do something |


<a id="doGet"></a>
# **doGet**
> DoGet200Response doGet()

Do something

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = DefaultApi()
try {
    val result : DoGet200Response = apiInstance.doGet()
    println(result)
} catch (e: ClientException) {
    println("4xx response calling DefaultApi#doGet")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling DefaultApi#doGet")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**DoGet200Response**](DoGet200Response.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

