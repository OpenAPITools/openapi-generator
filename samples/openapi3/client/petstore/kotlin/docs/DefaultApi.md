# DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**fooGet**](DefaultApi.md#fooGet) | **GET** /foo |  |


<a id="fooGet"></a>
# **fooGet**
> FooGetDefaultResponse fooGet()



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = DefaultApi()
try {
    val result : FooGetDefaultResponse = apiInstance.fooGet()
    println(result)
} catch (e: ClientException) {
    println("4xx response calling DefaultApi#fooGet")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling DefaultApi#fooGet")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**FooGetDefaultResponse**](FooGetDefaultResponse.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

