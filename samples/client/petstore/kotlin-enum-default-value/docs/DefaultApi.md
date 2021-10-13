# DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**operation**](DefaultApi.md#operation) | **GET** / | 


<a name="operation"></a>
# **operation**
> ModelWithEnumPropertyHavingDefault operation()



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = DefaultApi()
try {
    val result : ModelWithEnumPropertyHavingDefault = apiInstance.operation()
    println(result)
} catch (e: ClientException) {
    println("4xx response calling DefaultApi#operation")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling DefaultApi#operation")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**ModelWithEnumPropertyHavingDefault**](ModelWithEnumPropertyHavingDefault.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

