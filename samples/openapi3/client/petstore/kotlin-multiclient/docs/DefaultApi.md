# DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**multiStatusReturnGet**](DefaultApi.md#multiStatusReturnGet) | **GET** /multiStatusReturn | Is able to return a lot of status codes


<a name="multiStatusReturnGet"></a>
# **multiStatusReturnGet**
> FirstModel multiStatusReturnGet(statuscode)

Is able to return a lot of status codes

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = DefaultApi()
val statuscode : kotlin.String = statuscode_example // kotlin.String | 
try {
    val result : FirstModel = apiInstance.multiStatusReturnGet(statuscode)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling DefaultApi#multiStatusReturnGet")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling DefaultApi#multiStatusReturnGet")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **statuscode** | **kotlin.String**|  | [enum: 200, 204, 242, 400, 500]

### Return type

[**FirstModel**](FirstModel.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

