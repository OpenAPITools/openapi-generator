# DummyApi

All URIs are relative to *http://petstore.swagger.io/v2*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**dummyOperation**](DummyApi.md#dummyOperation) | **POST** /dummy | dummy operation |


<a id="dummyOperation"></a>
# **dummyOperation**
> dummyOperation()

dummy operation



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = DummyApi()
try {
    apiInstance.dummyOperation()
} catch (e: ClientException) {
    println("4xx response calling DummyApi#dummyOperation")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling DummyApi#dummyOperation")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

