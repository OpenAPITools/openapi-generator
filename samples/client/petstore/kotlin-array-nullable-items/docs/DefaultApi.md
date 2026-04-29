# DefaultApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**getModel**](DefaultApi.md#getModel) | **GET** / |  |


<a id="getModel"></a>
# **getModel**
> ArrayWithNullableItemsModel getModel()



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = DefaultApi()
try {
    val result : ArrayWithNullableItemsModel = apiInstance.getModel()
    println(result)
} catch (e: ClientException) {
    println("4xx response calling DefaultApi#getModel")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling DefaultApi#getModel")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**ArrayWithNullableItemsModel**](ArrayWithNullableItemsModel.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

