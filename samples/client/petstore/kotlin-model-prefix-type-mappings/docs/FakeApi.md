# FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**annotations**](FakeApi.md#annotations) | **POST** /fake/annotations | annotate


<a id="annotations"></a>
# **annotations**
> annotations(apiAnnotation)

annotate

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val apiAnnotation : ApiAnnotation =  // ApiAnnotation | 
try {
    apiInstance.annotations(apiAnnotation)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#annotations")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#annotations")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **apiAnnotation** | [**ApiAnnotation**](ApiAnnotation.md)|  |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

