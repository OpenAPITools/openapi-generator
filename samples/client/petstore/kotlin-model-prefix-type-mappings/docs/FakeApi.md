# FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**annotations**](FakeApi.md#annotations) | **POST** fake/annotations | annotate |



annotate

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FakeApi::class.java)
val apiAnnotation : ApiAnnotation =  // ApiAnnotation | 

launch(Dispatchers.IO) {
    webService.annotations(apiAnnotation)
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **apiAnnotation** | [**ApiAnnotation**](ApiAnnotation.md)|  | |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

