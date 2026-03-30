# BirdApi

All URIs are relative to *http://example.org*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**getBird**](BirdApi.md#getBird) | **GET** v1/bird/{id} |  |





### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(BirdApi::class.java)
val id : java.util.UUID = 38400000-8cf0-11bd-b23e-10b96e4ef00d // java.util.UUID | 

val result : Animal = webService.getBird(id)
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **id** | **java.util.UUID**|  | |

### Return type

[**Animal**](Animal.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

