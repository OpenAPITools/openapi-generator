# AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**call123testSpecialTags**](AnotherFakeApi.md#call123testSpecialTags) | **PATCH** another-fake/dummy | To test special tags



To test special tags

To test special tags and operation ID starting with number

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(AnotherFakeApi::class.java)
val client : Client =  // Client | client model

val result : Client = webService.call123testSpecialTags(client)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | [**Client**](Client.md)| client model |

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

