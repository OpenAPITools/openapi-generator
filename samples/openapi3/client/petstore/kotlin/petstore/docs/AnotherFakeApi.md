# AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**call123testSpecialTags**](AnotherFakeApi.md#call123testSpecialTags) | **PATCH** /another-fake/dummy | To test special tags


<a name="call123testSpecialTags"></a>
# **call123testSpecialTags**
> Client call123testSpecialTags(client)

To test special tags

To test special tags and operation ID starting with number

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = AnotherFakeApi()
val client : Client =  // Client | client model
try {
    val result : Client = apiInstance.call123testSpecialTags(client)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling AnotherFakeApi#call123testSpecialTags")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling AnotherFakeApi#call123testSpecialTags")
    e.printStackTrace()
}
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

