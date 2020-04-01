# FakeClassnameTags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testClassname**](FakeClassnameTags123Api.md#testClassname) | **PATCH** /fake_classname_test | To test class name in snake case


<a name="testClassname"></a>
# **testClassname**
> Client testClassname(client)

To test class name in snake case

To test class name in snake case

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeClassnameTags123Api()
val client : Client =  // Client | client model
try {
    val result : Client = apiInstance.testClassname(client)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling FakeClassnameTags123Api#testClassname")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeClassnameTags123Api#testClassname")
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


Configure api_key_query:
    ApiClient.apiKey["api_key_query"] = ""
    ApiClient.apiKeyPrefix["api_key_query"] = ""

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

