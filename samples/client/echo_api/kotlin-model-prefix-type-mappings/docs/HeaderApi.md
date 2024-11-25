# HeaderApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testHeaderIntegerBooleanStringEnums**](HeaderApi.md#testHeaderIntegerBooleanStringEnums) | **GET** header/integer/boolean/string/enums | Test header parameter(s) |



Test header parameter(s)

Test header parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(HeaderApi::class.java)
val integerHeader : kotlin.Int = 56 // kotlin.Int | 
val booleanHeader : kotlin.Boolean = true // kotlin.Boolean | 
val stringHeader : kotlin.String = stringHeader_example // kotlin.String | 
val enumNonrefStringHeader : kotlin.String = enumNonrefStringHeader_example // kotlin.String | 
val enumRefStringHeader : ApiStringEnumRef =  // ApiStringEnumRef | 

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testHeaderIntegerBooleanStringEnums(integerHeader, booleanHeader, stringHeader, enumNonrefStringHeader, enumRefStringHeader)
}
```

### Parameters
| **integerHeader** | **kotlin.Int**|  | [optional] |
| **booleanHeader** | **kotlin.Boolean**|  | [optional] |
| **stringHeader** | **kotlin.String**|  | [optional] |
| **enumNonrefStringHeader** | **kotlin.String**|  | [optional] [enum: success, failure, unclassified] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **enumRefStringHeader** | [**ApiStringEnumRef**](.md)|  | [optional] [enum: success, failure, unclassified] |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

