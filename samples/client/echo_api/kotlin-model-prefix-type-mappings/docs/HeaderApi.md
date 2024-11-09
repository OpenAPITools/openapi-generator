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
val integerHeader : Int = 56 // Int | 
val booleanHeader : Boolean = true // Boolean | 
val stringHeader : String = stringHeader_example // String | 
val enumNonrefStringHeader : String = enumNonrefStringHeader_example // String | 
val enumRefStringHeader : ApiStringEnumRef =  // ApiStringEnumRef | 

launch(Dispatchers.IO) {
    val result : String = webService.testHeaderIntegerBooleanStringEnums(integerHeader, booleanHeader, stringHeader, enumNonrefStringHeader, enumRefStringHeader)
}
```

### Parameters
| **integerHeader** | **Int**|  | [optional] |
| **booleanHeader** | **Boolean**|  | [optional] |
| **stringHeader** | **String**|  | [optional] |
| **enumNonrefStringHeader** | **String**|  | [optional] [enum: success, failure, unclassified] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **enumRefStringHeader** | [**ApiStringEnumRef**](.md)|  | [optional] [enum: success, failure, unclassified] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

