# HeaderApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testHeaderIntegerBooleanStringEnums**](HeaderApi.md#testHeaderIntegerBooleanStringEnums) | **GET** /header/integer/boolean/string/enums | Test header parameter(s) |


<a id="testHeaderIntegerBooleanStringEnums"></a>
# **testHeaderIntegerBooleanStringEnums**
> kotlin.String testHeaderIntegerBooleanStringEnums(integerHeader, booleanHeader, stringHeader, enumNonrefStringHeader, enumRefStringHeader)

Test header parameter(s)

Test header parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = HeaderApi()
val integerHeader : kotlin.Int = 56 // kotlin.Int | 
val booleanHeader : kotlin.Boolean = true // kotlin.Boolean | 
val stringHeader : kotlin.String = stringHeader_example // kotlin.String | 
val enumNonrefStringHeader : kotlin.String = enumNonrefStringHeader_example // kotlin.String | 
val enumRefStringHeader : ApiStringEnumRef =  // ApiStringEnumRef | 
try {
    val result : kotlin.String = apiInstance.testHeaderIntegerBooleanStringEnums(integerHeader, booleanHeader, stringHeader, enumNonrefStringHeader, enumRefStringHeader)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling HeaderApi#testHeaderIntegerBooleanStringEnums")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling HeaderApi#testHeaderIntegerBooleanStringEnums")
    e.printStackTrace()
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

