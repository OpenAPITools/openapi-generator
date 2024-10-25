# HeaderApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testHeaderIntegerBooleanStringEnums**](HeaderApi.md#testHeaderIntegerBooleanStringEnums) | **GET** /header/integer/boolean/string/enums | Test header parameter(s) |


<a id="testHeaderIntegerBooleanStringEnums"></a>
# **testHeaderIntegerBooleanStringEnums**
> String testHeaderIntegerBooleanStringEnums(integerHeader, booleanHeader, stringHeader, enumNonrefStringHeader, enumRefStringHeader)

Test header parameter(s)

Test header parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = HeaderApi()
val integerHeader : Int = 56 // Int | 
val booleanHeader : Boolean = true // Boolean | 
val stringHeader : String = stringHeader_example // String | 
val enumNonrefStringHeader : String = enumNonrefStringHeader_example // String | 
val enumRefStringHeader : StringEnumRef =  // StringEnumRef | 
try {
    val result : String = apiInstance.testHeaderIntegerBooleanStringEnums(integerHeader, booleanHeader, stringHeader, enumNonrefStringHeader, enumRefStringHeader)
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
| **integerHeader** | **Int**|  | [optional] |
| **booleanHeader** | **Boolean**|  | [optional] |
| **stringHeader** | **String**|  | [optional] |
| **enumNonrefStringHeader** | **String**|  | [optional] [enum: success, failure, unclassified] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **enumRefStringHeader** | [**StringEnumRef**](.md)|  | [optional] [enum: success, failure, unclassified] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

