# PathApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**](PathApi.md#testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath) | **GET** /path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path} | Test path parameter(s) |


<a id="testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath"></a>
# **testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**
> String testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath)

Test path parameter(s)

Test path parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PathApi()
val pathString : String = pathString_example // String | 
val pathInteger : Int = 56 // Int | 
val enumNonrefStringPath : String = enumNonrefStringPath_example // String | 
val enumRefStringPath : StringEnumRef =  // StringEnumRef | 
try {
    val result : String = apiInstance.testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling PathApi#testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PathApi#testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath")
    e.printStackTrace()
}
```

### Parameters
| **pathString** | **String**|  | |
| **pathInteger** | **Int**|  | |
| **enumNonrefStringPath** | **String**|  | [enum: success, failure, unclassified] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **enumRefStringPath** | [**StringEnumRef**](.md)|  | [enum: success, failure, unclassified] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

