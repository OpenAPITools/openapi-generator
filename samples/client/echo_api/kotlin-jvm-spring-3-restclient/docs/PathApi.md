# PathApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testsPathStringpathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**](PathApi.md#testsPathStringpathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath) | **GET** /path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path} | Test path parameter(s)


<a id="testsPathStringpathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath"></a>
# **testsPathStringpathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**
> kotlin.String testsPathStringpathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath)

Test path parameter(s)

Test path parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PathApi()
val pathString : kotlin.String = pathString_example // kotlin.String | 
val pathInteger : kotlin.Int = 56 // kotlin.Int | 
val enumNonrefStringPath : kotlin.String = enumNonrefStringPath_example // kotlin.String | 
val enumRefStringPath : StringEnumRef =  // StringEnumRef | 
try {
    val result : kotlin.String = apiInstance.testsPathStringpathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling PathApi#testsPathStringpathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PathApi#testsPathStringpathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pathString** | **kotlin.String**|  |
 **pathInteger** | **kotlin.Int**|  |
 **enumNonrefStringPath** | **kotlin.String**|  | [enum: success, failure, unclassified]
 **enumRefStringPath** | [**StringEnumRef**](.md)|  | [enum: success, failure, unclassified]

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

