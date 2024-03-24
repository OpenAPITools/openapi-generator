# PathApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**](PathApi.md#testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath) | **GET** path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path} | Test path parameter(s)



Test path parameter(s)

Test path parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(PathApi::class.java)
val pathString : kotlin.String = pathString_example // kotlin.String | 
val pathInteger : kotlin.Int = 56 // kotlin.Int | 
val enumNonrefStringPath : kotlin.String = enumNonrefStringPath_example // kotlin.String | 
val enumRefStringPath : ApiStringEnumRef =  // ApiStringEnumRef | 

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(pathString, pathInteger, enumNonrefStringPath, enumRefStringPath)
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pathString** | **kotlin.String**|  |
 **pathInteger** | **kotlin.Int**|  |
 **enumNonrefStringPath** | **kotlin.String**|  | [enum: success, failure, unclassified]
 **enumRefStringPath** | [**ApiStringEnumRef**](.md)|  | [enum: success, failure, unclassified]

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

