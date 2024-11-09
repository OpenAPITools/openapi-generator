# FakeApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**getParameterNameMapping**](FakeApi.md#getParameterNameMapping) | **GET** /fake/parameter-name-mapping | parameter name mapping test |


<a id="getParameterNameMapping"></a>
# **getParameterNameMapping**
> Environment getParameterNameMapping(underscoreType, type, typeWithUnderscore, httpDebugOption)

parameter name mapping test

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val underscoreType : Long = 789 // Long | _type
val type : String = type_example // String | type
val typeWithUnderscore : String = typeWithUnderscore_example // String | type_
val httpDebugOption : String = httpDebugOption_example // String | http debug option (to test parameter naming option)
try {
    val result : Environment = apiInstance.getParameterNameMapping(underscoreType, type, typeWithUnderscore, httpDebugOption)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#getParameterNameMapping")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#getParameterNameMapping")
    e.printStackTrace()
}
```

### Parameters
| **underscoreType** | **Long**| _type | |
| **type** | **String**| type | |
| **typeWithUnderscore** | **String**| type_ | |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **httpDebugOption** | **String**| http debug option (to test parameter naming option) | |

### Return type

[**Environment**](Environment.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

