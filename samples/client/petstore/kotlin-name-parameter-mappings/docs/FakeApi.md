# FakeApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**getParameterNameMapping**](FakeApi.md#getParameterNameMapping) | **GET** /fake/parameter-name-mapping | parameter name mapping test


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
val underscoreType : kotlin.Long = 789 // kotlin.Long | _type
val type : kotlin.String = type_example // kotlin.String | type
val typeWithUnderscore : kotlin.String = typeWithUnderscore_example // kotlin.String | type_
val httpDebugOption : kotlin.String = httpDebugOption_example // kotlin.String | http debug option (to test parameter naming option)
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

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **underscoreType** | **kotlin.Long**| _type |
 **type** | **kotlin.String**| type |
 **typeWithUnderscore** | **kotlin.String**| type_ |
 **httpDebugOption** | **kotlin.String**| http debug option (to test parameter naming option) |

### Return type

[**Environment**](Environment.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

