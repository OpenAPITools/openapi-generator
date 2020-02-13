# EnumApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**getEnum**](EnumApi.md#getEnum) | **GET** /enum | Get enums


<a name="getEnum"></a>
# **getEnum**
> PetEnum getEnum()

Get enums

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = EnumApi()
try {
    val result : PetEnum = apiInstance.getEnum()
    println(result)
} catch (e: ClientException) {
    println("4xx response calling EnumApi#getEnum")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling EnumApi#getEnum")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**PetEnum**](PetEnum.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

