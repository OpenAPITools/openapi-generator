# AuthApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testAuthHttpBasic**](AuthApi.md#testAuthHttpBasic) | **POST** /auth/http/basic | To test HTTP basic authentication |
| [**testAuthHttpBearer**](AuthApi.md#testAuthHttpBearer) | **POST** /auth/http/bearer | To test HTTP bearer authentication |


<a id="testAuthHttpBasic"></a>
# **testAuthHttpBasic**
> kotlin.String testAuthHttpBasic()

To test HTTP basic authentication

To test HTTP basic authentication

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = AuthApi()
try {
    val result : kotlin.String = apiInstance.testAuthHttpBasic()
    println(result)
} catch (e: ClientException) {
    println("4xx response calling AuthApi#testAuthHttpBasic")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling AuthApi#testAuthHttpBasic")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**kotlin.String**

### Authorization


Configure http_auth:
    ApiClient.username = ""
    ApiClient.password = ""

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

<a id="testAuthHttpBearer"></a>
# **testAuthHttpBearer**
> kotlin.String testAuthHttpBearer()

To test HTTP bearer authentication

To test HTTP bearer authentication

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = AuthApi()
try {
    val result : kotlin.String = apiInstance.testAuthHttpBearer()
    println(result)
} catch (e: ClientException) {
    println("4xx response calling AuthApi#testAuthHttpBearer")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling AuthApi#testAuthHttpBearer")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**kotlin.String**

### Authorization


Configure http_bearer_auth:
    ApiClient.accessToken = ""

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

