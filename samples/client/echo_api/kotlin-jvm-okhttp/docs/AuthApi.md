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


Configure http_auth statically:
```kotlin
ApiClient.username = ""
ApiClient.password = ""
```
Configure http_auth dynamically:
```kotlin
apiInstance.userCredentialProvider = { "user" to "pass" }
```

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


Configure http_bearer_auth statically:
```kotlin
ApiClient.accessToken = ""
```
Configure http_bearer_auth dynamically:
```kotlin
apiInstance.accessTokenProvider = { "" }
```

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

