# FormApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testFormIntegerBooleanString**](FormApi.md#testFormIntegerBooleanString) | **POST** form/integer/boolean/string | Test form parameter(s) |
| [**testFormOneof**](FormApi.md#testFormOneof) | **POST** form/oneof | Test form parameter(s) for oneOf schema |



Test form parameter(s)

Test form parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FormApi::class.java)
val integerForm : kotlin.Int = 56 // kotlin.Int | 
val booleanForm : kotlin.Boolean = true // kotlin.Boolean | 
val stringForm : kotlin.String = stringForm_example // kotlin.String | 

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testFormIntegerBooleanString(integerForm, booleanForm, stringForm)
}
```

### Parameters
| **integerForm** | **kotlin.Int**|  | [optional] |
| **booleanForm** | **kotlin.Boolean**|  | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **stringForm** | **kotlin.String**|  | [optional] |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: text/plain


Test form parameter(s) for oneOf schema

Test form parameter(s) for oneOf schema

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FormApi::class.java)

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.testFormOneof()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: text/plain

