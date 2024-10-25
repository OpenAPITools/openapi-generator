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
val integerForm : Int = 56 // Int | 
val booleanForm : Boolean = true // Boolean | 
val stringForm : String = stringForm_example // String | 

launch(Dispatchers.IO) {
    val result : String = webService.testFormIntegerBooleanString(integerForm, booleanForm, stringForm)
}
```

### Parameters
| **integerForm** | **Int**|  | [optional] |
| **booleanForm** | **Boolean**|  | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **stringForm** | **String**|  | [optional] |

### Return type

**String**

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
val form1 : String = form1_example // String | 
val form2 : Int = 56 // Int | 
val form3 : String = form3_example // String | 
val form4 : Boolean = true // Boolean | 
val id : Long = 789 // Long | 
val name : String = name_example // String | 

launch(Dispatchers.IO) {
    val result : String = webService.testFormOneof(form1, form2, form3, form4, id, name)
}
```

### Parameters
| **form1** | **String**|  | [optional] |
| **form2** | **Int**|  | [optional] |
| **form3** | **String**|  | [optional] |
| **form4** | **Boolean**|  | [optional] |
| **id** | **Long**|  | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **name** | **String**|  | [optional] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: text/plain

