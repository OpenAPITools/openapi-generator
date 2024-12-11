# FormApi

All URIs are relative to *http://localhost:3000*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testFormIntegerBooleanString**](FormApi.md#testFormIntegerBooleanString) | **POST** /form/integer/boolean/string | Test form parameter(s) |
| [**testFormOneof**](FormApi.md#testFormOneof) | **POST** /form/oneof | Test form parameter(s) for oneOf schema |


<a id="testFormIntegerBooleanString"></a>
# **testFormIntegerBooleanString**
> kotlin.String testFormIntegerBooleanString(integerForm, booleanForm, stringForm)

Test form parameter(s)

Test form parameter(s)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FormApi()
val integerForm : kotlin.Int = 56 // kotlin.Int | 
val booleanForm : kotlin.Boolean = true // kotlin.Boolean | 
val stringForm : kotlin.String = stringForm_example // kotlin.String | 
try {
    val result : kotlin.String = apiInstance.testFormIntegerBooleanString(integerForm, booleanForm, stringForm)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling FormApi#testFormIntegerBooleanString")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FormApi#testFormIntegerBooleanString")
    e.printStackTrace()
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

<a id="testFormOneof"></a>
# **testFormOneof**
> kotlin.String testFormOneof(form1, form2, form3, form4, id, name)

Test form parameter(s) for oneOf schema

Test form parameter(s) for oneOf schema

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FormApi()
val form1 : kotlin.String = form1_example // kotlin.String | 
val form2 : kotlin.Int = 56 // kotlin.Int | 
val form3 : kotlin.String = form3_example // kotlin.String | 
val form4 : kotlin.Boolean = true // kotlin.Boolean | 
val id : kotlin.Long = 789 // kotlin.Long | 
val name : kotlin.String = name_example // kotlin.String | 
try {
    val result : kotlin.String = apiInstance.testFormOneof(form1, form2, form3, form4, id, name)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling FormApi#testFormOneof")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FormApi#testFormOneof")
    e.printStackTrace()
}
```

### Parameters
| **form1** | **kotlin.String**|  | [optional] |
| **form2** | **kotlin.Int**|  | [optional] |
| **form3** | **kotlin.String**|  | [optional] |
| **form4** | **kotlin.Boolean**|  | [optional] |
| **id** | **kotlin.Long**|  | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **name** | **kotlin.String**|  | [optional] |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: text/plain

