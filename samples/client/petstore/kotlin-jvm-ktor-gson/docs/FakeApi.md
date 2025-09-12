# FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**annotations**](FakeApi.md#annotations) | **POST** /fake/annotations | annotate |
| [**updatePetWithFormNumber**](FakeApi.md#updatePetWithFormNumber) | **PUT** /fake/annotations | Updates a pet in the store with form data (number) |


<a id="annotations"></a>
# **annotations**
> annotations(`annotation`)

annotate

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val `annotation` : Annotation =  // Annotation | 
try {
    apiInstance.annotations(`annotation`)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#annotations")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#annotations")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **&#x60;annotation&#x60;** | [**Annotation**](Annotation.md)|  | |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

<a id="updatePetWithFormNumber"></a>
# **updatePetWithFormNumber**
> updatePetWithFormNumber(petId, name, status, status2)

Updates a pet in the store with form data (number)



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = FakeApi()
val petId : kotlin.Long = 789 // kotlin.Long | ID of pet that needs to be updated
val name : kotlin.String = name_example // kotlin.String | Updated name of the pet
val status : kotlin.Int = 56 // kotlin.Int | integer type
val status2 : java.math.BigDecimal = 8.14 // java.math.BigDecimal | number type
try {
    apiInstance.updatePetWithFormNumber(petId, name, status, status2)
} catch (e: ClientException) {
    println("4xx response calling FakeApi#updatePetWithFormNumber")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling FakeApi#updatePetWithFormNumber")
    e.printStackTrace()
}
```

### Parameters
| **petId** | **kotlin.Long**| ID of pet that needs to be updated | |
| **name** | **kotlin.String**| Updated name of the pet | [optional] |
| **status** | **kotlin.Int**| integer type | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **status2** | **java.math.BigDecimal**| number type | [optional] |

### Return type

null (empty response body)

### Authorization


Configure petstore_auth:
    ApiClient.accessToken = ""

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

