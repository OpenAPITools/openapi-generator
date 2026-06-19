# FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**annotations**](FakeApi.md#annotations) | **POST** fake/annotations | annotate |
| [**updatePetWithFormNumber**](FakeApi.md#updatePetWithFormNumber) | **PUT** fake/annotations | Updates a pet in the store with form data (number) |



annotate

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FakeApi::class.java)
val apiAnnotation : ApiAnnotation =  // ApiAnnotation | 

launch(Dispatchers.IO) {
    webService.annotations(apiAnnotation)
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **apiAnnotation** | [**ApiAnnotation**](ApiAnnotation.md)|  | |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


Updates a pet in the store with form data (number)



### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(FakeApi::class.java)
val petId : kotlin.Long = 789 // kotlin.Long | ID of pet that needs to be updated
val name : kotlin.String = name_example // kotlin.String | Updated name of the pet
val status : kotlin.Int = 56 // kotlin.Int | integer type
val status2 : java.math.BigDecimal = 8.14 // java.math.BigDecimal | number type

launch(Dispatchers.IO) {
    webService.updatePetWithFormNumber(petId, name, status, status2)
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



### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

