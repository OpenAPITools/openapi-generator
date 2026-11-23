# PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**addPet**](PetApi.md#addPet) | **POST** /pet | Add a new pet to the store |


<a id="addPet"></a>
# **addPet**
> Pet addPet(pet)

Add a new pet to the store



### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PetApi()
val pet : Pet =  // Pet | Pet object that needs to be added to the store
try {
    val result : Pet = apiInstance.addPet(pet)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling PetApi#addPet")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#addPet")
    e.printStackTrace()
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | |

### Return type

[**Pet**](Pet.md)

### Authorization


Configure petstore_auth statically:
```kotlin
ApiClient.accessToken = ""
```

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/xml, application/json

