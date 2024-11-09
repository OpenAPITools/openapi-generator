# PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**addPet**](PetApi.md#addPet) | **POST** pet | Add a new pet to the store |
| [**deletePet**](PetApi.md#deletePet) | **DELETE** pet/{petId} | Deletes a pet |
| [**findPetsByStatus**](PetApi.md#findPetsByStatus) | **GET** pet/findByStatus | Finds Pets by status |
| [**findPetsByTags**](PetApi.md#findPetsByTags) | **GET** pet/findByTags | Finds Pets by tags |
| [**getPetById**](PetApi.md#getPetById) | **GET** pet/{petId} | Find pet by ID |
| [**updatePet**](PetApi.md#updatePet) | **PUT** pet | Update an existing pet |
| [**updatePetWithForm**](PetApi.md#updatePetWithForm) | **POST** pet/{petId} | Updates a pet in the store with form data |
| [**uploadFile**](PetApi.md#uploadFile) | **POST** pet/{petId}/uploadImage | uploads an image |



Add a new pet to the store



### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(PetApi::class.java)
val apiPet : ApiPet =  // ApiPet | Pet object that needs to be added to the store

launch(Dispatchers.IO) {
    val result : ApiPet = webService.addPet(apiPet)
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **apiPet** | [**ApiPet**](ApiPet.md)| Pet object that needs to be added to the store | |

### Return type

[**ApiPet**](ApiPet.md)

### Authorization



### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/xml, application/json


Deletes a pet



### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(PetApi::class.java)
val petId : Long = 789 // Long | Pet id to delete
val apiKey : String = apiKey_example // String | 

launch(Dispatchers.IO) {
    webService.deletePet(petId, apiKey)
}
```

### Parameters
| **petId** | **Long**| Pet id to delete | |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **apiKey** | **String**|  | [optional] |

### Return type

null (empty response body)

### Authorization



### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(PetApi::class.java)
val status : List<String> =  // List<String> | Status values that need to be considered for filter

launch(Dispatchers.IO) {
    val result : List<ApiPet> = webService.findPetsByStatus(status)
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **status** | [**List&lt;String&gt;**](String.md)| Status values that need to be considered for filter | [enum: available, pending, sold] |

### Return type

[**List&lt;ApiPet&gt;**](ApiPet.md)

### Authorization



### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(PetApi::class.java)
val tags : List<String> =  // List<String> | Tags to filter by

launch(Dispatchers.IO) {
    val result : List<ApiPet> = webService.findPetsByTags(tags)
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **tags** | [**List&lt;String&gt;**](String.md)| Tags to filter by | |

### Return type

[**List&lt;ApiPet&gt;**](ApiPet.md)

### Authorization



### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


Find pet by ID

Returns a single pet

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(PetApi::class.java)
val petId : Long = 789 // Long | ID of pet to return

launch(Dispatchers.IO) {
    val result : ApiPet = webService.getPetById(petId)
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **petId** | **Long**| ID of pet to return | |

### Return type

[**ApiPet**](ApiPet.md)

### Authorization



### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


Update an existing pet



### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(PetApi::class.java)
val apiPet : ApiPet =  // ApiPet | Pet object that needs to be added to the store

launch(Dispatchers.IO) {
    val result : ApiPet = webService.updatePet(apiPet)
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **apiPet** | [**ApiPet**](ApiPet.md)| Pet object that needs to be added to the store | |

### Return type

[**ApiPet**](ApiPet.md)

### Authorization



### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/xml, application/json


Updates a pet in the store with form data



### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(PetApi::class.java)
val petId : Long = 789 // Long | ID of pet that needs to be updated
val name : String = name_example // String | Updated name of the pet
val status : String = status_example // String | Updated status of the pet

launch(Dispatchers.IO) {
    webService.updatePetWithForm(petId, name, status)
}
```

### Parameters
| **petId** | **Long**| ID of pet that needs to be updated | |
| **name** | **String**| Updated name of the pet | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **status** | **String**| Updated status of the pet | [optional] |

### Return type

null (empty response body)

### Authorization



### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined


uploads an image



### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(PetApi::class.java)
val petId : Long = 789 // Long | ID of pet to update
val additionalMetadata : String = additionalMetadata_example // String | Additional data to pass to server
val file : File = BINARY_DATA_HERE // File | file to upload

launch(Dispatchers.IO) {
    val result : ApiApiResponse = webService.uploadFile(petId, additionalMetadata, file)
}
```

### Parameters
| **petId** | **Long**| ID of pet to update | |
| **additionalMetadata** | **String**| Additional data to pass to server | [optional] |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **file** | **File**| file to upload | [optional] |

### Return type

[**ApiApiResponse**](ApiApiResponse.md)

### Authorization



### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

