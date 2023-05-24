# PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](PetApi.md#addPet) | **POST** pet | Add a new pet to the store
[**deletePet**](PetApi.md#deletePet) | **DELETE** pet/{petId} | Deletes a pet
[**findPetsByStatus**](PetApi.md#findPetsByStatus) | **GET** pet/findByStatus | Finds Pets by status
[**findPetsByTags**](PetApi.md#findPetsByTags) | **GET** pet/findByTags | Finds Pets by tags
[**getPetById**](PetApi.md#getPetById) | **GET** pet/{petId} | Find pet by ID
[**updatePet**](PetApi.md#updatePet) | **PUT** pet | Update an existing pet
[**updatePetWithForm**](PetApi.md#updatePetWithForm) | **POST** pet/{petId} | Updates a pet in the store with form data
[**uploadFile**](PetApi.md#uploadFile) | **POST** pet/{petId}/uploadImage | uploads an image



Add a new pet to the store

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(PetApi::class.java)
val body : Pet =  // Pet | Pet object that needs to be added to the store

webService.addPet(body)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store |

### Return type

null (empty response body)

### Authorization



### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined


Deletes a pet

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(PetApi::class.java)
val petId : kotlin.Long = 789 // kotlin.Long | Pet id to delete
val apiKey : kotlin.String = apiKey_example // kotlin.String | 

webService.deletePet(petId, apiKey)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **kotlin.Long**| Pet id to delete |
 **apiKey** | **kotlin.String**|  | [optional]

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
val status : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | Status values that need to be considered for filter

val result : kotlin.collections.List<Pet> = webService.findPetsByStatus(status)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)| Status values that need to be considered for filter | [enum: available, pending, sold]

### Return type

[**kotlin.collections.List&lt;Pet&gt;**](Pet.md)

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
val tags : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | Tags to filter by

val result : kotlin.collections.List<Pet> = webService.findPetsByTags(tags)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)| Tags to filter by |

### Return type

[**kotlin.collections.List&lt;Pet&gt;**](Pet.md)

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
val petId : kotlin.Long = 789 // kotlin.Long | ID of pet to return

val result : Pet = webService.getPetById(petId)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **kotlin.Long**| ID of pet to return |

### Return type

[**Pet**](Pet.md)

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
val body : Pet =  // Pet | Pet object that needs to be added to the store

webService.updatePet(body)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store |

### Return type

null (empty response body)

### Authorization



### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined


Updates a pet in the store with form data

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(PetApi::class.java)
val petId : kotlin.Long = 789 // kotlin.Long | ID of pet that needs to be updated
val name : kotlin.String = name_example // kotlin.String | Updated name of the pet
val status : kotlin.String = status_example // kotlin.String | Updated status of the pet

webService.updatePetWithForm(petId, name, status)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **kotlin.Long**| ID of pet that needs to be updated |
 **name** | **kotlin.String**| Updated name of the pet | [optional]
 **status** | **kotlin.String**| Updated status of the pet | [optional]

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
val petId : kotlin.Long = 789 // kotlin.Long | ID of pet to update
val additionalMetadata : kotlin.String = additionalMetadata_example // kotlin.String | Additional data to pass to server
val file : java.io.File = BINARY_DATA_HERE // java.io.File | file to upload

val result : ModelApiResponse = webService.uploadFile(petId, additionalMetadata, file)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **kotlin.Long**| ID of pet to update |
 **additionalMetadata** | **kotlin.String**| Additional data to pass to server | [optional]
 **file** | **java.io.File**| file to upload | [optional]

### Return type

[**ModelApiResponse**](ModelApiResponse.md)

### Authorization



### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

