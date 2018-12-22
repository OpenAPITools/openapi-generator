# PetApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](PetApi.md#addPet) | **POST** /pet | Add a new pet to the store
[**deletePet**](PetApi.md#deletePet) | **DELETE** /pet/{petId} | Deletes a pet
[**findPetsByStatus**](PetApi.md#findPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
[**findPetsByTags**](PetApi.md#findPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
[**getPetById**](PetApi.md#getPetById) | **GET** /pet/{petId} | Find pet by ID
[**updatePet**](PetApi.md#updatePet) | **PUT** /pet | Update an existing pet
[**updatePetWithForm**](PetApi.md#updatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**uploadFile**](PetApi.md#uploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image
[**uploadFileWithRequiredFile**](PetApi.md#uploadFileWithRequiredFile) | **POST** /fake/{petId}/uploadImageWithRequiredFile | uploads an image (required)


<a name="addPet"></a>
# **addPet**
> addPet(pet)

Add a new pet to the store

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PetApi()
val pet : Pet =  // Pet | Pet object that needs to be added to the store
try {
    apiInstance.addPet(pet)
} catch (e: ClientException) {
    println("4xx response calling PetApi#addPet")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#addPet")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store |

### Return type

null (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

<a name="deletePet"></a>
# **deletePet**
> deletePet(petId, apiKey)

Deletes a pet

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PetApi()
val petId : kotlin.Long = 789 // kotlin.Long | Pet id to delete
val apiKey : kotlin.String = apiKey_example // kotlin.String | 
try {
    apiInstance.deletePet(petId, apiKey)
} catch (e: ClientException) {
    println("4xx response calling PetApi#deletePet")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#deletePet")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **kotlin.Long**| Pet id to delete | [default to null]
 **apiKey** | **kotlin.String**|  | [optional] [default to null]

### Return type

null (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="findPetsByStatus"></a>
# **findPetsByStatus**
> kotlin.Array&lt;Pet&gt; findPetsByStatus(status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PetApi()
val status : kotlin.Array<kotlin.String> =  // kotlin.Array<kotlin.String> | Status values that need to be considered for filter
try {
    val result : kotlin.Array<Pet> = apiInstance.findPetsByStatus(status)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling PetApi#findPetsByStatus")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#findPetsByStatus")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**kotlin.Array&lt;kotlin.String&gt;**](kotlin.String.md)| Status values that need to be considered for filter | [default to null] [enum: available, pending, sold]

### Return type

[**kotlin.Array&lt;Pet&gt;**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

<a name="findPetsByTags"></a>
# **findPetsByTags**
> kotlin.Array&lt;Pet&gt; findPetsByTags(tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PetApi()
val tags : kotlin.Array<kotlin.String> =  // kotlin.Array<kotlin.String> | Tags to filter by
try {
    val result : kotlin.Array<Pet> = apiInstance.findPetsByTags(tags)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling PetApi#findPetsByTags")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#findPetsByTags")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**kotlin.Array&lt;kotlin.String&gt;**](kotlin.String.md)| Tags to filter by | [default to null]

### Return type

[**kotlin.Array&lt;Pet&gt;**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

<a name="getPetById"></a>
# **getPetById**
> Pet getPetById(petId)

Find pet by ID

Returns a single pet

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PetApi()
val petId : kotlin.Long = 789 // kotlin.Long | ID of pet to return
try {
    val result : Pet = apiInstance.getPetById(petId)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling PetApi#getPetById")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#getPetById")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **kotlin.Long**| ID of pet to return | [default to null]

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

<a name="updatePet"></a>
# **updatePet**
> updatePet(pet)

Update an existing pet

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PetApi()
val pet : Pet =  // Pet | Pet object that needs to be added to the store
try {
    apiInstance.updatePet(pet)
} catch (e: ClientException) {
    println("4xx response calling PetApi#updatePet")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#updatePet")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store |

### Return type

null (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

<a name="updatePetWithForm"></a>
# **updatePetWithForm**
> updatePetWithForm(petId, name, status)

Updates a pet in the store with form data

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PetApi()
val petId : kotlin.Long = 789 // kotlin.Long | ID of pet that needs to be updated
val name : kotlin.String = name_example // kotlin.String | Updated name of the pet
val status : kotlin.String = status_example // kotlin.String | Updated status of the pet
try {
    apiInstance.updatePetWithForm(petId, name, status)
} catch (e: ClientException) {
    println("4xx response calling PetApi#updatePetWithForm")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#updatePetWithForm")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **kotlin.Long**| ID of pet that needs to be updated | [default to null]
 **name** | **kotlin.String**| Updated name of the pet | [optional] [default to null]
 **status** | **kotlin.String**| Updated status of the pet | [optional] [default to null]

### Return type

null (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

<a name="uploadFile"></a>
# **uploadFile**
> ApiResponse uploadFile(petId, additionalMetadata, file)

uploads an image

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PetApi()
val petId : kotlin.Long = 789 // kotlin.Long | ID of pet to update
val additionalMetadata : kotlin.String = additionalMetadata_example // kotlin.String | Additional data to pass to server
val file : java.io.File = BINARY_DATA_HERE // java.io.File | file to upload
try {
    val result : ApiResponse = apiInstance.uploadFile(petId, additionalMetadata, file)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling PetApi#uploadFile")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#uploadFile")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **kotlin.Long**| ID of pet to update | [default to null]
 **additionalMetadata** | **kotlin.String**| Additional data to pass to server | [optional] [default to null]
 **file** | **java.io.File**| file to upload | [optional] [default to null]

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

<a name="uploadFileWithRequiredFile"></a>
# **uploadFileWithRequiredFile**
> ApiResponse uploadFileWithRequiredFile(petId, requiredFile, additionalMetadata)

uploads an image (required)

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PetApi()
val petId : kotlin.Long = 789 // kotlin.Long | ID of pet to update
val requiredFile : java.io.File = BINARY_DATA_HERE // java.io.File | file to upload
val additionalMetadata : kotlin.String = additionalMetadata_example // kotlin.String | Additional data to pass to server
try {
    val result : ApiResponse = apiInstance.uploadFileWithRequiredFile(petId, requiredFile, additionalMetadata)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling PetApi#uploadFileWithRequiredFile")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#uploadFileWithRequiredFile")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **kotlin.Long**| ID of pet to update | [default to null]
 **requiredFile** | **java.io.File**| file to upload | [default to null]
 **additionalMetadata** | **kotlin.String**| Additional data to pass to server | [optional] [default to null]

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

