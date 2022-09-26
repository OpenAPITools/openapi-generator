# PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](PetApi.md#addPet) | **POST** /pet | Add a new pet to the store
[**deletePet**](PetApi.md#deletePet) | **DELETE** /pet/{petId} | Deletes a pet
[**findPetsByTags**](PetApi.md#findPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
[**getAllPets**](PetApi.md#getAllPets) | **GET** /pet/getAll | Get all pets
[**getPetById**](PetApi.md#getPetById) | **GET** /pet/{petId} | Find pet by ID
[**updatePet**](PetApi.md#updatePet) | **PUT** /pet | Update an existing pet
[**updatePetWithForm**](PetApi.md#updatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**uploadFile**](PetApi.md#uploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image


<a name="addPet"></a>
# **addPet**
> addPet(body)

Add a new pet to the store

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PetApi()
val body : Pet =  // Pet | Pet object that needs to be added to the store
try {
    apiInstance.addPet(body)
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
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store |

### Return type

null (empty response body)

### Authorization


Configure petstore_auth:
    ApiClient.accessToken = ""

### HTTP request headers

 - **Content-Type**: application/json
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
 **petId** | **kotlin.Long**| Pet id to delete |
 **apiKey** | **kotlin.String**|  | [optional]

### Return type

null (empty response body)

### Authorization


Configure petstore_auth:
    ApiClient.accessToken = ""

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="findPetsByTags"></a>
# **findPetsByTags**
> kotlin.collections.List&lt;Pet&gt; findPetsByTags(tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PetApi()
val tags : kotlin.collections.List<kotlin.String> =  // kotlin.collections.List<kotlin.String> | Tags to filter by
try {
    val result : kotlin.collections.List<Pet> = apiInstance.findPetsByTags(tags)
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
 **tags** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)| Tags to filter by |

### Return type

[**kotlin.collections.List&lt;Pet&gt;**](Pet.md)

### Authorization


Configure petstore_auth:
    ApiClient.accessToken = ""

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

<a name="getAllPets"></a>
# **getAllPets**
> kotlin.collections.List&lt;Pet&gt; getAllPets(lastUpdated)

Get all pets

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PetApi()
val lastUpdated : java.time.OffsetDateTime = 2013-10-20T19:20:30+01:00 // java.time.OffsetDateTime | When this endpoint was hit last to help identify if the client already has the latest copy.
try {
    val result : kotlin.collections.List<Pet> = apiInstance.getAllPets(lastUpdated)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling PetApi#getAllPets")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#getAllPets")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **lastUpdated** | **java.time.OffsetDateTime**| When this endpoint was hit last to help identify if the client already has the latest copy. | [optional]

### Return type

[**kotlin.collections.List&lt;Pet&gt;**](Pet.md)

### Authorization


Configure petstore_auth:
    ApiClient.accessToken = ""

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

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
 **petId** | **kotlin.Long**| ID of pet to return |

### Return type

[**Pet**](Pet.md)

### Authorization


Configure api_key:
    ApiClient.apiKey["api_key"] = ""
    ApiClient.apiKeyPrefix["api_key"] = ""

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

<a name="updatePet"></a>
# **updatePet**
> updatePet(body)

Update an existing pet

### Example
```kotlin
// Import classes:
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiInstance = PetApi()
val body : Pet =  // Pet | Pet object that needs to be added to the store
try {
    apiInstance.updatePet(body)
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
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store |

### Return type

null (empty response body)

### Authorization


Configure petstore_auth:
    ApiClient.accessToken = ""

### HTTP request headers

 - **Content-Type**: application/json
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
 **petId** | **kotlin.Long**| ID of pet that needs to be updated |
 **name** | **kotlin.String**| Updated name of the pet | [optional]
 **status** | **kotlin.String**| Updated status of the pet | [optional]

### Return type

null (empty response body)

### Authorization


Configure petstore_auth:
    ApiClient.accessToken = ""

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

<a name="uploadFile"></a>
# **uploadFile**
> ModelApiResponse uploadFile(petId, additionalMetadata, file)

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
    val result : ModelApiResponse = apiInstance.uploadFile(petId, additionalMetadata, file)
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
 **petId** | **kotlin.Long**| ID of pet to update |
 **additionalMetadata** | **kotlin.String**| Additional data to pass to server | [optional]
 **file** | **java.io.File**| file to upload | [optional]

### Return type

[**ModelApiResponse**](ModelApiResponse.md)

### Authorization


Configure petstore_auth:
    ApiClient.accessToken = ""

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

