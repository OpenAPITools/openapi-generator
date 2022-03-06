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


## Creating PetApi

To initiate an instance of `PetApi`, you can use micronaut's `ApplicationContext`:
```java
/* imports
import io.micronaut.runtime.Micronaut;
import io.micronaut.context.ApplicationContext;
import org.openapitools.api.PetApi;
*/

ApplicationContext context = Micronaut.run(/* ... */);
PetApi apiInstance = context.getBean(PetApi.class);
```

Or the `@Inject` annotation:
```java
@Singleton
class MyClass {
    @Inject
    PetApi petApi;

    /* ... use the injected variable */
}
```
Note that the class needs to be annotated with one of Micronaut's [scope annotations](https://docs.micronaut.io/latest/guide/#scopes) like `Singleton` in order to be processed.

More information can be found inside [Inversion of Control guide section](https://docs.micronaut.io/latest/guide/#ioc).

<a name="addPet"></a>
# **addPet**
```java
Mono<Void> PetApi.addPet(_body)
```

Add a new pet to the store

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store |




### Authorization
* **[petstore_auth](auth.md#petstore_auth)**, scopes: `write:pets`, `read:pets`

### HTTP request headers
 - **Content-Type**: `application/json`, `application/xml`
 - **Accept**: Not defined

<a name="deletePet"></a>
# **deletePet**
```java
Mono<Void> PetApi.deletePet(petIdapiKey)
```

Deletes a pet

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | `Long`| Pet id to delete |
 **apiKey** | `String`|  | [optional parameter]




### Authorization
* **[petstore_auth](auth.md#petstore_auth)**, scopes: `write:pets`, `read:pets`

### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="findPetsByStatus"></a>
# **findPetsByStatus**
```java
Mono<List<Pet>> PetApi.findPetsByStatus(status)
```

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**List&lt;String&gt;**](String.md)| Status values that need to be considered for filter | [enum: `available`, `pending`, `sold`]


### Return type
[**List&lt;Pet&gt;**](Pet.md)

### Authorization
* **[petstore_auth](auth.md#petstore_auth)**, scopes: `write:pets`, `read:pets`

### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: `application/xml`, `application/json`

<a name="findPetsByTags"></a>
# **findPetsByTags**
```java
Mono<Set<Pet>> PetApi.findPetsByTags(tags)
```

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**Set&lt;String&gt;**](String.md)| Tags to filter by |


### Return type
[**Set&lt;Pet&gt;**](Pet.md)

### Authorization
* **[petstore_auth](auth.md#petstore_auth)**, scopes: `write:pets`, `read:pets`

### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: `application/xml`, `application/json`

<a name="getPetById"></a>
# **getPetById**
```java
Mono<Pet> PetApi.getPetById(petId)
```

Find pet by ID

Returns a single pet

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | `Long`| ID of pet to return |


### Return type
[**Pet**](Pet.md)

### Authorization
* **[api_key](auth.md#api_key)**

### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: `application/xml`, `application/json`

<a name="updatePet"></a>
# **updatePet**
```java
Mono<Void> PetApi.updatePet(_body)
```

Update an existing pet

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store |




### Authorization
* **[petstore_auth](auth.md#petstore_auth)**, scopes: `write:pets`, `read:pets`

### HTTP request headers
 - **Content-Type**: `application/json`, `application/xml`
 - **Accept**: Not defined

<a name="updatePetWithForm"></a>
# **updatePetWithForm**
```java
Mono<Void> PetApi.updatePetWithForm(petIdnamestatus)
```

Updates a pet in the store with form data

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | `Long`| ID of pet that needs to be updated |
 **name** | `String`| Updated name of the pet | [optional parameter]
 **status** | `String`| Updated status of the pet | [optional parameter]




### Authorization
* **[petstore_auth](auth.md#petstore_auth)**, scopes: `write:pets`, `read:pets`

### HTTP request headers
 - **Content-Type**: `application/x-www-form-urlencoded`
 - **Accept**: Not defined

<a name="uploadFile"></a>
# **uploadFile**
```java
Mono<ModelApiResponse> PetApi.uploadFile(petIdadditionalMetadata_file)
```

uploads an image

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | `Long`| ID of pet to update |
 **additionalMetadata** | `String`| Additional data to pass to server | [optional parameter]
 **_file** | `File`| file to upload | [optional parameter]


### Return type
[**ModelApiResponse**](ModelApiResponse.md)

### Authorization
* **[petstore_auth](auth.md#petstore_auth)**, scopes: `write:pets`, `read:pets`

### HTTP request headers
 - **Content-Type**: `multipart/form-data`
 - **Accept**: `application/json`

<a name="uploadFileWithRequiredFile"></a>
# **uploadFileWithRequiredFile**
```java
Mono<ModelApiResponse> PetApi.uploadFileWithRequiredFile(petIdrequiredFileadditionalMetadata)
```

uploads an image (required)

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | `Long`| ID of pet to update |
 **requiredFile** | `File`| file to upload |
 **additionalMetadata** | `String`| Additional data to pass to server | [optional parameter]


### Return type
[**ModelApiResponse**](ModelApiResponse.md)

### Authorization
* **[petstore_auth](auth.md#petstore_auth)**, scopes: `write:pets`, `read:pets`

### HTTP request headers
 - **Content-Type**: `multipart/form-data`
 - **Accept**: `application/json`

