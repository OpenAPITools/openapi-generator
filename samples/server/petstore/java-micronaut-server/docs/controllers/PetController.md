# PetController

All URIs are relative to `"/v2"`

The controller class is defined in **[PetController.java](../../src/main/java/org/openapitools/controller/PetController.java)**

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](#addPet) | **POST** /pet | Add a new pet to the store
[**deletePet**](#deletePet) | **DELETE** /pet/{petId} | Deletes a pet
[**findPetsByStatus**](#findPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
[**findPetsByTags**](#findPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
[**getPetById**](#getPetById) | **GET** /pet/{petId} | Find pet by ID
[**updatePet**](#updatePet) | **PUT** /pet | Update an existing pet
[**updatePetWithForm**](#updatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**uploadFile**](#uploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image

<a name="addPet"></a>
# **addPet**
```java
Mono<Pet> PetController.addPet(pet)
```

Add a new pet to the store



### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**pet** | [**Pet**](../../docs/models/Pet.md) | Pet object that needs to be added to the store |

### Return type
[**Pet**](../../docs/models/Pet.md)

### Authorization
* **petstore_auth**, scopes: `write:pets`, `read:pets`

### HTTP request headers
 - **Accepts Content-Type**: `application/json`, `application/xml`
 - **Produces Content-Type**: `application/xml`, `application/json`

<a name="deletePet"></a>
# **deletePet**
```java
Mono<Object> PetController.deletePet(petIdapiKey)
```

Deletes a pet



### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**petId** | `Long` | Pet id to delete |
**apiKey** | `String` |  | [optional parameter]


### Authorization
* **petstore_auth**, scopes: `write:pets`, `read:pets`

### HTTP request headers
 - **Accepts Content-Type**: Not defined
 - **Produces Content-Type**: Not defined

<a name="findPetsByStatus"></a>
# **findPetsByStatus**
```java
Mono<List<Pet>> PetController.findPetsByStatus(status)
```

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**status** | [**List&lt;String&gt;**](../../docs/models/String.md) | Status values that need to be considered for filter | [enum: `available`, `pending`, `sold`]

### Return type
[**List&lt;Pet&gt;**](../../docs/models/Pet.md)

### Authorization
* **petstore_auth**, scopes: `read:pets`

### HTTP request headers
 - **Accepts Content-Type**: Not defined
 - **Produces Content-Type**: `application/xml`, `application/json`

<a name="findPetsByTags"></a>
# **findPetsByTags**
```java
Mono<List<Pet>> PetController.findPetsByTags(tags)
```

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**tags** | [**List&lt;String&gt;**](../../docs/models/String.md) | Tags to filter by |

### Return type
[**List&lt;Pet&gt;**](../../docs/models/Pet.md)

### Authorization
* **petstore_auth**, scopes: `read:pets`

### HTTP request headers
 - **Accepts Content-Type**: Not defined
 - **Produces Content-Type**: `application/xml`, `application/json`

<a name="getPetById"></a>
# **getPetById**
```java
Mono<Pet> PetController.getPetById(petId)
```

Find pet by ID

Returns a single pet

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**petId** | `Long` | ID of pet to return |

### Return type
[**Pet**](../../docs/models/Pet.md)

### Authorization
* **api_key**

### HTTP request headers
 - **Accepts Content-Type**: Not defined
 - **Produces Content-Type**: `application/xml`, `application/json`

<a name="updatePet"></a>
# **updatePet**
```java
Mono<Pet> PetController.updatePet(pet)
```

Update an existing pet



### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**pet** | [**Pet**](../../docs/models/Pet.md) | Pet object that needs to be added to the store |

### Return type
[**Pet**](../../docs/models/Pet.md)

### Authorization
* **petstore_auth**, scopes: `write:pets`, `read:pets`

### HTTP request headers
 - **Accepts Content-Type**: `application/json`, `application/xml`
 - **Produces Content-Type**: `application/xml`, `application/json`

<a name="updatePetWithForm"></a>
# **updatePetWithForm**
```java
Mono<Object> PetController.updatePetWithForm(petIdnamestatus)
```

Updates a pet in the store with form data



### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**petId** | `Long` | ID of pet that needs to be updated |
**name** | `String` | Updated name of the pet | [optional parameter]
**status** | `String` | Updated status of the pet | [optional parameter]


### Authorization
* **petstore_auth**, scopes: `write:pets`, `read:pets`

### HTTP request headers
 - **Accepts Content-Type**: `application/x-www-form-urlencoded`
 - **Produces Content-Type**: Not defined

<a name="uploadFile"></a>
# **uploadFile**
```java
Mono<ModelApiResponse> PetController.uploadFile(petIdadditionalMetadata_file)
```

uploads an image



### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**petId** | `Long` | ID of pet to update |
**additionalMetadata** | `String` | Additional data to pass to server | [optional parameter]
**_file** | `CompletedFileUpload` | file to upload | [optional parameter]

### Return type
[**ModelApiResponse**](../../docs/models/ModelApiResponse.md)

### Authorization
* **petstore_auth**, scopes: `write:pets`, `read:pets`

### HTTP request headers
 - **Accepts Content-Type**: `multipart/form-data`
 - **Produces Content-Type**: `application/json`

