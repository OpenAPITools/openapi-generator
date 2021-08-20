# PetAPI

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**PetAPI_addPet**](PetAPI.md#PetAPI_addPet) | **POST** /pet | Add a new pet to the store
[**PetAPI_deletePet**](PetAPI.md#PetAPI_deletePet) | **DELETE** /pet/{petId} | Deletes a pet
[**PetAPI_findPetsByStatus**](PetAPI.md#PetAPI_findPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
[**PetAPI_findPetsByTags**](PetAPI.md#PetAPI_findPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
[**PetAPI_getPetById**](PetAPI.md#PetAPI_getPetById) | **GET** /pet/{petId} | Find pet by ID
[**PetAPI_updatePet**](PetAPI.md#PetAPI_updatePet) | **PUT** /pet | Update an existing pet
[**PetAPI_updatePetWithForm**](PetAPI.md#PetAPI_updatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**PetAPI_uploadFile**](PetAPI.md#PetAPI_uploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image


# **PetAPI_addPet**
```c
// Add a new pet to the store
//
void PetAPI_addPet(apiClient_t *apiClient, pet_t * body);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**body** | **[pet_t](pet.md) \*** | Pet object that needs to be added to the store | 

### Return type

void

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **PetAPI_deletePet**
```c
// Deletes a pet
//
void PetAPI_deletePet(apiClient_t *apiClient, long petId, char * api_key);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**petId** | **long** | Pet id to delete | 
**api_key** | **char \*** |  | [optional] 

### Return type

void

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **PetAPI_findPetsByStatus**
```c
// Finds Pets by status
//
// Multiple status values can be provided with comma separated strings
//
list_t* PetAPI_findPetsByStatus(apiClient_t *apiClient, list_t * status);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**status** | **[list_t](char.md) \*** | Status values that need to be considered for filter | 

### Return type

[list_t](pet.md) *


### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **PetAPI_findPetsByTags**
```c
// Finds Pets by tags
//
// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
//
list_t* PetAPI_findPetsByTags(apiClient_t *apiClient, list_t * tags);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**tags** | **[list_t](char.md) \*** | Tags to filter by | 

### Return type

[list_t](pet.md) *


### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **PetAPI_getPetById**
```c
// Find pet by ID
//
// Returns a single pet
//
pet_t* PetAPI_getPetById(apiClient_t *apiClient, long petId);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**petId** | **long** | ID of pet to return | 

### Return type

[pet_t](pet.md) *


### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **PetAPI_updatePet**
```c
// Update an existing pet
//
void PetAPI_updatePet(apiClient_t *apiClient, pet_t * body);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**body** | **[pet_t](pet.md) \*** | Pet object that needs to be added to the store | 

### Return type

void

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **PetAPI_updatePetWithForm**
```c
// Updates a pet in the store with form data
//
void PetAPI_updatePetWithForm(apiClient_t *apiClient, long petId, char * name, char * status);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**petId** | **long** | ID of pet that needs to be updated | 
**name** | **char \*** | Updated name of the pet | [optional] 
**status** | **char \*** | Updated status of the pet | [optional] 

### Return type

void

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **PetAPI_uploadFile**
```c
// uploads an image
//
api_response_t* PetAPI_uploadFile(apiClient_t *apiClient, long petId, char * additionalMetadata, binary_t* file);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**petId** | **long** | ID of pet to update | 
**additionalMetadata** | **char \*** | Additional data to pass to server | [optional] 
**file** | **binary_t*** | file to upload | [optional] 

### Return type

[api_response_t](api_response.md) *


### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

