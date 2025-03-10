# PetAPI

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**PetAPI_addPet**](PetAPI.md#PetAPI_addPet) | **POST** /pet | Add a new pet to the store
[**PetAPI_deletePet**](PetAPI.md#PetAPI_deletePet) | **DELETE** /pet/{petId} | Deletes a pet
[**PetAPI_findPetsByStatus**](PetAPI.md#PetAPI_findPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
[**PetAPI_findPetsByTags**](PetAPI.md#PetAPI_findPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
[**PetAPI_getDaysWithoutIncident**](PetAPI.md#PetAPI_getDaysWithoutIncident) | **GET** /store/daysWithoutIncident | Number of days since the last time a pet maimed someone at the store
[**PetAPI_getPetById**](PetAPI.md#PetAPI_getPetById) | **GET** /pet/{petId} | Find pet by ID
[**PetAPI_getPicture**](PetAPI.md#PetAPI_getPicture) | **GET** /pet/picture | Get a random picture of someone else&#39;s pet
[**PetAPI_isPetAvailable**](PetAPI.md#PetAPI_isPetAvailable) | **POST** /pet/{petId}/isAvailable | Is this pet still available?
[**PetAPI_sharePicture**](PetAPI.md#PetAPI_sharePicture) | **POST** /pet/picture | Send a picture of your happy pet
[**PetAPI_specialtyPet**](PetAPI.md#PetAPI_specialtyPet) | **GET** /pet/specialty | Specialty of the shop
[**PetAPI_updatePet**](PetAPI.md#PetAPI_updatePet) | **PUT** /pet | Update an existing pet
[**PetAPI_updatePetWithForm**](PetAPI.md#PetAPI_updatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**PetAPI_uploadFile**](PetAPI.md#PetAPI_uploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image


# **PetAPI_addPet**
```c
// Add a new pet to the store
//
void PetAPI_addPet(apiClient_t *apiClient, pet_t *body);
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
void PetAPI_deletePet(apiClient_t *apiClient, long petId, char *api_key);
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
list_t* PetAPI_findPetsByStatus(apiClient_t *apiClient, list_t *status);
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
list_t* PetAPI_findPetsByTags(apiClient_t *apiClient, list_t *tags);
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

# **PetAPI_getDaysWithoutIncident**
```c
// Number of days since the last time a pet maimed someone at the store
//
int* PetAPI_getDaysWithoutIncident(apiClient_t *apiClient);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |

### Return type

int*



### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

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

# **PetAPI_getPicture**
```c
// Get a random picture of someone else's pet
//
binary_t** PetAPI_getPicture(apiClient_t *apiClient);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |

### Return type

binary_t**



### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **PetAPI_isPetAvailable**
```c
// Is this pet still available?
//
bit_t* PetAPI_isPetAvailable(apiClient_t *apiClient, long petId);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**petId** | **long** | ID of pet to check | 

### Return type

[bit_t](bit.md) *


### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **PetAPI_sharePicture**
```c
// Send a picture of your happy pet
//
char* PetAPI_sharePicture(apiClient_t *apiClient, binary_t* picture);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**picture** | **binary_t*** | A picture you want to share | 

### Return type

char*



### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **PetAPI_specialtyPet**
```c
// Specialty of the shop
//
// Returns the kind of pet the store specializes in
//
preference_t* PetAPI_specialtyPet(apiClient_t *apiClient);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |

### Return type

[preference_t](preference.md) *


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
void PetAPI_updatePet(apiClient_t *apiClient, pet_t *body);
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
void PetAPI_updatePetWithForm(apiClient_t *apiClient, long petId, char *name, char *status);
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
api_response_t* PetAPI_uploadFile(apiClient_t *apiClient, long petId, char *additionalMetadata, binary_t* file);
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

