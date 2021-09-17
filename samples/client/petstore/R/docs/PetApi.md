# PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**AddPet**](PetApi.md#AddPet) | **POST** /pet | Add a new pet to the store
[**DeletePet**](PetApi.md#DeletePet) | **DELETE** /pet/{petId} | Deletes a pet
[**FindPetsByStatus**](PetApi.md#FindPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
[**FindPetsByTags**](PetApi.md#FindPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
[**GetPetById**](PetApi.md#GetPetById) | **GET** /pet/{petId} | Find pet by ID
[**UpdatePet**](PetApi.md#UpdatePet) | **PUT** /pet | Update an existing pet
[**UpdatePetWithForm**](PetApi.md#UpdatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**UploadFile**](PetApi.md#UploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image


# **AddPet**
> AddPet(body)

Add a new pet to the store

### Example
```R
library(petstore)

var.body <- Pet$new("name_example", list("photoUrls_example"), 123, Category$new(123, "name_example"), list(Tag$new(123, "name_example")), "available") # Pet | Pet object that needs to be added to the store

#Add a new pet to the store
api.instance <- PetApi$new()
# Configure OAuth2 access token for authorization: petstore_auth
api.instance$apiClient$accessToken <- 'TODO_YOUR_ACCESS_TOKEN';
api.instance$AddPet(var.body)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **405** | Invalid input |  -  |

# **DeletePet**
> DeletePet(pet.id, api.key=var.api.key)

Deletes a pet

### Example
```R
library(petstore)

var.pet.id <- 56 # integer | Pet id to delete
var.api.key <- 'api.key_example' # character | 

#Deletes a pet
api.instance <- PetApi$new()
# Configure OAuth2 access token for authorization: petstore_auth
api.instance$apiClient$accessToken <- 'TODO_YOUR_ACCESS_TOKEN';
api.instance$DeletePet(var.pet.id, api.key=var.api.key)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet.id** | **integer**| Pet id to delete | 
 **api.key** | **character**|  | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid pet value |  -  |

# **FindPetsByStatus**
> array[Pet] FindPetsByStatus(status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```R
library(petstore)

var.status <- list("available") # array[character] | Status values that need to be considered for filter

#Finds Pets by status
api.instance <- PetApi$new()
# Configure OAuth2 access token for authorization: petstore_auth
api.instance$apiClient$accessToken <- 'TODO_YOUR_ACCESS_TOKEN';
result <- api.instance$FindPetsByStatus(var.status)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | Enum [available, pending, sold] | Status values that need to be considered for filter | 

### Return type

[**array[Pet]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid status value |  -  |

# **FindPetsByTags**
> array[Pet] FindPetsByTags(tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```R
library(petstore)

var.tags <- list("inner_example") # array[character] | Tags to filter by

#Finds Pets by tags
api.instance <- PetApi$new()
# Configure OAuth2 access token for authorization: petstore_auth
api.instance$apiClient$accessToken <- 'TODO_YOUR_ACCESS_TOKEN';
result <- api.instance$FindPetsByTags(var.tags)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | list( **character** )| Tags to filter by | 

### Return type

[**array[Pet]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid tag value |  -  |

# **GetPetById**
> Pet GetPetById(pet.id)

Find pet by ID

Returns a single pet

### Example
```R
library(petstore)

var.pet.id <- 56 # integer | ID of pet to return

#Find pet by ID
api.instance <- PetApi$new()
# Configure API key authorization: api_key
api.instance$apiClient$apiKeys['api_key'] <- 'TODO_YOUR_API_KEY';
result <- api.instance$GetPetById(var.pet.id)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet.id** | **integer**| ID of pet to return | 

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid ID supplied |  -  |
| **404** | Pet not found |  -  |

# **UpdatePet**
> UpdatePet(body)

Update an existing pet

### Example
```R
library(petstore)

var.body <- Pet$new("name_example", list("photoUrls_example"), 123, Category$new(123, "name_example"), list(Tag$new(123, "name_example")), "available") # Pet | Pet object that needs to be added to the store

#Update an existing pet
api.instance <- PetApi$new()
# Configure OAuth2 access token for authorization: petstore_auth
api.instance$apiClient$accessToken <- 'TODO_YOUR_ACCESS_TOKEN';
api.instance$UpdatePet(var.body)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid ID supplied |  -  |
| **404** | Pet not found |  -  |
| **405** | Validation exception |  -  |

# **UpdatePetWithForm**
> UpdatePetWithForm(pet.id, name=var.name, status=var.status)

Updates a pet in the store with form data

### Example
```R
library(petstore)

var.pet.id <- 56 # integer | ID of pet that needs to be updated
var.name <- 'name_example' # character | Updated name of the pet
var.status <- 'status_example' # character | Updated status of the pet

#Updates a pet in the store with form data
api.instance <- PetApi$new()
# Configure OAuth2 access token for authorization: petstore_auth
api.instance$apiClient$accessToken <- 'TODO_YOUR_ACCESS_TOKEN';
api.instance$UpdatePetWithForm(var.pet.id, name=var.name, status=var.status)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet.id** | **integer**| ID of pet that needs to be updated | 
 **name** | **character**| Updated name of the pet | [optional] 
 **status** | **character**| Updated status of the pet | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **405** | Invalid input |  -  |

# **UploadFile**
> ModelApiResponse UploadFile(pet.id, additional.metadata=var.additional.metadata, file=var.file)

uploads an image

### Example
```R
library(petstore)

var.pet.id <- 56 # integer | ID of pet to update
var.additional.metadata <- 'additional.metadata_example' # character | Additional data to pass to server
var.file <- File.new('/path/to/file') # data.frame | file to upload

#uploads an image
api.instance <- PetApi$new()
# Configure OAuth2 access token for authorization: petstore_auth
api.instance$apiClient$accessToken <- 'TODO_YOUR_ACCESS_TOKEN';
result <- api.instance$UploadFile(var.pet.id, additional.metadata=var.additional.metadata, file=var.file)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet.id** | **integer**| ID of pet to update | 
 **additional.metadata** | **character**| Additional data to pass to server | [optional] 
 **file** | **data.frame**| file to upload | [optional] 

### Return type

[**ModelApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

