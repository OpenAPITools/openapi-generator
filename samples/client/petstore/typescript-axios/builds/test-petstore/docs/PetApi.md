# PetApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**addPet**](#addpet) | **POST** /pet | Add a new pet to the store|
|[**deletePet**](#deletepet) | **DELETE** /pet/{petId} | Deletes a pet|
|[**findPetsByStatus**](#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status|
|[**findPetsByTags**](#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags|
|[**getPetById**](#getpetbyid) | **GET** /pet/{petId} | Find pet by ID|
|[**updatePet**](#updatepet) | **PUT** /pet | Update an existing pet|
|[**updatePetWithForm**](#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data|
|[**uploadFile**](#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image|
|[**uploadFileWithRequiredFile**](#uploadfilewithrequiredfile) | **POST** /fake/{petId}/uploadImageWithRequiredFile | uploads an image (required)|

# **addPet**
> addPet(pet)



### Example

```typescript
import {
    PetApi,
    Configuration,
    Pet
} from './api';

const configuration = new Configuration();
const apiInstance = new PetApi(configuration);

let pet: Pet; //Pet object that needs to be added to the store

const { status, data } = await apiInstance.addPet(
    pet
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **pet** | **Pet**| Pet object that needs to be added to the store | |


### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth), [http_signature_test](../README.md#http_signature_test)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**405** | Invalid input |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **deletePet**
> deletePet()



### Example

```typescript
import {
    PetApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new PetApi(configuration);

let petId: number; //Pet id to delete (default to undefined)
let apiKey: string; // (optional) (default to undefined)

const { status, data } = await apiInstance.deletePet(
    petId,
    apiKey
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **petId** | [**number**] | Pet id to delete | defaults to undefined|
| **apiKey** | [**string**] |  | (optional) defaults to undefined|


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
|**400** | Invalid pet value |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **findPetsByStatus**
> Array<Pet> findPetsByStatus()

Multiple status values can be provided with comma separated strings

### Example

```typescript
import {
    PetApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new PetApi(configuration);

let status: Array<'available' | 'pending' | 'sold'>; //Status values that need to be considered for filter (default to undefined)

const { status, data } = await apiInstance.findPetsByStatus(
    status
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **status** | **Array<&#39;available&#39; &#124; &#39;pending&#39; &#124; &#39;sold&#39;>** | Status values that need to be considered for filter | defaults to undefined|


### Return type

**Array<Pet>**

### Authorization

[petstore_auth](../README.md#petstore_auth), [http_signature_test](../README.md#http_signature_test)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | successful operation |  -  |
|**400** | Invalid status value |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **findPetsByTags**
> Array<Pet> findPetsByTags()

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example

```typescript
import {
    PetApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new PetApi(configuration);

let tags: Array<string>; //Tags to filter by (default to undefined)

const { status, data } = await apiInstance.findPetsByTags(
    tags
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **tags** | **Array&lt;string&gt;** | Tags to filter by | defaults to undefined|


### Return type

**Array<Pet>**

### Authorization

[petstore_auth](../README.md#petstore_auth), [http_signature_test](../README.md#http_signature_test)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | successful operation |  -  |
|**400** | Invalid tag value |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getPetById**
> Pet getPetById()

Returns a single pet

### Example

```typescript
import {
    PetApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new PetApi(configuration);

let petId: number; //ID of pet to return (default to undefined)

const { status, data } = await apiInstance.getPetById(
    petId
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **petId** | [**number**] | ID of pet to return | defaults to undefined|


### Return type

**Pet**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | successful operation |  -  |
|**400** | Invalid ID supplied |  -  |
|**404** | Pet not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updatePet**
> updatePet(pet)



### Example

```typescript
import {
    PetApi,
    Configuration,
    Pet
} from './api';

const configuration = new Configuration();
const apiInstance = new PetApi(configuration);

let pet: Pet; //Pet object that needs to be added to the store

const { status, data } = await apiInstance.updatePet(
    pet
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **pet** | **Pet**| Pet object that needs to be added to the store | |


### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth), [http_signature_test](../README.md#http_signature_test)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**400** | Invalid ID supplied |  -  |
|**404** | Pet not found |  -  |
|**405** | Validation exception |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updatePetWithForm**
> updatePetWithForm()



### Example

```typescript
import {
    PetApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new PetApi(configuration);

let petId: number; //ID of pet that needs to be updated (default to undefined)
let name: string; //Updated name of the pet (optional) (default to undefined)
let status: string; //Updated status of the pet (optional) (default to undefined)

const { status, data } = await apiInstance.updatePetWithForm(
    petId,
    name,
    status
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **petId** | [**number**] | ID of pet that needs to be updated | defaults to undefined|
| **name** | [**string**] | Updated name of the pet | (optional) defaults to undefined|
| **status** | [**string**] | Updated status of the pet | (optional) defaults to undefined|


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
|**405** | Invalid input |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **uploadFile**
> ApiResponse uploadFile()



### Example

```typescript
import {
    PetApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new PetApi(configuration);

let petId: number; //ID of pet to update (default to undefined)
let additionalMetadata: string; //Additional data to pass to server (optional) (default to undefined)
let file: File; //file to upload (optional) (default to undefined)

const { status, data } = await apiInstance.uploadFile(
    petId,
    additionalMetadata,
    file
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **petId** | [**number**] | ID of pet to update | defaults to undefined|
| **additionalMetadata** | [**string**] | Additional data to pass to server | (optional) defaults to undefined|
| **file** | [**File**] | file to upload | (optional) defaults to undefined|


### Return type

**ApiResponse**

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **uploadFileWithRequiredFile**
> ApiResponse uploadFileWithRequiredFile()



### Example

```typescript
import {
    PetApi,
    Configuration
} from './api';

const configuration = new Configuration();
const apiInstance = new PetApi(configuration);

let petId: number; //ID of pet to update (default to undefined)
let requiredFile: File; //file to upload (default to undefined)
let additionalMetadata: string; //Additional data to pass to server (optional) (default to undefined)

const { status, data } = await apiInstance.uploadFileWithRequiredFile(
    petId,
    requiredFile,
    additionalMetadata
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **petId** | [**number**] | ID of pet to update | defaults to undefined|
| **requiredFile** | [**File**] | file to upload | defaults to undefined|
| **additionalMetadata** | [**string**] | Additional data to pass to server | (optional) defaults to undefined|


### Return type

**ApiResponse**

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

