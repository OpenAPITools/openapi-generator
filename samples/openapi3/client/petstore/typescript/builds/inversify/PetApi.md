# petstore.PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

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


# **addPet**
> Pet addPet(pet)


### Example


```typescript
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.PetApi(configuration);

let body:petstore.PetApiAddPetRequest = {
  // Pet | Pet object that needs to be added to the store
  pet: {
    id: 1,
    category: {
      id: 1,
      name: "CbUUGjjNSwg0_bs9ZayIMrKdgNvb6gvxmPb9GcsM61ate1RA89q3w1l4eH4XxEz.5awLMdeXylwK0lMGUSM4jsrh4dstlnQUN5vVdMLPA",
    },
    name: "doggie",
    photoUrls: [
      "photoUrls_example",
    ],
    tags: [
      {
        id: 1,
        name: "name_example",
      },
    ],
    status: "available",
  },
};

apiInstance.addPet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | **Pet**| Pet object that needs to be added to the store |


### Return type

**Pet**

### Authorization

[petstore_auth](README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |
**405** | Invalid input |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **deletePet**
> deletePet()


### Example


```typescript
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.PetApi(configuration);

let body:petstore.PetApiDeletePetRequest = {
  // number | Pet id to delete
  petId: 1,
  // string (optional)
  apiKey: "api_key_example",
};

apiInstance.deletePet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | [**number**] | Pet id to delete | defaults to undefined
 **apiKey** | [**string**] |  | (optional) defaults to undefined


### Return type

void (empty response body)

### Authorization

[petstore_auth](README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**400** | Invalid pet value |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **findPetsByStatus**
> Array<Pet> findPetsByStatus()

Multiple status values can be provided with comma separated strings

### Example


```typescript
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.PetApi(configuration);

let body:petstore.PetApiFindPetsByStatusRequest = {
  // Array<'available' | 'pending' | 'sold'> | Status values that need to be considered for filter
  status: [
    "available",
  ],
};

apiInstance.findPetsByStatus(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | **Array<&#39;available&#39; &#124; &#39;pending&#39; &#124; &#39;sold&#39;>** | Status values that need to be considered for filter | defaults to undefined


### Return type

**Array<Pet>**

### Authorization

[petstore_auth](README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |
**400** | Invalid status value |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **findPetsByTags**
> Array<Pet> findPetsByTags()

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example


```typescript
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.PetApi(configuration);

let body:petstore.PetApiFindPetsByTagsRequest = {
  // Array<string> | Tags to filter by
  tags: [
    "tags_example",
  ],
};

apiInstance.findPetsByTags(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | **Array&lt;string&gt;** | Tags to filter by | defaults to undefined


### Return type

**Array<Pet>**

### Authorization

[petstore_auth](README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |
**400** | Invalid tag value |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **getPetById**
> Pet getPetById()

Returns a single pet

### Example


```typescript
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.PetApi(configuration);

let body:petstore.PetApiGetPetByIdRequest = {
  // number | ID of pet to return
  petId: 1,
};

apiInstance.getPetById(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | [**number**] | ID of pet to return | defaults to undefined


### Return type

**Pet**

### Authorization

[api_key](README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |
**400** | Invalid ID supplied |  -  |
**404** | Pet not found |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **updatePet**
> Pet updatePet(pet)


### Example


```typescript
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.PetApi(configuration);

let body:petstore.PetApiUpdatePetRequest = {
  // Pet | Pet object that needs to be added to the store
  pet: {
    id: 1,
    category: {
      id: 1,
      name: "CbUUGjjNSwg0_bs9ZayIMrKdgNvb6gvxmPb9GcsM61ate1RA89q3w1l4eH4XxEz.5awLMdeXylwK0lMGUSM4jsrh4dstlnQUN5vVdMLPA",
    },
    name: "doggie",
    photoUrls: [
      "photoUrls_example",
    ],
    tags: [
      {
        id: 1,
        name: "name_example",
      },
    ],
    status: "available",
  },
};

apiInstance.updatePet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | **Pet**| Pet object that needs to be added to the store |


### Return type

**Pet**

### Authorization

[petstore_auth](README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |
**400** | Invalid ID supplied |  -  |
**404** | Pet not found |  -  |
**405** | Validation exception |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **updatePetWithForm**
> updatePetWithForm()


### Example


```typescript
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.PetApi(configuration);

let body:petstore.PetApiUpdatePetWithFormRequest = {
  // number | ID of pet that needs to be updated
  petId: 1,
  // string | Updated name of the pet (optional)
  name: "name_example",
  // string | Updated status of the pet (optional)
  status: "status_example",
};

apiInstance.updatePetWithForm(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | [**number**] | ID of pet that needs to be updated | defaults to undefined
 **name** | [**string**] | Updated name of the pet | (optional) defaults to undefined
 **status** | [**string**] | Updated status of the pet | (optional) defaults to undefined


### Return type

void (empty response body)

### Authorization

[petstore_auth](README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**405** | Invalid input |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **uploadFile**
> ApiResponse uploadFile()


### Example


```typescript
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.PetApi(configuration);

let body:petstore.PetApiUploadFileRequest = {
  // number | ID of pet to update
  petId: 1,
  // string | Additional data to pass to server (optional)
  additionalMetadata: "additionalMetadata_example",
  // HttpFile | file to upload (optional)
  file: { data: Buffer.from(fs.readFileSync('/path/to/file', 'utf-8')), name: '/path/to/file' },
};

apiInstance.uploadFile(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | [**number**] | ID of pet to update | defaults to undefined
 **additionalMetadata** | [**string**] | Additional data to pass to server | (optional) defaults to undefined
 **file** | [**HttpFile**] | file to upload | (optional) defaults to undefined


### Return type

**ApiResponse**

### Authorization

[petstore_auth](README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


