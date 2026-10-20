# PetApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**addPet**](PetApi.md#addpet) | **POST** /pet | Add a new pet to the store |
| [**deletePet**](PetApi.md#deletepet) | **DELETE** /pet/{petId} | Deletes a pet |
| [**findPetsByStatus**](PetApi.md#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status |
| [**findPetsByTags**](PetApi.md#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags |
| [**getPetById**](PetApi.md#getpetbyid) | **GET** /pet/{petId} | Find pet by ID |
| [**updatePet**](PetApi.md#updatepet) | **PUT** /pet | Update an existing pet |
| [**updatePetWithForm**](PetApi.md#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data |
| [**uploadFile**](PetApi.md#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image |
| [**uploadFileWithRequiredFile**](PetApi.md#uploadfilewithrequiredfile) | **POST** /fake/{petId}/uploadImageWithRequiredFile | uploads an image (required) |



## addPet

> addPet(pet)

Add a new pet to the store



### Example

```ts
import {
  Configuration,
  PetApi,
} from '';
import type { AddPetRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const config = new Configuration({ 
    // To configure OAuth2 access token for authorization: petstore_auth implicit
    accessToken: "YOUR ACCESS TOKEN",
  });
  const api = new PetApi(config);

  const body = {
    // Pet | Pet object that needs to be added to the store
    pet: ...,
  } satisfies AddPetRequest;

  try {
    const data = await api.addPet(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **pet** | [Pet](Pet.md) | Pet object that needs to be added to the store | |

### Return type

`void` (Empty response body)

### Authorization

[petstore_auth implicit](../README.md#petstore_auth-implicit)

### HTTP request headers

- **Content-Type**: `application/json`, `application/xml`
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |
| **405** | Invalid input |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## deletePet

> deletePet(petId, apiKey)

Deletes a pet



### Example

```ts
import {
  Configuration,
  PetApi,
} from '';
import type { DeletePetRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const config = new Configuration({ 
    // To configure OAuth2 access token for authorization: petstore_auth implicit
    accessToken: "YOUR ACCESS TOKEN",
  });
  const api = new PetApi(config);

  const body = {
    // number | Pet id to delete
    petId: 789,
    // string (optional)
    apiKey: apiKey_example,
  } satisfies DeletePetRequest;

  try {
    const data = await api.deletePet(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **petId** | `number` | Pet id to delete | [Defaults to `undefined`] |
| **apiKey** | `string` |  | [Optional] [Defaults to `undefined`] |

### Return type

`void` (Empty response body)

### Authorization

[petstore_auth implicit](../README.md#petstore_auth-implicit)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |
| **400** | Invalid pet value |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## findPetsByStatus

> Array&lt;Pet&gt; findPetsByStatus(status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example

```ts
import {
  Configuration,
  PetApi,
} from '';
import type { FindPetsByStatusRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const config = new Configuration({ 
    // To configure OAuth2 access token for authorization: petstore_auth implicit
    accessToken: "YOUR ACCESS TOKEN",
  });
  const api = new PetApi(config);

  const body = {
    // Array<'available' | 'pending' | 'sold'> | Status values that need to be considered for filter
    status: ...,
  } satisfies FindPetsByStatusRequest;

  try {
    const data = await api.findPetsByStatus(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **status** | `available`, `pending`, `sold` | Status values that need to be considered for filter | [Enum: available, pending, sold] |

### Return type

[**Array&lt;Pet&gt;**](Pet.md)

### Authorization

[petstore_auth implicit](../README.md#petstore_auth-implicit)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `application/xml`, `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid status value |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## findPetsByTags

> Set&lt;Pet&gt; findPetsByTags(tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example

```ts
import {
  Configuration,
  PetApi,
} from '';
import type { FindPetsByTagsRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const config = new Configuration({ 
    // To configure OAuth2 access token for authorization: petstore_auth implicit
    accessToken: "YOUR ACCESS TOKEN",
  });
  const api = new PetApi(config);

  const body = {
    // Set<string> | Tags to filter by
    tags: ...,
  } satisfies FindPetsByTagsRequest;

  try {
    const data = await api.findPetsByTags(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **tags** | `Set<string>` | Tags to filter by | |

### Return type

[**Set&lt;Pet&gt;**](Pet.md)

### Authorization

[petstore_auth implicit](../README.md#petstore_auth-implicit)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `application/xml`, `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid tag value |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## getPetById

> Pet getPetById(petId)

Find pet by ID

Returns a single pet

### Example

```ts
import {
  Configuration,
  PetApi,
} from '';
import type { GetPetByIdRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const config = new Configuration({ 
    // To configure API key authorization: api_key
    apiKey: "YOUR API KEY",
  });
  const api = new PetApi(config);

  const body = {
    // number | ID of pet to return
    petId: 789,
  } satisfies GetPetByIdRequest;

  try {
    const data = await api.getPetById(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **petId** | `number` | ID of pet to return | [Defaults to `undefined`] |

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `application/xml`, `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid ID supplied |  -  |
| **404** | Pet not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## updatePet

> updatePet(pet)

Update an existing pet



### Example

```ts
import {
  Configuration,
  PetApi,
} from '';
import type { UpdatePetRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const config = new Configuration({ 
    // To configure OAuth2 access token for authorization: petstore_auth implicit
    accessToken: "YOUR ACCESS TOKEN",
  });
  const api = new PetApi(config);

  const body = {
    // Pet | Pet object that needs to be added to the store
    pet: ...,
  } satisfies UpdatePetRequest;

  try {
    const data = await api.updatePet(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **pet** | [Pet](Pet.md) | Pet object that needs to be added to the store | |

### Return type

`void` (Empty response body)

### Authorization

[petstore_auth implicit](../README.md#petstore_auth-implicit)

### HTTP request headers

- **Content-Type**: `application/json`, `application/xml`
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |
| **400** | Invalid ID supplied |  -  |
| **404** | Pet not found |  -  |
| **405** | Validation exception |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## updatePetWithForm

> updatePetWithForm(petId, name, status)

Updates a pet in the store with form data



### Example

```ts
import {
  Configuration,
  PetApi,
} from '';
import type { UpdatePetWithFormRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const config = new Configuration({ 
    // To configure OAuth2 access token for authorization: petstore_auth implicit
    accessToken: "YOUR ACCESS TOKEN",
  });
  const api = new PetApi(config);

  const body = {
    // number | ID of pet that needs to be updated
    petId: 789,
    // string | Updated name of the pet (optional)
    name: name_example,
    // string | Updated status of the pet (optional)
    status: status_example,
  } satisfies UpdatePetWithFormRequest;

  try {
    const data = await api.updatePetWithForm(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **petId** | `number` | ID of pet that needs to be updated | [Defaults to `undefined`] |
| **name** | `string` | Updated name of the pet | [Optional] [Defaults to `undefined`] |
| **status** | `string` | Updated status of the pet | [Optional] [Defaults to `undefined`] |

### Return type

`void` (Empty response body)

### Authorization

[petstore_auth implicit](../README.md#petstore_auth-implicit)

### HTTP request headers

- **Content-Type**: `application/x-www-form-urlencoded`
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |
| **405** | Invalid input |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## uploadFile

> ModelApiResponse uploadFile(petId, additionalMetadata, file)

uploads an image



### Example

```ts
import {
  Configuration,
  PetApi,
} from '';
import type { UploadFileRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const config = new Configuration({ 
    // To configure OAuth2 access token for authorization: petstore_auth implicit
    accessToken: "YOUR ACCESS TOKEN",
  });
  const api = new PetApi(config);

  const body = {
    // number | ID of pet to update
    petId: 789,
    // string | Additional data to pass to server (optional)
    additionalMetadata: additionalMetadata_example,
    // Blob | file to upload (optional)
    file: BINARY_DATA_HERE,
  } satisfies UploadFileRequest;

  try {
    const data = await api.uploadFile(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **petId** | `number` | ID of pet to update | [Defaults to `undefined`] |
| **additionalMetadata** | `string` | Additional data to pass to server | [Optional] [Defaults to `undefined`] |
| **file** | `Blob` | file to upload | [Optional] [Defaults to `undefined`] |

### Return type

[**ModelApiResponse**](ModelApiResponse.md)

### Authorization

[petstore_auth implicit](../README.md#petstore_auth-implicit)

### HTTP request headers

- **Content-Type**: `multipart/form-data`
- **Accept**: `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## uploadFileWithRequiredFile

> ModelApiResponse uploadFileWithRequiredFile(petId, requiredFile, additionalMetadata)

uploads an image (required)



### Example

```ts
import {
  Configuration,
  PetApi,
} from '';
import type { UploadFileWithRequiredFileRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const config = new Configuration({ 
    // To configure OAuth2 access token for authorization: petstore_auth implicit
    accessToken: "YOUR ACCESS TOKEN",
  });
  const api = new PetApi(config);

  const body = {
    // number | ID of pet to update
    petId: 789,
    // Blob | file to upload
    requiredFile: BINARY_DATA_HERE,
    // string | Additional data to pass to server (optional)
    additionalMetadata: additionalMetadata_example,
  } satisfies UploadFileWithRequiredFileRequest;

  try {
    const data = await api.uploadFileWithRequiredFile(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **petId** | `number` | ID of pet to update | [Defaults to `undefined`] |
| **requiredFile** | `Blob` | file to upload | [Defaults to `undefined`] |
| **additionalMetadata** | `string` | Additional data to pass to server | [Optional] [Defaults to `undefined`] |

### Return type

[**ModelApiResponse**](ModelApiResponse.md)

### Authorization

[petstore_auth implicit](../README.md#petstore_auth-implicit)

### HTTP request headers

- **Content-Type**: `multipart/form-data`
- **Accept**: `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

