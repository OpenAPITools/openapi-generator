# .PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](PetApi.md#addpet) | **POST** /pet | Add a new pet to the store
[**deletePet**](PetApi.md#deletepet) | **DELETE** /pet/{petId} | Deletes a pet
[**findPetsByIds**](PetApi.md#findpetsbyids) | **GET** /pet/findByIds | Finds Pets by ids
[**findPetsByStatus**](PetApi.md#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status
[**findPetsByTags**](PetApi.md#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags
[**findPetsByUserIds**](PetApi.md#findpetsbyuserids) | **GET** /pet/findByUserIds | Finds Pets by user ids
[**getPetById**](PetApi.md#getpetbyid) | **GET** /pet/{petId} | Find pet by ID
[**getPetRegions**](PetApi.md#getpetregions) | **GET** /pet/{petId}/regions | Gets regions for a single pet.
[**updatePet**](PetApi.md#updatepet) | **PUT** /pet | Update an existing pet
[**updatePetRegions**](PetApi.md#updatepetregions) | **PUT** /pet/{petId}/regions | Updates the pet regions.
[**updatePetWithForm**](PetApi.md#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**uploadFile**](PetApi.md#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image


## **addPet**
> addPet(dummyCat)


### Example


```typescript
import { PetApi } from '';

const apiInstance = new .PetApi();

let body:.PetApiAddPetRequest = {
    // Category | dummy category for testing
    dummyCat: ,
};

apiInstance.addPet(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **dummyCat** | **Category**| dummy category for testing |


### Return type

void (empty response body)

### Authorization

[petstore_auth](README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: application/json, application/xml
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**405** | Invalid input |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

## **deletePet**
> deletePet()


### Example


```typescript
import { PetApi } from '';

const apiInstance = new .PetApi();

let body:.PetApiDeletePetRequest = {
    // number | Pet id to delete
    petId: 789,
    // string (optional)
    apiKey: apiKey_example,
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

## **findPetsByIds**
> Array<Pet> findPetsByIds()

Multiple ids can be provided with comma separated strings.

### Example


```typescript
import { PetApi } from '';

const apiInstance = new .PetApi();

let body:.PetApiFindPetsByIdsRequest = {
    // Array<number> | Ids to filter by
    ids: ,
};

apiInstance.findPetsByIds(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ids** | **Array&lt;number&gt;** | Ids to filter by | defaults to undefined


### Return type

[**Array<Pet>**](Pet.md)

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

## **findPetsByStatus**
> FindPetsByStatusResponse findPetsByStatus()

Multiple status values can be provided with comma separated strings

### Example


```typescript
import { PetApi } from '';

const apiInstance = new .PetApi();

let body:.PetApiFindPetsByStatusRequest = {
    // Array<'available' | 'pending' | 'sold'> | Status values that need to be considered for filter
    status: ,
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

[**FindPetsByStatusResponse**](FindPetsByStatusResponse.md)

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

## **findPetsByTags**
> Array<Pet> findPetsByTags()

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example


```typescript
import { PetApi } from '';

const apiInstance = new .PetApi();

let body:.PetApiFindPetsByTagsRequest = {
    // Array<string> | Tags to filter by
    tags: ,
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

[**Array<Pet>**](Pet.md)

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

## **findPetsByUserIds**
> FindPetsByUserResponse findPetsByUserIds()

Multiple ids can be provided with comma separated strings.

### Example


```typescript
import { PetApi } from '';

const apiInstance = new .PetApi();

let body:.PetApiFindPetsByUserIdsRequest = {
    // Array<number> | Ids to filter by
    ids: ,
};

apiInstance.findPetsByUserIds(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ids** | **Array&lt;number&gt;** | Ids to filter by | defaults to undefined


### Return type

[**FindPetsByUserResponse**](FindPetsByUserResponse.md)

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

## **getPetById**
> Pet getPetById()

Returns a single pet

### Example


```typescript
import { PetApi } from '';

const apiInstance = new .PetApi();

let body:.PetApiGetPetByIdRequest = {
    // number | ID of pet to return
    petId: 789,
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

[**Pet**](Pet.md)

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

## **getPetRegions**
> PetRegionsResponse getPetRegions()


### Example


```typescript
import { PetApi } from '';

const apiInstance = new .PetApi();

let body:.PetApiGetPetRegionsRequest = {
    // number | ID of pet
    petId: 789,
};

apiInstance.getPetRegions(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | [**number**] | ID of pet | defaults to undefined


### Return type

[**PetRegionsResponse**](PetRegionsResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

## **updatePet**
> updatePet(body)


### Example


```typescript
import { PetApi } from '';

const apiInstance = new .PetApi();

let body:.PetApiUpdatePetRequest = {
    // Pet | Pet object that needs to be updated in the store
    body: ,
};

apiInstance.updatePet(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **Pet**| Pet object that needs to be updated in the store |


### Return type

void (empty response body)

### Authorization

[petstore_auth](README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: application/json, application/xml
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**400** | Invalid ID supplied |  -  |
**404** | Pet not found |  -  |
**405** | Validation exception |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

## **updatePetRegions**
> PetRegionsResponse updatePetRegions(newRegions)


### Example


```typescript
import { PetApi } from '';

const apiInstance = new .PetApi();

let body:.PetApiUpdatePetRegionsRequest = {
    // number | ID of pet
    petId: 789,
    // Array<Array<number>>
    newRegions: ,
};

apiInstance.updatePetRegions(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **newRegions** | **Array<Array<number>>**|  |
 **petId** | [**number**] | ID of pet | defaults to undefined


### Return type

[**PetRegionsResponse**](PetRegionsResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

## **updatePetWithForm**
> updatePetWithForm()


### Example


```typescript
import { PetApi } from '';

const apiInstance = new .PetApi();

let body:.PetApiUpdatePetWithFormRequest = {
    // number | ID of pet that needs to be updated
    petId: 789,
    // string | Updated name of the pet (optional)
    name: name_example,
    // string | Updated status of the pet (optional)
    status: status_example,
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

## **uploadFile**
> ModelApiResponse uploadFile()


### Example


```typescript
import { PetApi } from '';

const apiInstance = new .PetApi();

let body:.PetApiUploadFileRequest = {
    // number | ID of pet to update
    petId: 789,
    // string | Additional data to pass to server (optional)
    additionalMetadata: additionalMetadata_example,
    // Blob | file to upload (optional)
    file: BINARY_DATA_HERE,
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
 **file** | [**Blob**] | file to upload | (optional) defaults to undefined


### Return type

[**ModelApiResponse**](ModelApiResponse.md)

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


