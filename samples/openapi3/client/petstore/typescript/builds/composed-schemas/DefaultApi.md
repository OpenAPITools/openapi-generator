# petstore.DefaultApi

All URIs are relative to *http://api.example.xyz/v1*

Method | HTTP request | Description
------------- | ------------- | -------------
[**filePost**](DefaultApi.md#filePost) | **POST** /file | 
[**petsFilteredPatch**](DefaultApi.md#petsFilteredPatch) | **PATCH** /pets-filtered | 
[**petsPatch**](DefaultApi.md#petsPatch) | **PATCH** /pets | 
[**sizesPut**](DefaultApi.md#sizesPut) | **PUT** /sizes | 


# **filePost**
> void filePost()


### Example


```typescript
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.DefaultApi(configuration);

let body:petstore.DefaultApiFilePostRequest = {
  // InlineObject (optional)
  inlineObject: {
    file: ,
  },
};

apiInstance.filePost(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **inlineObject** | **InlineObject**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | File uploaded |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **petsFilteredPatch**
> void petsFilteredPatch()


### Example


```typescript
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.DefaultApi(configuration);

let body:petstore.DefaultApiPetsFilteredPatchRequest = {
  // PetByAge | PetByType (optional)
  petByAgePetByType: ,
};

apiInstance.petsFilteredPatch(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petByAgePetByType** | **PetByAge | PetByType**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Updated |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **petsPatch**
> void petsPatch()


### Example


```typescript
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.DefaultApi(configuration);

let body:petstore.DefaultApiPetsPatchRequest = {
  // Cat | Dog (optional)
  catDog: ,
};

apiInstance.petsPatch(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **catDog** | **Cat | Dog**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Updated |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **sizesPut**
> void sizesPut()


### Example


```typescript
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.DefaultApi(configuration);

let body:petstore.DefaultApiSizesPutRequest = {
  // Sizes (optional)
  sizes: {
    data: [
      [
        1.575317847E9,
        12.3,
      ],
    ],
    dataWithExamples: [
      [
        "a",
        "b",
      ],
      [
        "c",
        "d",
      ],
    ],
  },
};

apiInstance.sizesPut(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **sizes** | **Sizes**|  |


### Return type

**void**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Created |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


