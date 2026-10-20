# .DefaultApi

All URIs are relative to *http://api.example.xyz/v1*

Method | HTTP request | Description
------------- | ------------- | -------------
[**filePost**](DefaultApi.md#filePost) | **POST** /file | 
[**petsFilteredPatch**](DefaultApi.md#petsFilteredPatch) | **PATCH** /pets-filtered | 
[**petsPatch**](DefaultApi.md#petsPatch) | **PATCH** /pets | 


# **filePost**
> void filePost()


### Example


```typescript
import { createConfiguration, DefaultApi } from '';
import type { DefaultApiFilePostRequest } from '';

const configuration = createConfiguration();
const apiInstance = new DefaultApi(configuration);

const request: DefaultApiFilePostRequest = {
  
  filePostRequest: {
null,
  },
};

const data = await apiInstance.filePost(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **filePostRequest** | **FilePostRequest**|  |


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
import { createConfiguration, DefaultApi } from '';
import type { DefaultApiPetsFilteredPatchRequest } from '';

const configuration = createConfiguration();
const apiInstance = new DefaultApi(configuration);

const request: DefaultApiPetsFilteredPatchRequest = {
  
  petsFilteredPatchRequest: null,
};

const data = await apiInstance.petsFilteredPatch(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petsFilteredPatchRequest** | **PetsFilteredPatchRequest**|  |


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
import { createConfiguration, DefaultApi } from '';
import type { DefaultApiPetsPatchRequest } from '';

const configuration = createConfiguration();
const apiInstance = new DefaultApi(configuration);

const request: DefaultApiPetsPatchRequest = {
  
  petsPatchRequest: null,
};

const data = await apiInstance.petsPatch(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petsPatchRequest** | **PetsPatchRequest**|  |


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


