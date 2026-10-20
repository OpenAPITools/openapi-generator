# DefaultApi

All URIs are relative to *http://api.example.xyz/v1*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**filePost**](#filepost) | **POST** /file | |
|[**petsFilteredPatch**](#petsfilteredpatch) | **PATCH** /pets-filtered | |
|[**petsPatch**](#petspatch) | **PATCH** /pets | |

# **filePost**
> filePost()


### Example

```typescript
import {
    DefaultApi,
    Configuration,
    FilePostRequest
} from './api';

const configuration = new Configuration();
const apiInstance = new DefaultApi(configuration);

let filePostRequest: FilePostRequest; // (optional)

const { status, data } = await apiInstance.filePost(
    filePostRequest
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **filePostRequest** | **FilePostRequest**|  | |


### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | File uploaded |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **petsFilteredPatch**
> petsFilteredPatch()


### Example

```typescript
import {
    DefaultApi,
    Configuration,
    PetsFilteredPatchRequest
} from './api';

const configuration = new Configuration();
const apiInstance = new DefaultApi(configuration);

let petsFilteredPatchRequest: PetsFilteredPatchRequest; // (optional)

const { status, data } = await apiInstance.petsFilteredPatch(
    petsFilteredPatchRequest
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **petsFilteredPatchRequest** | **PetsFilteredPatchRequest**|  | |


### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | Updated |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **petsPatch**
> petsPatch()


### Example

```typescript
import {
    DefaultApi,
    Configuration,
    PetsPatchRequest
} from './api';

const configuration = new Configuration();
const apiInstance = new DefaultApi(configuration);

let petsPatchRequest: PetsPatchRequest; // (optional)

const { status, data } = await apiInstance.petsPatch(
    petsPatchRequest
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **petsPatchRequest** | **PetsPatchRequest**|  | |


### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | Updated |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

