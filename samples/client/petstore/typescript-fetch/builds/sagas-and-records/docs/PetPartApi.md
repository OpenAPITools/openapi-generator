# .PetPartApi

All URIs are relative to *http://petstore.swagger.io/v2*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**getFakePetPartType**](PetPartApi.md#getFakePetPartType) | **GET** /fake_petParts/{fake_petPart-id}/part-type | Returns single pet part type for the petPart id.|
|[**getMatchingParts**](PetPartApi.md#getMatchingParts) | **GET** /fake_petParts/{fake_petPart-id}/matching-parts | Get the matching parts for the given pet part.|

# **getFakePetPartType**
> GetPetPartTypeResponse getFakePetPartType()


### Example

```typescript
import * as ApiModule from '@openapitools/typescript-fetch-petstore';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.PetPartApi = new ApiModule.PetPartApi(configuration);

let fakePetPartId: number = 789; // number | 

apiInstance.getFakePetPartType(fakePetPartId).then((data: any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error: any) => {
  console.error(error);
});
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **fakePetPartId** | [**number**] |  | defaults to undefined|


### Return type

**GetPetPartTypeResponse**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **getMatchingParts**
> GetMatchingPartsResponse getMatchingParts()


### Example

```typescript
import * as ApiModule from '@openapitools/typescript-fetch-petstore';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.PetPartApi = new ApiModule.PetPartApi(configuration);

let fakePetPartId: number = 789; // number | 
let _long: boolean = true; // boolean | 
let smooth: boolean = true; // boolean | 
let _short: boolean = true; // boolean | 
const name: ApiModule.string = {
  // set here some attributes...
}; , // string | 
const connectedPart: ApiModule.string = {
  // set here some attributes...
};  // string | 

apiInstance.getMatchingParts(fakePetPartId, _long, smooth, _short, connectedPart).then((data: any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error: any) => {
  console.error(error);
});
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **fakePetPartId** | [**number**] |  | defaults to undefined|
| **_long** | [**boolean**] |  | defaults to undefined|
| **smooth** | [**boolean**] |  | defaults to undefined|
| **_short** | [**boolean**] |  | defaults to undefined|
| **name** | [**string**] |  | (optional) defaults to undefined|
| **connectedPart** | [**string**] |  | (optional) defaults to undefined|


### Return type

**GetMatchingPartsResponse**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


