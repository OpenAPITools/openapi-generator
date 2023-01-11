# .BehaviorApi

All URIs are relative to *http://petstore.swagger.io/v2*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**getBehaviorPermissions**](BehaviorApi.md#getBehaviorPermissions) | **GET** /fake_behavior/{behavior-id}/permissions | Get permissions for the behavior|
|[**getBehaviorType**](BehaviorApi.md#getBehaviorType) | **GET** /fake_behavior/{behavior-id}/type | Get the type of behavior|

# **getBehaviorPermissions**
> GetBehaviorPermissionsResponse getBehaviorPermissions()


### Example

```typescript
import * as ApiModule from '@openapitools/typescript-fetch-petstore';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.BehaviorApi = new ApiModule.BehaviorApi(configuration);

let behaviorId: number = 789; // number | 

apiInstance.getBehaviorPermissions(behaviorId).then((data: any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error: any) => {
  console.error(error);
});
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **behaviorId** | [**number**] |  | defaults to undefined|


### Return type

**GetBehaviorPermissionsResponse**

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

# **getBehaviorType**
> GetBehaviorTypeResponse getBehaviorType()


### Example

```typescript
import * as ApiModule from '@openapitools/typescript-fetch-petstore';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.BehaviorApi = new ApiModule.BehaviorApi(configuration);

let behaviorId: number = 789; // number | 

apiInstance.getBehaviorType(behaviorId).then((data: any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error: any) => {
  console.error(error);
});
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **behaviorId** | [**number**] |  | defaults to undefined|


### Return type

**GetBehaviorTypeResponse**

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


