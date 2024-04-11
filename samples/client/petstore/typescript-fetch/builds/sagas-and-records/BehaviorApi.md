# .BehaviorApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**getBehaviorPermissions**](BehaviorApi.md#getbehaviorpermissions) | **GET** /fake_behavior/{behavior-id}/permissions | Get permissions for the behavior
[**getBehaviorType**](BehaviorApi.md#getbehaviortype) | **GET** /fake_behavior/{behavior-id}/type | Get the type of behavior


## **getBehaviorPermissions**
> GetBehaviorPermissionsResponse getBehaviorPermissions()


### Example


```typescript
import { BehaviorApi } from '';

const apiInstance = new .BehaviorApi();

let body:.BehaviorApiGetBehaviorPermissionsRequest = {
    // number
    behaviorId: 789,
};

apiInstance.getBehaviorPermissions(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **behaviorId** | [**number**] |  | defaults to undefined


### Return type

[**GetBehaviorPermissionsResponse**](GetBehaviorPermissionsResponse.md)

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

## **getBehaviorType**
> GetBehaviorTypeResponse getBehaviorType()


### Example


```typescript
import { BehaviorApi } from '';

const apiInstance = new .BehaviorApi();

let body:.BehaviorApiGetBehaviorTypeRequest = {
    // number
    behaviorId: 789,
};

apiInstance.getBehaviorType(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **behaviorId** | [**number**] |  | defaults to undefined


### Return type

[**GetBehaviorTypeResponse**](GetBehaviorTypeResponse.md)

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


