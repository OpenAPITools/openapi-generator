# BehaviorApi

All URIs are relative to *http://petstore.swagger.io/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**getBehaviorPermissions**](BehaviorApi.md#getbehaviorpermissions) | **GET** /fake_behavior/{behavior-id}/permissions | Get permissions for the behavior |
| [**getBehaviorType**](BehaviorApi.md#getbehaviortype) | **GET** /fake_behavior/{behavior-id}/type | Get the type of behavior |



## getBehaviorPermissions

> GetBehaviorPermissionsResponse getBehaviorPermissions(behaviorId)

Get permissions for the behavior

### Example

```ts
import {
  Configuration,
  BehaviorApi,
} from '@openapitools/typescript-fetch-petstore';
import type { GetBehaviorPermissionsRequest } from '@openapitools/typescript-fetch-petstore';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-petstore SDK...");
  const api = new BehaviorApi();

  const body = {
    // number
    behaviorId: 789,
  } satisfies GetBehaviorPermissionsRequest;

  try {
    const data = await api.getBehaviorPermissions(body);
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
| **behaviorId** | `number` |  | [Defaults to `undefined`] |

### Return type

[**GetBehaviorPermissionsResponse**](GetBehaviorPermissionsResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `*/*`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## getBehaviorType

> GetBehaviorTypeResponse getBehaviorType(behaviorId)

Get the type of behavior

### Example

```ts
import {
  Configuration,
  BehaviorApi,
} from '@openapitools/typescript-fetch-petstore';
import type { GetBehaviorTypeRequest } from '@openapitools/typescript-fetch-petstore';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-petstore SDK...");
  const api = new BehaviorApi();

  const body = {
    // number
    behaviorId: 789,
  } satisfies GetBehaviorTypeRequest;

  try {
    const data = await api.getBehaviorType(body);
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
| **behaviorId** | `number` |  | [Defaults to `undefined`] |

### Return type

[**GetBehaviorTypeResponse**](GetBehaviorTypeResponse.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `*/*`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

