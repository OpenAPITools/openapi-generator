# AuthApi

All URIs are relative to *http://localhost:3000*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**testAuthHttpBasic**](#testauthhttpbasic) | **POST** /auth/http/basic | To test HTTP basic authentication|
|[**testAuthHttpBearer**](#testauthhttpbearer) | **POST** /auth/http/bearer | To test HTTP bearer authentication|

# **testAuthHttpBasic**
> string testAuthHttpBasic()

To test HTTP basic authentication

### Example

```typescript
import {
    AuthApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new AuthApi(configuration);

const { status, data } = await apiInstance.testAuthHttpBasic();
```

### Parameters
This endpoint does not have any parameters.


### Return type

**string**

### Authorization

[http_auth](../README.md#http_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testAuthHttpBearer**
> string testAuthHttpBearer()

To test HTTP bearer authentication

### Example

```typescript
import {
    AuthApi,
    Configuration
} from '@openapitools/typescript-axios-echo-api';

const configuration = new Configuration();
const apiInstance = new AuthApi(configuration);

const { status, data } = await apiInstance.testAuthHttpBearer();
```

### Parameters
This endpoint does not have any parameters.


### Return type

**string**

### Authorization

[http_bearer_auth](../README.md#http_bearer_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

