# AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**_123testSpecialTags**](#_123testspecialtags) | **PATCH** /another-fake/dummy | To test special tags|

# **_123testSpecialTags**
> Client _123testSpecialTags(client)

To test special tags and operation ID starting with number

### Example

```typescript
import {
    AnotherFakeApi,
    Configuration,
    Client
} from './api';

const configuration = new Configuration();
const apiInstance = new AnotherFakeApi(configuration);

let client: Client; //client model

const { status, data } = await apiInstance._123testSpecialTags(
    client
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **client** | **Client**| client model | |


### Return type

**Client**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

