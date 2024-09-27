# petstore.AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**_123testSpecialTags**](AnotherFakeApi.md#_123testSpecialTags) | **PATCH** /another-fake/dummy | To test special tags


# **_123testSpecialTags**
> Client _123testSpecialTags(client)

To test special tags and operation ID starting with number

### Example


```typescript
import { createConfiguration, AnotherFakeApi } from 'ts-petstore-client';
import type { AnotherFakeApi123testSpecialTagsRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new AnotherFakeApi(configuration);

const request: AnotherFakeApi123testSpecialTagsRequest = {
    // client model
  client: {
    client: "client_example",
  },
};

const data = await apiInstance._123testSpecialTags(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | **Client**| client model |


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
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


