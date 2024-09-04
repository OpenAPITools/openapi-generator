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
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.AnotherFakeApi(configuration);

let body:petstore.AnotherFakeApi123testSpecialTagsRequest = {
  // Client | client model
  client: {
    client: "client_example",
  },
};

apiInstance._123testSpecialTags(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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


