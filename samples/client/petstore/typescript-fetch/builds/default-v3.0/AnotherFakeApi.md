# .AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**_123testSpecialTags**](AnotherFakeApi.md#_123testspecialtags) | **PATCH** /another-fake/dummy | To test special tags


## **_123testSpecialTags**
> Client _123testSpecialTags(client)

To test special tags and operation ID starting with number

### Example


```typescript
import { AnotherFakeApi } from '';

const apiInstance = new .AnotherFakeApi();

let body:.AnotherFakeApi123testSpecialTagsRequest = {
    // Client | client model
    client: ,
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

[**Client**](Client.md)

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


