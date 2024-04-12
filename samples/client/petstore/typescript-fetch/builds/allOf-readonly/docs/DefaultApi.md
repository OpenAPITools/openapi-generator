# .DefaultApi

All URIs are relative to *http://api.example.xyz/v1*

Method | HTTP request | Description
------------- | ------------- | -------------
[**list**](DefaultApi.md#list) | **GET** /person/display/{personId} | 


## **list**
> Club list()


### Example


```typescript
import { DefaultApi } from '';

const apiInstance = new .DefaultApi();

let body:.DefaultApiListRequest = {
    // string | The id of the person to retrieve
    personId: personId_example,
};

apiInstance.list(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **personId** | [**string**] | The id of the person to retrieve | defaults to undefined


### Return type

[**Club**](Club.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


