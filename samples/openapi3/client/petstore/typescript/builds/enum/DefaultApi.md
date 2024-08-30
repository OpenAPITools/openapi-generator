# petstore.DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**interactionsGet**](DefaultApi.md#interactionsGet) | **GET** /interactions | Returns all interactions.


# **interactionsGet**
> Interaction interactionsGet()

Optional extended description in Markdown.

### Example


```typescript
import { petstore } from 'ts-petstore-client';
import * as fs from 'fs';

const configuration = petstore.createConfiguration();
const apiInstance = new petstore.DefaultApi(configuration);

let body:petstore.DefaultApiInteractionsGetRequest = {
  // string (optional)
  parameterTypeMapping: "parameter_type_mapping_example",
};

apiInstance.interactionsGet(body).then((data:any) => {
  console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **parameterTypeMapping** | [**string**] |  | (optional) defaults to undefined


### Return type

**Interaction**

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


