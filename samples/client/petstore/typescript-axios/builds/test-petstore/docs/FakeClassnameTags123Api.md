# .FakeClassnameTags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**testClassname**](FakeClassnameTags123Api.md#testClassname) | **PATCH** /fake_classname_test | To test class name in snake case|

# **testClassname**
> Client testClassname(client)

To test class name in snake case

### Example

```typescript
import * as ApiModule from '';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.FakeClassnameTags123Api = new ApiModule.FakeClassnameTags123Api(configuration);

let client: ApiModule.Client = ; // Client | client model

apiInstance.testClassname(client).then((data: any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error: any) => {
  console.error(error);
});
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **client** | **Client**| client model | |


### Return type

**Client**

### Authorization

[api_key_query](README.md#api_key_query)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


