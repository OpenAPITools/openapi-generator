# petstore.FakeClassnameTags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testClassname**](FakeClassnameTags123Api.md#testClassname) | **PATCH** /fake_classname_test | To test class name in snake case


# **testClassname**
> Client testClassname(client)

To test class name in snake case

### Example


```typescript
import { createConfiguration, FakeClassnameTags123Api } from 'ts-petstore-client';
import type { FakeClassnameTags123ApiTestClassnameRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new FakeClassnameTags123Api(configuration);

const request: FakeClassnameTags123ApiTestClassnameRequest = {
    // client model
  client: {
    client: "client_example",
  },
};

const data = await apiInstance.testClassname(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | **Client**| client model |


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
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


