# FakeClassnameTags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**testClassname**](FakeClassnameTags123Api.md#testclassname) | **PATCH** /fake_classname_test | To test class name in snake case |



## testClassname

> Client testClassname(client)

To test class name in snake case

To test class name in snake case

### Example

```ts
import {
  Configuration,
  FakeClassnameTags123Api,
} from '';
import type { TestClassnameRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const config = new Configuration({ 
    // To configure API key authorization: api_key_query
    apiKey: "YOUR API KEY",
  });
  const api = new FakeClassnameTags123Api(config);

  const body = {
    // Client | client model
    client: ...,
  } satisfies TestClassnameRequest;

  try {
    const data = await api.testClassname(body);
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
| **client** | [Client](Client.md) | client model | |

### Return type

[**Client**](Client.md)

### Authorization

[api_key_query](../README.md#api_key_query)

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

