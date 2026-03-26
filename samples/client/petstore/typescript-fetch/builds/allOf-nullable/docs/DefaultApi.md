# DefaultApi

All URIs are relative to *http://api.example.xyz/v1*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**list**](DefaultApi.md#list) | **GET** /person/display/{personId} |  |



## list

> Club list(personId)



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { ListRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  const body = {
    // string | The id of the person to retrieve
    personId: personId_example,
  } satisfies ListRequest;

  try {
    const data = await api.list(body);
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
| **personId** | `string` | The id of the person to retrieve | [Defaults to `undefined`] |

### Return type

[**Club**](Club.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

