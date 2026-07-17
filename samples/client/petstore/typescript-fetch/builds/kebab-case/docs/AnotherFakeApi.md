# AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**_123testSpecialTags**](AnotherFakeApi.md#_123testspecialtags) | **PATCH** /another-fake/dummy | To test special tags |



## _123testSpecialTags

> Client _123testSpecialTags(client)

To test special tags

To test special tags and operation ID starting with number

### Example

```ts
import {
  Configuration,
  AnotherFakeApi,
} from '';
import type { 123testSpecialTagsRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new AnotherFakeApi();

  const body = {
    // Client | client model
    client: ...,
  } satisfies 123testSpecialTagsRequest;

  try {
    const data = await api._123testSpecialTags(body);
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

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

