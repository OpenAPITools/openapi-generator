# FileApi

All URIs are relative to *http://localhost:8080*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**createFile**](FileApi.md#createfile) | **POST** /api/v1/file |  |



## createFile

> createFile(documentBytes, documentType, properties, structured)



### Example

```ts
import {
  Configuration,
  FileApi,
} from '';
import type { CreateFileRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new FileApi();

  const body = {
    // Blob
    documentBytes: BINARY_DATA_HERE,
    // string
    documentType: documentType_example,
    // { [key: string]: string; }
    properties: ...,
    // StructuredType (optional)
    structured: ...,
  } satisfies CreateFileRequest;

  try {
    const data = await api.createFile(body);
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
| **documentBytes** | `Blob` |  | [Defaults to `undefined`] |
| **documentType** | `string` |  | [Defaults to `undefined`] |
| **properties** | `{ [key: string]: string; }` |  | |
| **structured** | [StructuredType](StructuredType.md) |  | [Optional] [Defaults to `undefined`] |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `multipart/form-data`
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **201** | File created successfully |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

