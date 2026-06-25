# DefaultApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**uploadFiles**](DefaultApi.md#uploadfiles) | **POST** /upload |  |



## uploadFiles

> uploadFiles(files, metadata)



### Example

```ts
import {
  Configuration,
  DefaultApi,
} from '';
import type { UploadFilesRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new DefaultApi();

  const body = {
    // Array<Blob>
    files: /path/to/file.txt,
    // string (optional)
    metadata: metadata_example,
  } satisfies UploadFilesRequest;

  try {
    const data = await api.uploadFiles(body);
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
| **files** | `Array<Blob>` |  | |
| **metadata** | `string` |  | [Optional] [Defaults to `undefined`] |

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
| **204** | Successful upload |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)
