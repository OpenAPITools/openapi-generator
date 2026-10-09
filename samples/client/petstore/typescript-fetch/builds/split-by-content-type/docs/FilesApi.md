# FilesApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**convert**](FilesApi.md#convert) | **POST** /convert |  |
| [**upload**](FilesApi.md#upload) | **POST** /upload |  |



## convert

> Receipt convert(receipt)



### Example

```ts
import {
  Configuration,
  FilesApi,
} from '@openapitools/typescript-fetch-split-by-content-type';
import type { ConvertRequest } from '@openapitools/typescript-fetch-split-by-content-type';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-split-by-content-type SDK...");
  const api = new FilesApi();

  const body = {
    // Receipt (optional)
    receipt: ...,
  } satisfies ConvertRequest;

  try {
    const data = await api.convert(body);
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
| **receipt** | [Receipt](Receipt.md) |  | [Optional] |

### Return type

[**Receipt**](Receipt.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`, `multipart/form-data`
- **Accept**: `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | ok |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## upload

> Receipt upload(file, name)



### Example

```ts
import {
  Configuration,
  FilesApi,
} from '@openapitools/typescript-fetch-split-by-content-type';
import type { UploadRequest } from '@openapitools/typescript-fetch-split-by-content-type';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-split-by-content-type SDK...");
  const api = new FilesApi();

  const body = {
    // Blob (optional)
    file: BINARY_DATA_HERE,
    // string (optional)
    name: name_example,
  } satisfies UploadRequest;

  try {
    const data = await api.upload(body);
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
| **file** | `Blob` |  | [Optional] [Defaults to `undefined`] |
| **name** | `string` |  | [Optional] [Defaults to `undefined`] |

### Return type

[**Receipt**](Receipt.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `multipart/form-data`
- **Accept**: `application/json`, `application/pdf`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | ok |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

