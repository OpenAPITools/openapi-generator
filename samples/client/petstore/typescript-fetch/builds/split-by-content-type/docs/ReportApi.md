# ReportApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**createReport**](ReportApi.md#createreport) | **POST** /reports |  |
| [**getReport**](ReportApi.md#getreport) | **GET** /reports/{id} |  |



## createReport

> Receipt createReport(report)



### Example

```ts
import {
  Configuration,
  ReportApi,
} from '@openapitools/typescript-fetch-split-by-content-type';
import type { CreateReportRequest } from '@openapitools/typescript-fetch-split-by-content-type';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-split-by-content-type SDK...");
  const api = new ReportApi();

  const body = {
    // Report (optional)
    report: ...,
  } satisfies CreateReportRequest;

  try {
    const data = await api.createReport(body);
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
| **report** | [Report](Report.md) |  | [Optional] |

### Return type

[**Receipt**](Receipt.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`, `application/merge-patch+json`
- **Accept**: `application/json`, `application/pdf`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | ok |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## getReport

> Report getReport(id)



### Example

```ts
import {
  Configuration,
  ReportApi,
} from '@openapitools/typescript-fetch-split-by-content-type';
import type { GetReportRequest } from '@openapitools/typescript-fetch-split-by-content-type';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-split-by-content-type SDK...");
  const api = new ReportApi();

  const body = {
    // string
    id: id_example,
  } satisfies GetReportRequest;

  try {
    const data = await api.getReport(body);
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
| **id** | `string` |  | [Defaults to `undefined`] |

### Return type

[**Report**](Report.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `application/json`, `application/directlog`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | ok |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

