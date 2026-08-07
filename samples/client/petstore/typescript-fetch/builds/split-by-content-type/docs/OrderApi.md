# OrderApi

All URIs are relative to *http://localhost*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**getOrders**](OrderApi.md#getorders) | **GET** /orders |  |



## getOrders

> Order getOrders(orderBy)



### Example

```ts
import {
  Configuration,
  OrderApi,
} from '@openapitools/typescript-fetch-split-by-content-type';
import type { GetOrdersRequest } from '@openapitools/typescript-fetch-split-by-content-type';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-split-by-content-type SDK...");
  const api = new OrderApi();

  const body = {
    // 'date' | 'amount' (optional)
    orderBy: orderBy_example,
  } satisfies GetOrdersRequest;

  try {
    const data = await api.getOrders(body);
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
| **orderBy** | `date`, `amount` |  | [Optional] [Defaults to `undefined`] [Enum: date, amount] |

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `application/json`, `application/pdf`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | ok |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

