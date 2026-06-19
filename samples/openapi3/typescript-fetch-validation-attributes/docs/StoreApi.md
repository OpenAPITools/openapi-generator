# StoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**deleteOrder**](StoreApi.md#deleteorder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID |
| [**getInventory**](StoreApi.md#getinventory) | **GET** /store/inventory | Returns pet inventories by status |
| [**getOrderById**](StoreApi.md#getorderbyid) | **GET** /store/order/{orderId} | Find purchase order by ID |
| [**placeOrder**](StoreApi.md#placeorder) | **POST** /store/order | Place an order for a pet |



## deleteOrder

> deleteOrder(orderId)

Delete purchase order by ID

For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors

### Example

```ts
import {
  Configuration,
  StoreApi,
} from '';
import type { DeleteOrderRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new StoreApi();

  const body = {
    // string | ID of the order that needs to be deleted
    orderId: orderId_example,
  } satisfies DeleteOrderRequest;

  try {
    const data = await api.deleteOrder(body);
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
| **orderId** | `string` | ID of the order that needs to be deleted | [Defaults to `undefined`] |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid ID supplied |  -  |
| **404** | Order not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## getInventory

> { [key: string]: number; } getInventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example

```ts
import {
  Configuration,
  StoreApi,
} from '';
import type { GetInventoryRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const config = new Configuration({ 
    // To configure API key authorization: api_key
    apiKey: "YOUR API KEY",
  });
  const api = new StoreApi(config);

  try {
    const data = await api.getInventory();
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters

This endpoint does not need any parameter.

### Return type

**{ [key: string]: number; }**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## getOrderById

> Order getOrderById(orderId)

Find purchase order by ID

For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions

### Example

```ts
import {
  Configuration,
  StoreApi,
} from '';
import type { GetOrderByIdRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new StoreApi();

  const body = {
    // number | ID of pet that needs to be fetched
    orderId: 789,
  } satisfies GetOrderByIdRequest;

  try {
    const data = await api.getOrderById(body);
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
| **orderId** | `number` | ID of pet that needs to be fetched | [Defaults to `undefined`] |

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `application/xml`, `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid ID supplied |  -  |
| **404** | Order not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## placeOrder

> Order placeOrder(order)

Place an order for a pet



### Example

```ts
import {
  Configuration,
  StoreApi,
} from '';
import type { PlaceOrderRequest } from '';

async function example() {
  console.log("🚀 Testing  SDK...");
  const api = new StoreApi();

  const body = {
    // Order | order placed for purchasing the pet
    order: ...,
  } satisfies PlaceOrderRequest;

  try {
    const data = await api.placeOrder(body);
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
| **order** | [Order](Order.md) | order placed for purchasing the pet | |

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/json`
- **Accept**: `application/xml`, `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid Order |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

