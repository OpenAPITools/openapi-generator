# .StoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**deleteOrder**](StoreApi.md#deleteOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID|
|[**getInventory**](StoreApi.md#getInventory) | **GET** /store/inventory | Returns pet inventories by status|
|[**getOrderById**](StoreApi.md#getOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID|
|[**placeOrder**](StoreApi.md#placeOrder) | **POST** /store/order | Place an order for a pet|

# **deleteOrder**
> deleteOrder()

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example

```typescript
import * as ApiModule from '@openapitools/typescript-fetch-petstore';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.StoreApi(configuration);

let body: ApiModule.StoreApiDeleteOrderRequest = {
  // string | ID of the order that needs to be deleted
  orderId: orderId_example,
};
apiInstance.deleteOrder(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **orderId** | [**string**] | ID of the order that needs to be deleted | defaults to undefined|


### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**400** | Invalid ID supplied |  -  |
|**404** | Order not found |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **getInventory**
> { [key: string]: number; } getInventory()

Returns a map of status codes to quantities

### Example

```typescript
import * as ApiModule from '@openapitools/typescript-fetch-petstore';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.StoreApi(configuration);

let body: any = {};
apiInstance.getInventory(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters
This endpoint does not need any parameter.


### Return type

**{ [key: string]: number; }**

### Authorization

[api_key](README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **getOrderById**
> Order getOrderById()

For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions

### Example

```typescript
import * as ApiModule from '@openapitools/typescript-fetch-petstore';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.StoreApi(configuration);

let body: ApiModule.StoreApiGetOrderByIdRequest = {
  // number | ID of pet that needs to be fetched
  orderId: 789,
};
apiInstance.getOrderById(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **orderId** | [**number**] | ID of pet that needs to be fetched | defaults to undefined|


### Return type

**Order**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | successful operation |  -  |
|**400** | Invalid ID supplied |  -  |
|**404** | Order not found |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **placeOrder**
> Order placeOrder(body)


### Example

```typescript
import * as ApiModule from '@openapitools/typescript-fetch-petstore';

const configuration = ApiModule.createConfiguration();
const apiInstance = new ApiModule.StoreApi(configuration);

let body: ApiModule.StoreApiPlaceOrderRequest = {
  // Order | order placed for purchasing the pet
  body: ,
};
apiInstance.placeOrder(body).then((data:any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error:any) => console.error(error));
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **body** | **Order**| order placed for purchasing the pet | |


### Return type

**Order**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**200** | successful operation |  -  |
|**400** | Invalid Order |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


