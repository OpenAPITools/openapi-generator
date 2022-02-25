# StoreAPI

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**StoreAPI_deleteOrder**](StoreAPI.md#StoreAPI_deleteOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**StoreAPI_getInventory**](StoreAPI.md#StoreAPI_getInventory) | **GET** /store/inventory | Returns pet inventories by status
[**StoreAPI_getOrderById**](StoreAPI.md#StoreAPI_getOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID
[**StoreAPI_placeOrder**](StoreAPI.md#StoreAPI_placeOrder) | **POST** /store/order | Place an order for a pet


# **StoreAPI_deleteOrder**
```c
// Delete purchase order by ID
//
// For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors
//
void StoreAPI_deleteOrder(apiClient_t *apiClient, char * orderId);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**orderId** | **char \*** | ID of the order that needs to be deleted | 

### Return type

void

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **StoreAPI_getInventory**
```c
// Returns pet inventories by status
//
// Returns a map of status codes to quantities
//
list_t* StoreAPI_getInventory(apiClient_t *apiClient);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |

### Return type



list_t*



### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **StoreAPI_getOrderById**
```c
// Find purchase order by ID
//
// For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions
//
order_t* StoreAPI_getOrderById(apiClient_t *apiClient, long orderId);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**orderId** | **long** | ID of pet that needs to be fetched | 

### Return type

[order_t](order.md) *


### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **StoreAPI_placeOrder**
```c
// Place an order for a pet
//
order_t* StoreAPI_placeOrder(apiClient_t *apiClient, order_t * body);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**body** | **[order_t](order.md) \*** | order placed for purchasing the pet | 

### Return type

[order_t](order.md) *


### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

