# Org.OpenAPITools.Api.StoreApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**DeleteOrder**](StoreApi.md#deleteorder) | **DELETE** /store/order/{order_id} | Delete purchase order by ID |
| [**GetInventory**](StoreApi.md#getinventory) | **GET** /store/inventory | Returns pet inventories by status |
| [**GetOrderById**](StoreApi.md#getorderbyid) | **GET** /store/order/{order_id} | Find purchase order by ID |
| [**PlaceOrder**](StoreApi.md#placeorder) | **POST** /store/order | Place an order for a pet |

<a id="deleteorder"></a>
# **DeleteOrder**
> void DeleteOrder (string orderId)

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **orderId** | **string** | ID of the order that needs to be deleted |  |

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
| **400** | Invalid ID supplied |  -  |
| **404** | Order not found |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="getinventory"></a>
# **GetInventory**
> Dictionary&lt;string, int&gt; GetInventory ()

Returns pet inventories by status

Returns a map of status codes to quantities


### Parameters
This endpoint does not need any parameter.
### Return type

**Dictionary<string, int>**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="getorderbyid"></a>
# **GetOrderById**
> Order GetOrderById (long orderId)

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **orderId** | **long** | ID of pet that needs to be fetched |  |

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid ID supplied |  -  |
| **404** | Order not found |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="placeorder"></a>
# **PlaceOrder**
> Order PlaceOrder (Order order)

Place an order for a pet


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **order** | [**Order**](Order.md) | order placed for purchasing the pet |  |

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid Order |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

