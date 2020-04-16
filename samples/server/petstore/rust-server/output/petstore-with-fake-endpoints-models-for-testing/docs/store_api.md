# store_api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
**DeleteOrder**](store_api.md#DeleteOrder) | **DELETE** /store/order/{order_id} | Delete purchase order by ID
**GetInventory**](store_api.md#GetInventory) | **GET** /store/inventory | Returns pet inventories by status
**GetOrderById**](store_api.md#GetOrderById) | **GET** /store/order/{order_id} | Find purchase order by ID
**PlaceOrder**](store_api.md#PlaceOrder) | **POST** /store/order | Place an order for a pet


# **DeleteOrder**
> DeleteOrder(order_id)
Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **order_id** | **String**| ID of the order that needs to be deleted | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **GetInventory**
> std::collections::HashMap<String, i32> GetInventory(ctx, )
Returns pet inventories by status

Returns a map of status codes to quantities

### Required Parameters
This endpoint does not need any parameter.

### Return type

[**std::collections::HashMap<String, i32>**](integer.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **GetOrderById**
> models::Order GetOrderById(order_id)
Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **order_id** | **i64**| ID of pet that needs to be fetched | 

### Return type

[**models::Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **PlaceOrder**
> models::Order PlaceOrder(body)
Place an order for a pet

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **body** | [**Order**](Order.md)| order placed for purchasing the pet | 

### Return type

[**models::Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/jsonapplication/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

