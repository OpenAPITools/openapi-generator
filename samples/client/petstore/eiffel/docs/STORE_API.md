# STORE_API

All URIs are relative to *http://petstore.swagger.io/v2*

Feature | HTTP request | Description
------------- | ------------- | -------------
[**delete_order**](STORE_API.md#delete_order) | **Delete** /store/order/{orderId} | Delete purchase order by ID
[**inventory**](STORE_API.md#inventory) | **Get** /store/inventory | Returns pet inventories by status
[**order_by_id**](STORE_API.md#order_by_id) | **Get** /store/order/{orderId} | Find purchase order by ID
[**place_order**](STORE_API.md#place_order) | **Post** /store/order | Place an order for a pet


# **delete_order**
> delete_order (order_id: STRING_32 )
	

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order_id** | **STRING_32**| ID of the order that needs to be deleted | 

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **inventory**
> inventory : detachable STRING_TABLE[INTEGER_32]
	

Returns pet inventories by status

Returns a map of status codes to quantities


### Parameters
This endpoint does not need any parameter.

### Return type

**STRING_TABLE[INTEGER_32]**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **order_by_id**
> order_by_id (order_id: INTEGER_64 ): detachable ORDER
	

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order_id** | **INTEGER_64**| ID of pet that needs to be fetched | 

### Return type

[**ORDER**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **place_order**
> place_order (body: ORDER ): detachable ORDER
	

Place an order for a pet




### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**ORDER**](ORDER.md)| order placed for purchasing the pet | 

### Return type

[**ORDER**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

