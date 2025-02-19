# StoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**delete_order**](StoreApi.md#delete_order) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**get_inventory**](StoreApi.md#get_inventory) | **GET** /store/inventory | Returns pet inventories by status
[**get_order_by_id**](StoreApi.md#get_order_by_id) | **GET** /store/order/{orderId} | Find purchase order by ID
[**place_order**](StoreApi.md#place_order) | **POST** /store/order | Place an order for a pet


# **delete_order**
> delete_order(_api::StoreApi, order_id::String; _mediaType=nothing) -> Nothing, OpenAPI.Clients.ApiResponse <br/>
> delete_order(_api::StoreApi, response_stream::Channel, order_id::String; _mediaType=nothing) -> Channel{ Nothing }, OpenAPI.Clients.ApiResponse

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **StoreApi** | API context | 
**order_id** | **String**| ID of the order that needs to be deleted | [default to nothing]

### Return type

Nothing

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **get_inventory**
> get_inventory(_api::StoreApi; _mediaType=nothing) -> Dict{String, Int64}, OpenAPI.Clients.ApiResponse <br/>
> get_inventory(_api::StoreApi, response_stream::Channel; _mediaType=nothing) -> Channel{ Dict{String, Int64} }, OpenAPI.Clients.ApiResponse

Returns pet inventories by status

Returns a map of status codes to quantities

### Required Parameters
This endpoint does not need any parameter.

### Return type

**Dict{String, Int64}**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **get_order_by_id**
> get_order_by_id(_api::StoreApi, order_id::Int64; _mediaType=nothing) -> Order, OpenAPI.Clients.ApiResponse <br/>
> get_order_by_id(_api::StoreApi, response_stream::Channel, order_id::Int64; _mediaType=nothing) -> Channel{ Order }, OpenAPI.Clients.ApiResponse

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **StoreApi** | API context | 
**order_id** | **Int64**| ID of pet that needs to be fetched | [default to nothing]

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **place_order**
> place_order(_api::StoreApi, order::Order; _mediaType=nothing) -> Order, OpenAPI.Clients.ApiResponse <br/>
> place_order(_api::StoreApi, response_stream::Channel, order::Order; _mediaType=nothing) -> Channel{ Order }, OpenAPI.Clients.ApiResponse

Place an order for a pet



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **StoreApi** | API context | 
**order** | [**Order**](Order.md)| order placed for purchasing the pet | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

