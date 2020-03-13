# PSOpenAPITools.PSOpenAPITools/API.StoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**DeleteOrder**](StoreApi.md#deleteorder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**GetInventory**](StoreApi.md#getinventory) | **GET** /store/inventory | Returns pet inventories by status
[**GetOrderById**](StoreApi.md#getorderbyid) | **GET** /store/order/{orderId} | Find purchase order by ID
[**PlaceOrder**](StoreApi.md#placeorder) | **POST** /store/order | Place an order for a pet


<a id="deleteorder"></a>
# **Invoker-PSOpenAPIToolsDeleteOrder**
> void Invoker-PSOpenAPIToolsDeleteOrder
    -orderId <String>

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example
```powershell
Import-Module -Name PSOpenAPITools

$orderId = "orderId_example" # String | ID of the order that needs to be deleted (default to null)

# Delete purchase order by ID
Invoker-PSOpenAPIToolsDeleteOrder -orderId $orderId
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **String**| ID of the order that needs to be deleted | [default to null]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="getinventory"></a>
# **Invoker-PSOpenAPIToolsGetInventory**
> {String, Int32} Invoker-PSOpenAPIToolsGetInventory

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure API key authorization: api_key
$Configuration["ApiKey"]["api_key"] = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration["ApiKeyPrefix"]["api_key"] = "Bearer"


# Returns pet inventories by status
{String, Int32} $Result = Invoker-PSOpenAPIToolsGetInventory
```

### Parameters
This endpoint does not need any parameter.

### Return type

**{String, Int32}**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="getorderbyid"></a>
# **Invoker-PSOpenAPIToolsGetOrderById**
> Order Invoker-PSOpenAPIToolsGetOrderById
    -orderId <Int64>

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example
```powershell
Import-Module -Name PSOpenAPITools

$orderId = 987 # Int64 | ID of pet that needs to be fetched (default to null)

# Find purchase order by ID
Order $Result = Invoker-PSOpenAPIToolsGetOrderById -orderId $orderId
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **Int64**| ID of pet that needs to be fetched | [default to null]

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="placeorder"></a>
# **Invoker-PSOpenAPIToolsPlaceOrder**
> Order Invoker-PSOpenAPIToolsPlaceOrder
    -body <Order>

Place an order for a pet

### Example
```powershell
Import-Module -Name PSOpenAPITools

$body = (New-Order -Id 123  -PetId 123  -Quantity 123  -ShipDate Get-Date  -Status "Status_example"  -Complete $false) # Order | order placed for purchasing the pet

# Place an order for a pet
Order $Result = Invoker-PSOpenAPIToolsPlaceOrder -body $body
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Order**](Order.md)| order placed for purchasing the pet | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

