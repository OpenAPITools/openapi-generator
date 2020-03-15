# PSOpenAPITools.PSOpenAPITools/API.StoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Invoke-DeleteOrder**](StoreApi.md#invoke-deleteorder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**Get-Inventory**](StoreApi.md#get-inventory) | **GET** /store/inventory | Returns pet inventories by status
[**Get-OrderById**](StoreApi.md#get-orderbyid) | **GET** /store/order/{orderId} | Find purchase order by ID
[**Invoke-PlaceOrder**](StoreApi.md#invoke-placeorder) | **POST** /store/order | Place an order for a pet


<a name="invoke-deleteorder"></a>
# **Invoke-DeleteOrder**
> void Invoke-DeleteOrder
>    [-OrderId] <String>

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example
```powershell
Import-Module -Name PSOpenAPITools

$OrderId = "OrderId_example" # String | ID of the order that needs to be deleted (default to null)

# Delete purchase order by ID
Invoke-DeleteOrder -OrderId $OrderId
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **OrderId** | **String**| ID of the order that needs to be deleted | [default to null]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="get-inventory"></a>
# **Get-Inventory**
> {String, Int32} Get-Inventory

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
{String, Int32} $Result = Get-Inventory
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

<a name="get-orderbyid"></a>
# **Get-OrderById**
> Order Get-OrderById
>    [-OrderId] <Int64>

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example
```powershell
Import-Module -Name PSOpenAPITools

$OrderId = 987 # Int64 | ID of pet that needs to be fetched (default to null)

# Find purchase order by ID
Order $Result = Get-OrderById -OrderId $OrderId
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **OrderId** | **Int64**| ID of pet that needs to be fetched | [default to null]

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="invoke-placeorder"></a>
# **Invoke-PlaceOrder**
> Order Invoke-PlaceOrder
>    [-Order] <Order>

Place an order for a pet

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Order = (New-Order -Id 123  -PetId 123  -Quantity 123  -ShipDate Get-Date  -Status "Status_example"  -Complete $false) # Order | order placed for purchasing the pet

# Place an order for a pet
Order $Result = Invoke-PlaceOrder -Order $Order
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Order** | [**Order**](Order.md)| order placed for purchasing the pet | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

