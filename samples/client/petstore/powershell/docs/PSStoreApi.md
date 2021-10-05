# PSPetstore.PSPetstore/Api.PSStoreApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Remove-PSOrder**](PSStoreApi.md#Remove-PSOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**Get-PSInventory**](PSStoreApi.md#Get-PSInventory) | **GET** /store/inventory | Returns pet inventories by status
[**Get-PSOrderById**](PSStoreApi.md#Get-PSOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID
[**Invoke-PSPlaceOrder**](PSStoreApi.md#Invoke-PSPlaceOrder) | **POST** /store/order | Place an order for a pet


<a name="Remove-PSOrder"></a>
# **Remove-PSOrder**
> void Remove-PSOrder<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-OrderId] <String><br>

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example
```powershell
$OrderId = "MyOrderId" # String | ID of the order that needs to be deleted

# Delete purchase order by ID
try {
    $Result = Remove-PSOrder -OrderId $OrderId
} catch {
    Write-Host ("Exception occurred when calling Remove-PSOrder: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **OrderId** | **String**| ID of the order that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Get-PSInventory"></a>
# **Get-PSInventory**
> System.Collections.Hashtable Get-PSInventory<br>

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure API key authorization: api_key
$Configuration.ApiKey.api_key = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration.ApiKeyPrefix.api_key = "Bearer"


# Returns pet inventories by status
try {
    $Result = Get-PSInventory
} catch {
    Write-Host ("Exception occurred when calling Get-PSInventory: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**System.Collections.Hashtable**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Get-PSOrderById"></a>
# **Get-PSOrderById**
> Order Get-PSOrderById<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-OrderId] <Int64><br>

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example
```powershell
$OrderId = 789 # Int64 | ID of pet that needs to be fetched

# Find purchase order by ID
try {
    $Result = Get-PSOrderById -OrderId $OrderId
} catch {
    Write-Host ("Exception occurred when calling Get-PSOrderById: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **OrderId** | **Int64**| ID of pet that needs to be fetched | 

### Return type

[**Order**](Order.md) (PSCustomObject)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Invoke-PSPlaceOrder"></a>
# **Invoke-PSPlaceOrder**
> Order Invoke-PSPlaceOrder<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Order] <PSCustomObject><br>

Place an order for a pet

### Example
```powershell
$Order = Initialize-Order -Id 0 -PetId 0 -Quantity 0 -ShipDate (Get-Date) -Status "placed" -Complete $false # Order | order placed for purchasing the pet

# Place an order for a pet
try {
    $Result = Invoke-PSPlaceOrder -Order $Order
} catch {
    Write-Host ("Exception occurred when calling Invoke-PSPlaceOrder: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Order** | [**Order**](Order.md)| order placed for purchasing the pet | 

### Return type

[**Order**](Order.md) (PSCustomObject)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

