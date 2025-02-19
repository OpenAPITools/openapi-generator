# PSOpenAPITools.PSOpenAPITools\Api.AuthApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Test-AuthHttpBasic**](AuthApi.md#Test-AuthHttpBasic) | **POST** /auth/http/basic | To test HTTP basic authentication
[**Test-AuthHttpBearer**](AuthApi.md#Test-AuthHttpBearer) | **POST** /auth/http/bearer | To test HTTP bearer authentication


<a id="Test-AuthHttpBasic"></a>
# **Test-AuthHttpBasic**
> String Test-AuthHttpBasic<br>

To test HTTP basic authentication

To test HTTP basic authentication

### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure HTTP basic authorization: http_auth
$Configuration.Username = "YOUR_USERNAME"
$Configuration.Password = "YOUR_PASSWORD"


# To test HTTP basic authentication
try {
    $Result = Test-AuthHttpBasic
} catch {
    Write-Host ("Exception occurred when calling Test-AuthHttpBasic: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**String**

### Authorization

[http_auth](../README.md#http_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-AuthHttpBearer"></a>
# **Test-AuthHttpBearer**
> String Test-AuthHttpBearer<br>

To test HTTP bearer authentication

To test HTTP bearer authentication

### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration


# To test HTTP bearer authentication
try {
    $Result = Test-AuthHttpBearer
} catch {
    Write-Host ("Exception occurred when calling Test-AuthHttpBearer: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**String**

### Authorization

[http_bearer_auth](../README.md#http_bearer_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

