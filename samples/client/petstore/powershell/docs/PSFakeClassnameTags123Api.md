# PSPetstore.PSPetstore\Api.PSFakeClassnameTags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Test-PSClassname**](PSFakeClassnameTags123Api.md#Test-PSClassname) | **PATCH** /fake_classname_test | To test class name in snake case


<a id="Test-PSClassname"></a>
# **Test-PSClassname**
> Client Test-PSClassname<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Client] <PSCustomObject><br>

To test class name in snake case

To test class name in snake case

### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure API key authorization: api_key_query
$Configuration.ApiKey.api_key_query = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration.ApiKeyPrefix.api_key_query = "Bearer"

$Client = Initialize-Client -Client "MyClient" # Client | client model

# To test class name in snake case
try {
    $Result = Test-PSClassname -Client $Client
} catch {
    Write-Host ("Exception occurred when calling Test-PSClassname: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Client** | [**Client**](Client.md)| client model | 

### Return type

[**Client**](Client.md) (PSCustomObject)

### Authorization

[api_key_query](../README.md#api_key_query)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

