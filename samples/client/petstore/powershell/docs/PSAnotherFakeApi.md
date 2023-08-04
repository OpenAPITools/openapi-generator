# PSPetstore.PSPetstore/Api.PSAnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Invoke-PS123TestSpecialTags**](PSAnotherFakeApi.md#Invoke-PS123TestSpecialTags) | **PATCH** /another-fake/dummy | To test special tags


<a id="Invoke-PS123TestSpecialTags"></a>
# **Invoke-PS123TestSpecialTags**
> Client Invoke-PS123TestSpecialTags<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Client] <PSCustomObject><br>

To test special tags

To test special tags and operation ID starting with number

### Example
```powershell
$Client = Initialize-Client -Client "MyClient" # Client | client model

# To test special tags
try {
    $Result = Invoke-PS123TestSpecialTags -Client $Client
} catch {
    Write-Host ("Exception occurred when calling Invoke-PS123TestSpecialTags: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
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

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

