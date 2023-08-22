# PSPetstore.PSPetstore/Api.PSDefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Invoke-PSFooGet**](PSDefaultApi.md#Invoke-PSFooGet) | **GET** /foo | 


<a id="Invoke-PSFooGet"></a>
# **Invoke-PSFooGet**
> FooGetDefaultResponse Invoke-PSFooGet<br>



### Example
```powershell

try {
    $Result = Invoke-PSFooGet
} catch {
    Write-Host ("Exception occurred when calling Invoke-PSFooGet: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**FooGetDefaultResponse**](FooGetDefaultResponse.md) (PSCustomObject)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

