# PSOpenAPITools.PSOpenAPITools\Api.FormApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Test-FormIntegerBooleanString**](FormApi.md#Test-FormIntegerBooleanString) | **POST** /form/integer/boolean/string | Test form parameter(s)
[**Test-FormObjectMultipart**](FormApi.md#Test-FormObjectMultipart) | **POST** /form/object/multipart | Test form parameter(s) for multipart schema
[**Test-FormOneof**](FormApi.md#Test-FormOneof) | **POST** /form/oneof | Test form parameter(s) for oneOf schema


<a id="Test-FormIntegerBooleanString"></a>
# **Test-FormIntegerBooleanString**
> String Test-FormIntegerBooleanString<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-IntegerForm] <System.Nullable[Int32]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-BooleanForm] <System.Nullable[Boolean]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-StringForm] <String><br>

Test form parameter(s)

Test form parameter(s)

### Example
```powershell
$IntegerForm = 56 # Int32 |  (optional)
$BooleanForm = $true # Boolean |  (optional)
$StringForm = "MyStringForm" # String |  (optional)

# Test form parameter(s)
try {
    $Result = Test-FormIntegerBooleanString -IntegerForm $IntegerForm -BooleanForm $BooleanForm -StringForm $StringForm
} catch {
    Write-Host ("Exception occurred when calling Test-FormIntegerBooleanString: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **IntegerForm** | **Int32**|  | [optional] 
 **BooleanForm** | **Boolean**|  | [optional] 
 **StringForm** | **String**|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-FormObjectMultipart"></a>
# **Test-FormObjectMultipart**
> String Test-FormObjectMultipart<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Marker] <PSCustomObject><br>

Test form parameter(s) for multipart schema

Test form parameter(s) for multipart schema

### Example
```powershell
$TestFormObjectMultipartRequestMarker = Initialize-TestFormObjectMultipartRequestMarker -Name "MyName" # TestFormObjectMultipartRequestMarker | 

# Test form parameter(s) for multipart schema
try {
    $Result = Test-FormObjectMultipart -Marker $Marker
} catch {
    Write-Host ("Exception occurred when calling Test-FormObjectMultipart: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Marker** | [**TestFormObjectMultipartRequestMarker**](TestFormObjectMultipartRequestMarker.md)|  | 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-FormOneof"></a>
# **Test-FormOneof**
> String Test-FormOneof<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Form1] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Form2] <System.Nullable[Int32]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Form3] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Form4] <System.Nullable[Boolean]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Id] <System.Nullable[Int64]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Name] <String><br>

Test form parameter(s) for oneOf schema

Test form parameter(s) for oneOf schema

### Example
```powershell
$Form1 = "MyForm1" # String |  (optional)
$Form2 = 56 # Int32 |  (optional)
$Form3 = "MyForm3" # String |  (optional)
$Form4 = $true # Boolean |  (optional)
$Id = 789 # Int64 |  (optional)
$Name = "MyName" # String |  (optional)

# Test form parameter(s) for oneOf schema
try {
    $Result = Test-FormOneof -Form1 $Form1 -Form2 $Form2 -Form3 $Form3 -Form4 $Form4 -Id $Id -Name $Name
} catch {
    Write-Host ("Exception occurred when calling Test-FormOneof: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Form1** | **String**|  | [optional] 
 **Form2** | **Int32**|  | [optional] 
 **Form3** | **String**|  | [optional] 
 **Form4** | **Boolean**|  | [optional] 
 **Id** | **Int64**|  | [optional] 
 **Name** | **String**|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

