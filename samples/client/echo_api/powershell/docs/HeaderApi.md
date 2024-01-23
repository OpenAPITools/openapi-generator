# PSOpenAPITools.PSOpenAPITools\Api.HeaderApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Test-HeaderIntegerBooleanStringEnums**](HeaderApi.md#Test-HeaderIntegerBooleanStringEnums) | **GET** /header/integer/boolean/string/enums | Test header parameter(s)


<a id="Test-HeaderIntegerBooleanStringEnums"></a>
# **Test-HeaderIntegerBooleanStringEnums**
> String Test-HeaderIntegerBooleanStringEnums<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-IntegerHeader] <System.Nullable[Int32]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-BooleanHeader] <System.Nullable[Boolean]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-StringHeader] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-EnumNonrefStringHeader] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-EnumRefStringHeader] <PSCustomObject><br>

Test header parameter(s)

Test header parameter(s)

### Example
```powershell
$IntegerHeader = 56 # Int32 |  (optional)
$BooleanHeader = $true # Boolean |  (optional)
$StringHeader = "MyStringHeader" # String |  (optional)
$EnumNonrefStringHeader = "success" # String |  (optional)
$EnumRefStringHeader = "success" # StringEnumRef |  (optional)

# Test header parameter(s)
try {
    $Result = Test-HeaderIntegerBooleanStringEnums -IntegerHeader $IntegerHeader -BooleanHeader $BooleanHeader -StringHeader $StringHeader -EnumNonrefStringHeader $EnumNonrefStringHeader -EnumRefStringHeader $EnumRefStringHeader
} catch {
    Write-Host ("Exception occurred when calling Test-HeaderIntegerBooleanStringEnums: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **IntegerHeader** | **Int32**|  | [optional] 
 **BooleanHeader** | **Boolean**|  | [optional] 
 **StringHeader** | **String**|  | [optional] 
 **EnumNonrefStringHeader** | **String**|  | [optional] 
 **EnumRefStringHeader** | [**StringEnumRef**](StringEnumRef.md)|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

