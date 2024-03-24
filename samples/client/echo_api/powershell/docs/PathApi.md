# PSOpenAPITools.PSOpenAPITools\Api.PathApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Test-sPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**](PathApi.md#Test-sPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath) | **GET** /path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path} | Test path parameter(s)


<a id="Test-sPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath"></a>
# **Test-sPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**
> String Test-sPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-PathString] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-PathInteger] <Int32><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-EnumNonrefStringPath] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-EnumRefStringPath] <PSCustomObject><br>

Test path parameter(s)

Test path parameter(s)

### Example
```powershell
$PathString = "MyPathString" # String | 
$PathInteger = 56 # Int32 | 
$EnumNonrefStringPath = "success" # String | 
$EnumRefStringPath = "success" # StringEnumRef | 

# Test path parameter(s)
try {
    $Result = Test-sPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath -PathString $PathString -PathInteger $PathInteger -EnumNonrefStringPath $EnumNonrefStringPath -EnumRefStringPath $EnumRefStringPath
} catch {
    Write-Host ("Exception occurred when calling Test-sPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **PathString** | **String**|  | 
 **PathInteger** | **Int32**|  | 
 **EnumNonrefStringPath** | **String**|  | 
 **EnumRefStringPath** | [**StringEnumRef**](StringEnumRef.md)|  | 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

