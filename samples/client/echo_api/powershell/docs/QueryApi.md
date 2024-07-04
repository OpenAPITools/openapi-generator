# PSOpenAPITools.PSOpenAPITools\Api.QueryApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Test-EnumRefString**](QueryApi.md#Test-EnumRefString) | **GET** /query/enum_ref_string | Test query parameter(s)
[**Test-QueryDatetimeDateString**](QueryApi.md#Test-QueryDatetimeDateString) | **GET** /query/datetime/date/string | Test query parameter(s)
[**Test-QueryIntegerBooleanString**](QueryApi.md#Test-QueryIntegerBooleanString) | **GET** /query/integer/boolean/string | Test query parameter(s)
[**Test-QueryStyleDeepObjectExplodeTrueObject**](QueryApi.md#Test-QueryStyleDeepObjectExplodeTrueObject) | **GET** /query/style_deepObject/explode_true/object | Test query parameter(s)
[**Test-QueryStyleDeepObjectExplodeTrueObjectAllOf**](QueryApi.md#Test-QueryStyleDeepObjectExplodeTrueObjectAllOf) | **GET** /query/style_deepObject/explode_true/object/allOf | Test query parameter(s)
[**Test-QueryStyleFormExplodeFalseArrayInteger**](QueryApi.md#Test-QueryStyleFormExplodeFalseArrayInteger) | **GET** /query/style_form/explode_false/array_integer | Test query parameter(s)
[**Test-QueryStyleFormExplodeFalseArrayString**](QueryApi.md#Test-QueryStyleFormExplodeFalseArrayString) | **GET** /query/style_form/explode_false/array_string | Test query parameter(s)
[**Test-QueryStyleFormExplodeTrueArrayString**](QueryApi.md#Test-QueryStyleFormExplodeTrueArrayString) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s)
[**Test-QueryStyleFormExplodeTrueObject**](QueryApi.md#Test-QueryStyleFormExplodeTrueObject) | **GET** /query/style_form/explode_true/object | Test query parameter(s)
[**Test-QueryStyleFormExplodeTrueObjectAllOf**](QueryApi.md#Test-QueryStyleFormExplodeTrueObjectAllOf) | **GET** /query/style_form/explode_true/object/allOf | Test query parameter(s)


<a id="Test-EnumRefString"></a>
# **Test-EnumRefString**
> String Test-EnumRefString<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-EnumNonrefStringQuery] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-EnumRefStringQuery] <PSCustomObject><br>

Test query parameter(s)

Test query parameter(s)

### Example
```powershell
$EnumNonrefStringQuery = "success" # String |  (optional)
$EnumRefStringQuery = "success" # StringEnumRef |  (optional)

# Test query parameter(s)
try {
    $Result = Test-EnumRefString -EnumNonrefStringQuery $EnumNonrefStringQuery -EnumRefStringQuery $EnumRefStringQuery
} catch {
    Write-Host ("Exception occurred when calling Test-EnumRefString: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **EnumNonrefStringQuery** | **String**|  | [optional] 
 **EnumRefStringQuery** | [**StringEnumRef**](StringEnumRef.md)|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-QueryDatetimeDateString"></a>
# **Test-QueryDatetimeDateString**
> String Test-QueryDatetimeDateString<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-DatetimeQuery] <System.Nullable[System.DateTime]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-DateQuery] <System.Nullable[System.DateTime]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-StringQuery] <String><br>

Test query parameter(s)

Test query parameter(s)

### Example
```powershell
$DatetimeQuery = (Get-Date) # System.DateTime |  (optional)
$DateQuery = (Get-Date) # System.DateTime |  (optional)
$StringQuery = "MyStringQuery" # String |  (optional)

# Test query parameter(s)
try {
    $Result = Test-QueryDatetimeDateString -DatetimeQuery $DatetimeQuery -DateQuery $DateQuery -StringQuery $StringQuery
} catch {
    Write-Host ("Exception occurred when calling Test-QueryDatetimeDateString: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **DatetimeQuery** | **System.DateTime**|  | [optional] 
 **DateQuery** | **System.DateTime**|  | [optional] 
 **StringQuery** | **String**|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-QueryIntegerBooleanString"></a>
# **Test-QueryIntegerBooleanString**
> String Test-QueryIntegerBooleanString<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-IntegerQuery] <System.Nullable[Int32]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-BooleanQuery] <System.Nullable[Boolean]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-StringQuery] <String><br>

Test query parameter(s)

Test query parameter(s)

### Example
```powershell
$IntegerQuery = 56 # Int32 |  (optional)
$BooleanQuery = $true # Boolean |  (optional)
$StringQuery = "MyStringQuery" # String |  (optional)

# Test query parameter(s)
try {
    $Result = Test-QueryIntegerBooleanString -IntegerQuery $IntegerQuery -BooleanQuery $BooleanQuery -StringQuery $StringQuery
} catch {
    Write-Host ("Exception occurred when calling Test-QueryIntegerBooleanString: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **IntegerQuery** | **Int32**|  | [optional] 
 **BooleanQuery** | **Boolean**|  | [optional] 
 **StringQuery** | **String**|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-QueryStyleDeepObjectExplodeTrueObject"></a>
# **Test-QueryStyleDeepObjectExplodeTrueObject**
> String Test-QueryStyleDeepObjectExplodeTrueObject<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-QueryObject] <PSCustomObject><br>

Test query parameter(s)

Test query parameter(s)

### Example
```powershell
$Category = Initialize-Category -Id 1 -Name "Dogs"
$Tag = Initialize-Tag -Id 0 -Name "MyName"
$Pet = Initialize-Pet -Id 10 -Name "doggie" -Category $Category -PhotoUrls "MyPhotoUrls" -Tags $Tag -Status "available" # Pet |  (optional)

# Test query parameter(s)
try {
    $Result = Test-QueryStyleDeepObjectExplodeTrueObject -QueryObject $QueryObject
} catch {
    Write-Host ("Exception occurred when calling Test-QueryStyleDeepObjectExplodeTrueObject: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **QueryObject** | [**Pet**](Pet.md)|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-QueryStyleDeepObjectExplodeTrueObjectAllOf"></a>
# **Test-QueryStyleDeepObjectExplodeTrueObjectAllOf**
> String Test-QueryStyleDeepObjectExplodeTrueObjectAllOf<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-QueryObject] <PSCustomObject><br>

Test query parameter(s)

Test query parameter(s)

### Example
```powershell
$TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter = Initialize-TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter -Size "MySize" -Color "MyColor" -Id 1 -Name "Dogs" # TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter |  (optional)

# Test query parameter(s)
try {
    $Result = Test-QueryStyleDeepObjectExplodeTrueObjectAllOf -QueryObject $QueryObject
} catch {
    Write-Host ("Exception occurred when calling Test-QueryStyleDeepObjectExplodeTrueObjectAllOf: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **QueryObject** | [**TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter**](TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter.md)|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-QueryStyleFormExplodeFalseArrayInteger"></a>
# **Test-QueryStyleFormExplodeFalseArrayInteger**
> String Test-QueryStyleFormExplodeFalseArrayInteger<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-QueryObject] <System.Nullable[Int32][]><br>

Test query parameter(s)

Test query parameter(s)

### Example
```powershell
$QueryObject = 0 # Int32[] |  (optional)

# Test query parameter(s)
try {
    $Result = Test-QueryStyleFormExplodeFalseArrayInteger -QueryObject $QueryObject
} catch {
    Write-Host ("Exception occurred when calling Test-QueryStyleFormExplodeFalseArrayInteger: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **QueryObject** | [**Int32[]**](Int32.md)|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-QueryStyleFormExplodeFalseArrayString"></a>
# **Test-QueryStyleFormExplodeFalseArrayString**
> String Test-QueryStyleFormExplodeFalseArrayString<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-QueryObject] <String[]><br>

Test query parameter(s)

Test query parameter(s)

### Example
```powershell
$QueryObject = "MyQueryObject" # String[] |  (optional)

# Test query parameter(s)
try {
    $Result = Test-QueryStyleFormExplodeFalseArrayString -QueryObject $QueryObject
} catch {
    Write-Host ("Exception occurred when calling Test-QueryStyleFormExplodeFalseArrayString: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **QueryObject** | [**String[]**](String.md)|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-QueryStyleFormExplodeTrueArrayString"></a>
# **Test-QueryStyleFormExplodeTrueArrayString**
> String Test-QueryStyleFormExplodeTrueArrayString<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-QueryObject] <PSCustomObject><br>

Test query parameter(s)

Test query parameter(s)

### Example
```powershell
$TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter = Initialize-TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter -Values "MyValues" # TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter |  (optional)

# Test query parameter(s)
try {
    $Result = Test-QueryStyleFormExplodeTrueArrayString -QueryObject $QueryObject
} catch {
    Write-Host ("Exception occurred when calling Test-QueryStyleFormExplodeTrueArrayString: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **QueryObject** | [**TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter**](TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter.md)|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-QueryStyleFormExplodeTrueObject"></a>
# **Test-QueryStyleFormExplodeTrueObject**
> String Test-QueryStyleFormExplodeTrueObject<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-QueryObject] <PSCustomObject><br>

Test query parameter(s)

Test query parameter(s)

### Example
```powershell
$Category = Initialize-Category -Id 1 -Name "Dogs"
$Tag = Initialize-Tag -Id 0 -Name "MyName"
$Pet = Initialize-Pet -Id 10 -Name "doggie" -Category $Category -PhotoUrls "MyPhotoUrls" -Tags $Tag -Status "available" # Pet |  (optional)

# Test query parameter(s)
try {
    $Result = Test-QueryStyleFormExplodeTrueObject -QueryObject $QueryObject
} catch {
    Write-Host ("Exception occurred when calling Test-QueryStyleFormExplodeTrueObject: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **QueryObject** | [**Pet**](Pet.md)|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-QueryStyleFormExplodeTrueObjectAllOf"></a>
# **Test-QueryStyleFormExplodeTrueObjectAllOf**
> String Test-QueryStyleFormExplodeTrueObjectAllOf<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-QueryObject] <PSCustomObject><br>

Test query parameter(s)

Test query parameter(s)

### Example
```powershell
"SUCCESS"$DataQuery = Initialize-DataQuery -Id 0 -Outcomes "SUCCESS" -Suffix "MySuffix" -Text "Some text" -Date (Get-Date) # DataQuery |  (optional)

# Test query parameter(s)
try {
    $Result = Test-QueryStyleFormExplodeTrueObjectAllOf -QueryObject $QueryObject
} catch {
    Write-Host ("Exception occurred when calling Test-QueryStyleFormExplodeTrueObjectAllOf: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **QueryObject** | [**DataQuery**](DataQuery.md)|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

