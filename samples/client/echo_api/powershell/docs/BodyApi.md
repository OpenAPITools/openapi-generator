# PSOpenAPITools.PSOpenAPITools\Api.BodyApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Test-BinaryGif**](BodyApi.md#Test-BinaryGif) | **POST** /binary/gif | Test binary (gif) response body
[**Test-BodyApplicationOctetstreamBinary**](BodyApi.md#Test-BodyApplicationOctetstreamBinary) | **POST** /body/application/octetstream/binary | Test body parameter(s)
[**Test-BodyMultipartFormdataArrayOfBinary**](BodyApi.md#Test-BodyMultipartFormdataArrayOfBinary) | **POST** /body/application/octetstream/array_of_binary | Test array of binary in multipart mime
[**Test-BodyMultipartFormdataSingleBinary**](BodyApi.md#Test-BodyMultipartFormdataSingleBinary) | **POST** /body/application/octetstream/single_binary | Test single binary in multipart mime
[**Test-EchoBodyFreeFormObjectResponseString**](BodyApi.md#Test-EchoBodyFreeFormObjectResponseString) | **POST** /echo/body/FreeFormObject/response_string | Test free form object
[**Test-EchoBodyPet**](BodyApi.md#Test-EchoBodyPet) | **POST** /echo/body/Pet | Test body parameter(s)
[**Test-EchoBodyPetResponseString**](BodyApi.md#Test-EchoBodyPetResponseString) | **POST** /echo/body/Pet/response_string | Test empty response body
[**Test-EchoBodyTagResponseString**](BodyApi.md#Test-EchoBodyTagResponseString) | **POST** /echo/body/Tag/response_string | Test empty json (request body)
[**Test-EchoBodyAllOfPet**](BodyApi.md#Test-EchoBodyAllOfPet) | **POST** /echo/body/allOf/Pet | Test body parameter(s)
[**Test-EchoBodyStringEnum**](BodyApi.md#Test-EchoBodyStringEnum) | **POST** /echo/body/string_enum | Test string enum response body


<a id="Test-BinaryGif"></a>
# **Test-BinaryGif**
> System.IO.FileInfo Test-BinaryGif<br>

Test binary (gif) response body

Test binary (gif) response body

### Example
```powershell

# Test binary (gif) response body
try {
    $Result = Test-BinaryGif
} catch {
    Write-Host ("Exception occurred when calling Test-BinaryGif: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**System.IO.FileInfo**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: image/gif

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-BodyApplicationOctetstreamBinary"></a>
# **Test-BodyApplicationOctetstreamBinary**
> String Test-BodyApplicationOctetstreamBinary<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Body] <System.IO.FileInfo><br>

Test body parameter(s)

Test body parameter(s)

### Example
```powershell
$Body =  # System.IO.FileInfo |  (optional)

# Test body parameter(s)
try {
    $Result = Test-BodyApplicationOctetstreamBinary -Body $Body
} catch {
    Write-Host ("Exception occurred when calling Test-BodyApplicationOctetstreamBinary: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Body** | **System.IO.FileInfo****System.IO.FileInfo**|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/octet-stream
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-BodyMultipartFormdataArrayOfBinary"></a>
# **Test-BodyMultipartFormdataArrayOfBinary**
> String Test-BodyMultipartFormdataArrayOfBinary<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Files] <System.IO.FileInfo[]><br>

Test array of binary in multipart mime

Test array of binary in multipart mime

### Example
```powershell
$Files =  # System.IO.FileInfo[] | 

# Test array of binary in multipart mime
try {
    $Result = Test-BodyMultipartFormdataArrayOfBinary -Files $Files
} catch {
    Write-Host ("Exception occurred when calling Test-BodyMultipartFormdataArrayOfBinary: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Files** | **System.IO.FileInfo[]**|  | 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-BodyMultipartFormdataSingleBinary"></a>
# **Test-BodyMultipartFormdataSingleBinary**
> String Test-BodyMultipartFormdataSingleBinary<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-MyFile] <System.IO.FileInfo><br>

Test single binary in multipart mime

Test single binary in multipart mime

### Example
```powershell
$MyFile =  # System.IO.FileInfo |  (optional)

# Test single binary in multipart mime
try {
    $Result = Test-BodyMultipartFormdataSingleBinary -MyFile $MyFile
} catch {
    Write-Host ("Exception occurred when calling Test-BodyMultipartFormdataSingleBinary: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **MyFile** | **System.IO.FileInfo****System.IO.FileInfo**|  | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-EchoBodyFreeFormObjectResponseString"></a>
# **Test-EchoBodyFreeFormObjectResponseString**
> String Test-EchoBodyFreeFormObjectResponseString<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Body] <System.Nullable[SystemCollectionsHashtable]><br>

Test free form object

Test free form object

### Example
```powershell
$Body = @{ key_example = ... } # SystemCollectionsHashtable | Free form object (optional)

# Test free form object
try {
    $Result = Test-EchoBodyFreeFormObjectResponseString -Body $Body
} catch {
    Write-Host ("Exception occurred when calling Test-EchoBodyFreeFormObjectResponseString: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Body** | **SystemCollectionsHashtable**| Free form object | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-EchoBodyPet"></a>
# **Test-EchoBodyPet**
> Pet Test-EchoBodyPet<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Pet] <PSCustomObject><br>

Test body parameter(s)

Test body parameter(s)

### Example
```powershell
$Category = Initialize-Category -Id 1 -Name "Dogs"
$Tag = Initialize-Tag -Id 0 -Name "MyName"
$Pet = Initialize-Pet -Id 10 -Name "doggie" -Category $Category -PhotoUrls "MyPhotoUrls" -Tags $Tag -Status "available" # Pet | Pet object that needs to be added to the store (optional)

# Test body parameter(s)
try {
    $Result = Test-EchoBodyPet -Pet $Pet
} catch {
    Write-Host ("Exception occurred when calling Test-EchoBodyPet: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

[**Pet**](Pet.md) (PSCustomObject)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-EchoBodyPetResponseString"></a>
# **Test-EchoBodyPetResponseString**
> String Test-EchoBodyPetResponseString<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Pet] <PSCustomObject><br>

Test empty response body

Test empty response body

### Example
```powershell
$Category = Initialize-Category -Id 1 -Name "Dogs"
$Tag = Initialize-Tag -Id 0 -Name "MyName"
$Pet = Initialize-Pet -Id 10 -Name "doggie" -Category $Category -PhotoUrls "MyPhotoUrls" -Tags $Tag -Status "available" # Pet | Pet object that needs to be added to the store (optional)

# Test empty response body
try {
    $Result = Test-EchoBodyPetResponseString -Pet $Pet
} catch {
    Write-Host ("Exception occurred when calling Test-EchoBodyPetResponseString: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-EchoBodyTagResponseString"></a>
# **Test-EchoBodyTagResponseString**
> String Test-EchoBodyTagResponseString<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Tag] <PSCustomObject><br>

Test empty json (request body)

Test empty json (request body)

### Example
```powershell
$Tag = Initialize-Tag -Id 0 -Name "MyName" # Tag | Tag object (optional)

# Test empty json (request body)
try {
    $Result = Test-EchoBodyTagResponseString -Tag $Tag
} catch {
    Write-Host ("Exception occurred when calling Test-EchoBodyTagResponseString: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Tag** | [**Tag**](Tag.md)| Tag object | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-EchoBodyAllOfPet"></a>
# **Test-EchoBodyAllOfPet**
> Pet Test-EchoBodyAllOfPet<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Pet] <PSCustomObject><br>

Test body parameter(s)

Test body parameter(s)

### Example
```powershell
$Category = Initialize-Category -Id 1 -Name "Dogs"
$Tag = Initialize-Tag -Id 0 -Name "MyName"
$Pet = Initialize-Pet -Id 10 -Name "doggie" -Category $Category -PhotoUrls "MyPhotoUrls" -Tags $Tag -Status "available" # Pet | Pet object that needs to be added to the store (optional)

# Test body parameter(s)
try {
    $Result = Test-EchoBodyAllOfPet -Pet $Pet
} catch {
    Write-Host ("Exception occurred when calling Test-EchoBodyAllOfPet: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

[**Pet**](Pet.md) (PSCustomObject)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="Test-EchoBodyStringEnum"></a>
# **Test-EchoBodyStringEnum**
> StringEnumRef Test-EchoBodyStringEnum<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Body] <System.Nullable[String]><br>

Test string enum response body

Test string enum response body

### Example
```powershell
$Body = 0 # String | String enum (optional)

# Test string enum response body
try {
    $Result = Test-EchoBodyStringEnum -Body $Body
} catch {
    Write-Host ("Exception occurred when calling Test-EchoBodyStringEnum: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Body** | **String**| String enum | [optional] 

### Return type

[**StringEnumRef**](StringEnumRef.md) (PSCustomObject)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

