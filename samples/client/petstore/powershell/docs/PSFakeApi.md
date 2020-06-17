# PSPetstore.PSPetstore/Api.PSFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Invoke-PSFakeHealthGet**](PSFakeApi.md#Invoke-PSFakeHealthGet) | **GET** /fake/health | Health check endpoint
[**Invoke-PSFakeOuterBooleanSerialize**](PSFakeApi.md#Invoke-PSFakeOuterBooleanSerialize) | **POST** /fake/outer/boolean | 
[**Invoke-PSFakeOuterCompositeSerialize**](PSFakeApi.md#Invoke-PSFakeOuterCompositeSerialize) | **POST** /fake/outer/composite | 
[**Invoke-PSFakeOuterNumberSerialize**](PSFakeApi.md#Invoke-PSFakeOuterNumberSerialize) | **POST** /fake/outer/number | 
[**Invoke-PSFakeOuterStringSerialize**](PSFakeApi.md#Invoke-PSFakeOuterStringSerialize) | **POST** /fake/outer/string | 
[**Get-PSArrayOfEnums**](PSFakeApi.md#Get-PSArrayOfEnums) | **GET** /fake/array-of-enums | Array of Enums
[**Test-PSBodyWithFileSchema**](PSFakeApi.md#Test-PSBodyWithFileSchema) | **PUT** /fake/body-with-file-schema | 
[**Test-PSBodyWithQueryParams**](PSFakeApi.md#Test-PSBodyWithQueryParams) | **PUT** /fake/body-with-query-params | 
[**Test-PSClientModel**](PSFakeApi.md#Test-PSClientModel) | **PATCH** /fake | To test &quot;&quot;client&quot;&quot; model
[**Test-PSEndpointParameters**](PSFakeApi.md#Test-PSEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**Test-PSEnumParameters**](PSFakeApi.md#Test-PSEnumParameters) | **GET** /fake | To test enum parameters
[**Test-PSGroupParameters**](PSFakeApi.md#Test-PSGroupParameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**Test-PSInlineAdditionalProperties**](PSFakeApi.md#Test-PSInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**Test-PSJsonFormData**](PSFakeApi.md#Test-PSJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data
[**Test-PSQueryParameterCollectionFormat**](PSFakeApi.md#Test-PSQueryParameterCollectionFormat) | **PUT** /fake/test-query-paramters | 


<a name="Invoke-PSFakeHealthGet"></a>
# **Invoke-PSFakeHealthGet**
> HealthCheckResult Invoke-PSFakeHealthGet<br>

Health check endpoint

### Example
```powershell
Import-Module -Name PSPetstore


# Health check endpoint
try {
    HealthCheckResult $Result = Invoke-PSFakeHealthGet
} catch {
    Write-Host ("Exception occured when calling Invoke-PSFakeHealthGet: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**HealthCheckResult**](HealthCheckResult.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Invoke-PSFakeOuterBooleanSerialize"></a>
# **Invoke-PSFakeOuterBooleanSerialize**
> Boolean Invoke-PSFakeOuterBooleanSerialize<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Body] <System.Nullable[Boolean]><br>



Test serialization of outer boolean types

### Example
```powershell
Import-Module -Name PSPetstore

$Body = true # Boolean | Input boolean as post body (optional)

try {
    Boolean $Result = Invoke-PSFakeOuterBooleanSerialize -Body $Body
} catch {
    Write-Host ("Exception occured when calling Invoke-PSFakeOuterBooleanSerialize: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Body** | **Boolean**| Input boolean as post body | [optional] 

### Return type

**Boolean**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Invoke-PSFakeOuterCompositeSerialize"></a>
# **Invoke-PSFakeOuterCompositeSerialize**
> OuterComposite Invoke-PSFakeOuterCompositeSerialize<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-OuterComposite] <PSCustomObject><br>



Test serialization of object with outer number type

### Example
```powershell
Import-Module -Name PSPetstore

$OuterComposite = (Initialize-OuterComposite-MyNumber 123 -MyString "MyString_example" -MyBoolean $false) # OuterComposite | Input composite as post body (optional)

try {
    OuterComposite $Result = Invoke-PSFakeOuterCompositeSerialize -OuterComposite $OuterComposite
} catch {
    Write-Host ("Exception occured when calling Invoke-PSFakeOuterCompositeSerialize: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **OuterComposite** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional] 

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Invoke-PSFakeOuterNumberSerialize"></a>
# **Invoke-PSFakeOuterNumberSerialize**
> Decimal Invoke-PSFakeOuterNumberSerialize<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Body] <System.Nullable[Decimal]><br>



Test serialization of outer number types

### Example
```powershell
Import-Module -Name PSPetstore

$Body = 987 # Decimal | Input number as post body (optional)

try {
    Decimal $Result = Invoke-PSFakeOuterNumberSerialize -Body $Body
} catch {
    Write-Host ("Exception occured when calling Invoke-PSFakeOuterNumberSerialize: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Body** | **Decimal**| Input number as post body | [optional] 

### Return type

**Decimal**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Invoke-PSFakeOuterStringSerialize"></a>
# **Invoke-PSFakeOuterStringSerialize**
> String Invoke-PSFakeOuterStringSerialize<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Body] <String><br>



Test serialization of outer string types

### Example
```powershell
Import-Module -Name PSPetstore

$Body = "Body_example" # String | Input string as post body (optional)

try {
    String $Result = Invoke-PSFakeOuterStringSerialize -Body $Body
} catch {
    Write-Host ("Exception occured when calling Invoke-PSFakeOuterStringSerialize: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Body** | **String**| Input string as post body | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Get-PSArrayOfEnums"></a>
# **Get-PSArrayOfEnums**
> OuterEnum[] Get-PSArrayOfEnums<br>

Array of Enums

### Example
```powershell
Import-Module -Name PSPetstore


# Array of Enums
try {
    OuterEnum[] $Result = Get-PSArrayOfEnums
} catch {
    Write-Host ("Exception occured when calling Get-PSArrayOfEnums: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**OuterEnum[]**](OuterEnum.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Test-PSBodyWithFileSchema"></a>
# **Test-PSBodyWithFileSchema**
> void Test-PSBodyWithFileSchema<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-FileSchemaTestClass] <PSCustomObject><br>



For this test, the body for this request much reference a schema named `File`.

### Example
```powershell
Import-Module -Name PSPetstore

$FileSchemaTestClass = (Initialize-FileSchemaTestClass-File "TODO" -Files @("TODO")) # FileSchemaTestClass | 

try {
    Test-PSBodyWithFileSchema -FileSchemaTestClass $FileSchemaTestClass
} catch {
    Write-Host ("Exception occured when calling Test-PSBodyWithFileSchema: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **FileSchemaTestClass** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Test-PSBodyWithQueryParams"></a>
# **Test-PSBodyWithQueryParams**
> void Test-PSBodyWithQueryParams<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Query] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-User] <PSCustomObject><br>



### Example
```powershell
Import-Module -Name PSPetstore

$Query = "Query_example" # String |  (default to null)
$User = (Initialize-User-Id 123 -Username "Username_example" -FirstName "FirstName_example" -LastName "LastName_example" -Email "Email_example" -Password "Password_example" -Phone "Phone_example" -UserStatus 123 -ObjectWithNoDeclaredProps "TODO" -ObjectWithNoDeclaredPropsNullable "TODO" -AnyTypeProp "TODO" -AnyTypePropNullable "TODO") # User | 

try {
    Test-PSBodyWithQueryParams -Query $Query -User $User
} catch {
    Write-Host ("Exception occured when calling Test-PSBodyWithQueryParams: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Query** | **String**|  | [default to null]
 **User** | [**User**](User.md)|  | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Test-PSClientModel"></a>
# **Test-PSClientModel**
> Client Test-PSClientModel<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Client] <PSCustomObject><br>

To test ""client"" model

To test ""client"" model

### Example
```powershell
Import-Module -Name PSPetstore

$Client = (Initialize-Client-Client "Client_example") # Client | client model

# To test ""client"" model
try {
    Client $Result = Test-PSClientModel -Client $Client
} catch {
    Write-Host ("Exception occured when calling Test-PSClientModel: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Client** | [**Client**](Client.md)| client model | 

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Test-PSEndpointParameters"></a>
# **Test-PSEndpointParameters**
> void Test-PSEndpointParameters<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Number] <Decimal><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Double] <Double><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-PatternWithoutDelimiter] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Byte] <SystemByte><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Integer] <System.Nullable[Int32]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Int32] <System.Nullable[Int32]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Int64] <System.Nullable[Int64]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Float] <System.Nullable[Double]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-String] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Binary] <System.IO.FileInfo><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Date] <System.Nullable[System.DateTime]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-DateTime] <System.Nullable[System.DateTime]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Password] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Callback] <String><br>

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example
```powershell
Import-Module -Name PSPetstore

$Configuration = Get-PSPetstoreConfiguration
# Configure HTTP basic authorization: http_basic_test
$Configuration["Username"] = "YOUR_USERNAME";
$Configuration["Password"] = "YOUR_PASSWORD";

$Number = 987 # Decimal | None (default to null)
$Double = 987 # Double | None (default to null)
$PatternWithoutDelimiter = "PatternWithoutDelimiter_example" # String | None (default to null)
$Byte = TODO # SystemByte | None (default to null)
$Integer = 987 # Int32 | None (optional) (default to null)
$Int32 = 987 # Int32 | None (optional) (default to null)
$Int64 = 987 # Int64 | None (optional) (default to null)
$Float = 987 # Double | None (optional) (default to null)
$String = "String_example" # String | None (optional) (default to null)
$Binary = 987 # System.IO.FileInfo | None (optional) (default to null)
$Date = Get-Date # System.DateTime | None (optional) (default to null)
$DateTime = Get-Date # System.DateTime | None (optional) (default to 2010-02-01T10:20:10.111110+01:00)
$Password = "Password_example" # String | None (optional) (default to null)
$Callback = "Callback_example" # String | None (optional) (default to null)

# Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
try {
    Test-PSEndpointParameters -Number $Number -Double $Double -PatternWithoutDelimiter $PatternWithoutDelimiter -Byte $Byte -Integer $Integer -Int32 $Int32 -Int64 $Int64 -Float $Float -String $String -Binary $Binary -Date $Date -DateTime $DateTime -Password $Password -Callback $Callback
} catch {
    Write-Host ("Exception occured when calling Test-PSEndpointParameters: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Number** | **Decimal**| None | [default to null]
 **Double** | **Double**| None | [default to null]
 **PatternWithoutDelimiter** | **String**| None | [default to null]
 **Byte** | **SystemByte**| None | [default to null]
 **Integer** | **Int32**| None | [optional] [default to null]
 **Int32** | **Int32**| None | [optional] [default to null]
 **Int64** | **Int64**| None | [optional] [default to null]
 **Float** | **Double**| None | [optional] [default to null]
 **String** | **String**| None | [optional] [default to null]
 **Binary** | **System.IO.FileInfo****System.IO.FileInfo**| None | [optional] [default to null]
 **Date** | **System.DateTime**| None | [optional] [default to null]
 **DateTime** | **System.DateTime**| None | [optional] [default to 2010-02-01T10:20:10.111110+01:00]
 **Password** | **String**| None | [optional] [default to null]
 **Callback** | **String**| None | [optional] [default to null]

### Return type

void (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Test-PSEnumParameters"></a>
# **Test-PSEnumParameters**
> void Test-PSEnumParameters<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-EnumHeaderStringArray] <String[]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-EnumHeaderString] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-EnumQueryStringArray] <String[]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-EnumQueryString] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-EnumQueryInteger] <System.Nullable[Int32]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-EnumQueryDouble] <System.Nullable[Double]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-EnumFormStringArray] <String[]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-EnumFormString] <String><br>

To test enum parameters

To test enum parameters

### Example
```powershell
Import-Module -Name PSPetstore

$EnumHeaderStringArray = @("EnumHeaderStringArray_example") # String[] | Header parameter enum test (string array) (optional) (default to null)
$EnumHeaderString = "EnumHeaderString_example" # String | Header parameter enum test (string) (optional) (default to -efg)
$EnumQueryStringArray = @("EnumQueryStringArray_example") # String[] | Query parameter enum test (string array) (optional) (default to null)
$EnumQueryString = "EnumQueryString_example" # String | Query parameter enum test (string) (optional) (default to -efg)
$EnumQueryInteger = 987 # Int32 | Query parameter enum test (double) (optional) (default to null)
$EnumQueryDouble = 987 # Double | Query parameter enum test (double) (optional) (default to null)
$EnumFormStringArray = @("Inner_example") # String[] | Form parameter enum test (string array) (optional) (default to $)
$EnumFormString = "EnumFormString_example" # String | Form parameter enum test (string) (optional) (default to -efg)

# To test enum parameters
try {
    Test-PSEnumParameters -EnumHeaderStringArray $EnumHeaderStringArray -EnumHeaderString $EnumHeaderString -EnumQueryStringArray $EnumQueryStringArray -EnumQueryString $EnumQueryString -EnumQueryInteger $EnumQueryInteger -EnumQueryDouble $EnumQueryDouble -EnumFormStringArray $EnumFormStringArray -EnumFormString $EnumFormString
} catch {
    Write-Host ("Exception occured when calling Test-PSEnumParameters: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **EnumHeaderStringArray** | [**String[]**](String.md)| Header parameter enum test (string array) | [optional] [default to null]
 **EnumHeaderString** | **String**| Header parameter enum test (string) | [optional] [default to -efg]
 **EnumQueryStringArray** | [**String[]**](String.md)| Query parameter enum test (string array) | [optional] [default to null]
 **EnumQueryString** | **String**| Query parameter enum test (string) | [optional] [default to -efg]
 **EnumQueryInteger** | **Int32**| Query parameter enum test (double) | [optional] [default to null]
 **EnumQueryDouble** | **Double**| Query parameter enum test (double) | [optional] [default to null]
 **EnumFormStringArray** | [**String[]**](String.md)| Form parameter enum test (string array) | [optional] [default to $]
 **EnumFormString** | **String**| Form parameter enum test (string) | [optional] [default to -efg]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Test-PSGroupParameters"></a>
# **Test-PSGroupParameters**
> void Test-PSGroupParameters<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-RequiredStringGroup] <Int32><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-RequiredBooleanGroup] <Boolean><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-RequiredInt64Group] <Int64><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-StringGroup] <System.Nullable[Int32]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-BooleanGroup] <System.Nullable[Boolean]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Int64Group] <System.Nullable[Int64]><br>

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example
```powershell
Import-Module -Name PSPetstore

$Configuration = Get-PSPetstoreConfiguration
# Configure HTTP basic authorization: bearer_test
$Configuration["Username"] = "YOUR_USERNAME";
$Configuration["Password"] = "YOUR_PASSWORD";

$RequiredStringGroup = 987 # Int32 | Required String in group parameters (default to null)
$RequiredBooleanGroup = true # Boolean | Required Boolean in group parameters (default to null)
$RequiredInt64Group = 987 # Int64 | Required Integer in group parameters (default to null)
$StringGroup = 987 # Int32 | String in group parameters (optional) (default to null)
$BooleanGroup = true # Boolean | Boolean in group parameters (optional) (default to null)
$Int64Group = 987 # Int64 | Integer in group parameters (optional) (default to null)

# Fake endpoint to test group parameters (optional)
try {
    Test-PSGroupParameters -RequiredStringGroup $RequiredStringGroup -RequiredBooleanGroup $RequiredBooleanGroup -RequiredInt64Group $RequiredInt64Group -StringGroup $StringGroup -BooleanGroup $BooleanGroup -Int64Group $Int64Group
} catch {
    Write-Host ("Exception occured when calling Test-PSGroupParameters: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **RequiredStringGroup** | **Int32**| Required String in group parameters | [default to null]
 **RequiredBooleanGroup** | **Boolean**| Required Boolean in group parameters | [default to null]
 **RequiredInt64Group** | **Int64**| Required Integer in group parameters | [default to null]
 **StringGroup** | **Int32**| String in group parameters | [optional] [default to null]
 **BooleanGroup** | **Boolean**| Boolean in group parameters | [optional] [default to null]
 **Int64Group** | **Int64**| Integer in group parameters | [optional] [default to null]

### Return type

void (empty response body)

### Authorization

[bearer_test](../README.md#bearer_test)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Test-PSInlineAdditionalProperties"></a>
# **Test-PSInlineAdditionalProperties**
> void Test-PSInlineAdditionalProperties<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-RequestBody] <System.Collections.Hashtable><br>

test inline additionalProperties

### Example
```powershell
Import-Module -Name PSPetstore

$RequestBody = @{ "Key" = "Value" } # System.Collections.Hashtable | request body

# test inline additionalProperties
try {
    Test-PSInlineAdditionalProperties -RequestBody $RequestBody
} catch {
    Write-Host ("Exception occured when calling Test-PSInlineAdditionalProperties: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **RequestBody** | [**System.Collections.Hashtable**](String.md)| request body | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Test-PSJsonFormData"></a>
# **Test-PSJsonFormData**
> void Test-PSJsonFormData<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-VarParam] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Param2] <String><br>

test json serialization of form data

### Example
```powershell
Import-Module -Name PSPetstore

$VarParam = "VarParam_example" # String | field1 (default to null)
$Param2 = "Param2_example" # String | field2 (default to null)

# test json serialization of form data
try {
    Test-PSJsonFormData -VarParam $VarParam -Param2 $Param2
} catch {
    Write-Host ("Exception occured when calling Test-PSJsonFormData: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **VarParam** | **String**| field1 | [default to null]
 **Param2** | **String**| field2 | [default to null]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Test-PSQueryParameterCollectionFormat"></a>
# **Test-PSQueryParameterCollectionFormat**
> void Test-PSQueryParameterCollectionFormat<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Pipe] <String[]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Ioutil] <String[]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Http] <String[]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Url] <String[]><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Context] <String[]><br>



To test the collection format in query parameters

### Example
```powershell
Import-Module -Name PSPetstore

$Pipe = @("Inner_example") # String[] |  (default to null)
$Ioutil = @("Inner_example") # String[] |  (default to null)
$Http = @("Inner_example") # String[] |  (default to null)
$Url = @("Inner_example") # String[] |  (default to null)
$Context = @("Inner_example") # String[] |  (default to null)

try {
    Test-PSQueryParameterCollectionFormat -Pipe $Pipe -Ioutil $Ioutil -Http $Http -Url $Url -Context $Context
} catch {
    Write-Host ("Exception occured when calling Test-PSQueryParameterCollectionFormat: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Pipe** | [**String[]**](String.md)|  | [default to null]
 **Ioutil** | [**String[]**](String.md)|  | [default to null]
 **Http** | [**String[]**](String.md)|  | [default to null]
 **Url** | [**String[]**](String.md)|  | [default to null]
 **Context** | [**String[]**](String.md)|  | [default to null]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

