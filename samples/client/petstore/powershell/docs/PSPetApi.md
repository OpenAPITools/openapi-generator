# PSPetstore.PSPetstore/Api.PSPetApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Add-PSPet**](PSPetApi.md#Add-PSPet) | **POST** /pet | Add a new pet to the store
[**Remove-Pet**](PSPetApi.md#remove-pet) | **DELETE** /pet/{petId} | Deletes a pet
[**Find-PSPetsByStatus**](PSPetApi.md#Find-PSPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
[**Find-PSPetsByTags**](PSPetApi.md#Find-PSPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
[**Get-PSPetById**](PSPetApi.md#Get-PSPetById) | **GET** /pet/{petId} | Find pet by ID
[**Update-PSPet**](PSPetApi.md#Update-PSPet) | **PUT** /pet | Update an existing pet
[**Update-PSPetWithForm**](PSPetApi.md#Update-PSPetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**Invoke-PSUploadFile**](PSPetApi.md#Invoke-PSUploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image
[**Invoke-PSUploadFileWithRequiredFile**](PSPetApi.md#Invoke-PSUploadFileWithRequiredFile) | **POST** /fake/{petId}/uploadImageWithRequiredFile | uploads an image (required)


<a name="Add-PSPet"></a>
# **Add-PSPet**
> void Add-PSPet<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Pet] <PSCustomObject><br>

Add a new pet to the store



### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure HTTP basic authorization: http_signature_test
$Configuration.Username = "YOUR_USERNAME"
$Configuration.Password = "YOUR_PASSWORD"
# Configure HttpSignature for authorization :http_signature_test
$httpSigningParams = @{
    KeyId = "xxxxxx1776876789ac747/xxxxxxx564612d31a62c01/xxxxxxxa1d7564612d31a66ee8"
    KeyFilePath = "C:\SecretKey.txt"
    HttpSigningHeader = @("(request-target)","Host","Date","Digest")
    HashAlgorithm = "sha256"
}
Set-ConfigurationHttpSigning $httpSigningParams

# Configure OAuth2 access token for authorization: petstore_auth
$Configuration.AccessToken = "YOUR_ACCESS_TOKEN"

$Category = Initialize-Category -Id 0 -Name "MyName"
$Tag = Initialize-Tag -Id 0 -Name "MyName"
$Pet = Initialize-Pet -Id 0 -Category $Category -Name "doggie" -PhotoUrls "MyPhotoUrls" -Tags $Tag -Status "available" # Pet | Pet object that needs to be added to the store

# Add a new pet to the store
try {
    $Result = Add-PSPet -Pet $Pet
} catch {
    Write-Host ("Exception occurred when calling Add-PSPet: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[http_signature_test](../README.md#http_signature_test), [petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="remove-pet"></a>
# **Remove-Pet**
> void Remove-Pet<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-PetId] <Int64><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-ApiKey] <String><br>

Deletes a pet



### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration.AccessToken = "YOUR_ACCESS_TOKEN"

$PetId = 789 # Int64 | Pet id to delete
$ApiKey = "MyApiKey" # String |  (optional)

# Deletes a pet
try {
    $Result = Remove-Pet -PetId $PetId -ApiKey $ApiKey
} catch {
    Write-Host ("Exception occurred when calling Remove-Pet: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **PetId** | **Int64**| Pet id to delete | 
 **ApiKey** | **String**|  | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Find-PSPetsByStatus"></a>
# **Find-PSPetsByStatus**
> Pet[] Find-PSPetsByStatus<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Status] <String[]><br>

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure HTTP basic authorization: http_signature_test
$Configuration.Username = "YOUR_USERNAME"
$Configuration.Password = "YOUR_PASSWORD"
# Configure HttpSignature for authorization :http_signature_test
$httpSigningParams = @{
    KeyId = "xxxxxx1776876789ac747/xxxxxxx564612d31a62c01/xxxxxxxa1d7564612d31a66ee8"
    KeyFilePath = "C:\SecretKey.txt"
    HttpSigningHeader = @("(request-target)","Host","Date","Digest")
    HashAlgorithm = "sha256"
}
Set-ConfigurationHttpSigning $httpSigningParams

# Configure OAuth2 access token for authorization: petstore_auth
$Configuration.AccessToken = "YOUR_ACCESS_TOKEN"

$Status = "available" # String[] | Status values that need to be considered for filter

# Finds Pets by status
try {
    $Result = Find-PSPetsByStatus -Status $Status
} catch {
    Write-Host ("Exception occurred when calling Find-PSPetsByStatus: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Status** | [**String[]**](String.md)| Status values that need to be considered for filter | 

### Return type

[**Pet[]**](Pet.md) (PSCustomObject)

### Authorization

[http_signature_test](../README.md#http_signature_test), [petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Find-PSPetsByTags"></a>
# **Find-PSPetsByTags**
> Pet[] Find-PSPetsByTags<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Tags] <String[]><br>

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure HTTP basic authorization: http_signature_test
$Configuration.Username = "YOUR_USERNAME"
$Configuration.Password = "YOUR_PASSWORD"
# Configure HttpSignature for authorization :http_signature_test
$httpSigningParams = @{
    KeyId = "xxxxxx1776876789ac747/xxxxxxx564612d31a62c01/xxxxxxxa1d7564612d31a66ee8"
    KeyFilePath = "C:\SecretKey.txt"
    HttpSigningHeader = @("(request-target)","Host","Date","Digest")
    HashAlgorithm = "sha256"
}
Set-ConfigurationHttpSigning $httpSigningParams

# Configure OAuth2 access token for authorization: petstore_auth
$Configuration.AccessToken = "YOUR_ACCESS_TOKEN"

$Tags = "MyTags" # String[] | Tags to filter by

# Finds Pets by tags
try {
    $Result = Find-PSPetsByTags -Tags $Tags
} catch {
    Write-Host ("Exception occurred when calling Find-PSPetsByTags: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Tags** | [**String[]**](String.md)| Tags to filter by | 

### Return type

[**Pet[]**](Pet.md) (PSCustomObject)

### Authorization

[http_signature_test](../README.md#http_signature_test), [petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Get-PSPetById"></a>
# **Get-PSPetById**
> Pet Get-PSPetById<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-PetId] <Int64><br>

Find pet by ID

Returns a single pet

### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure API key authorization: api_key
$Configuration.ApiKey.api_key = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration.ApiKeyPrefix.api_key = "Bearer"

$PetId = 789 # Int64 | ID of pet to return

# Find pet by ID
try {
    $Result = Get-PSPetById -PetId $PetId
} catch {
    Write-Host ("Exception occurred when calling Get-PSPetById: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **PetId** | **Int64**| ID of pet to return | 

### Return type

[**Pet**](Pet.md) (PSCustomObject)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Update-PSPet"></a>
# **Update-PSPet**
> void Update-PSPet<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Pet] <PSCustomObject><br>

Update an existing pet



### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure HTTP basic authorization: http_signature_test
$Configuration.Username = "YOUR_USERNAME"
$Configuration.Password = "YOUR_PASSWORD"
# Configure HttpSignature for authorization :http_signature_test
$httpSigningParams = @{
    KeyId = "xxxxxx1776876789ac747/xxxxxxx564612d31a62c01/xxxxxxxa1d7564612d31a66ee8"
    KeyFilePath = "C:\SecretKey.txt"
    HttpSigningHeader = @("(request-target)","Host","Date","Digest")
    HashAlgorithm = "sha256"
}
Set-ConfigurationHttpSigning $httpSigningParams

# Configure OAuth2 access token for authorization: petstore_auth
$Configuration.AccessToken = "YOUR_ACCESS_TOKEN"

$Category = Initialize-Category -Id 0 -Name "MyName"
$Tag = Initialize-Tag -Id 0 -Name "MyName"
$Pet = Initialize-Pet -Id 0 -Category $Category -Name "doggie" -PhotoUrls "MyPhotoUrls" -Tags $Tag -Status "available" # Pet | Pet object that needs to be added to the store

# Update an existing pet
try {
    $Result = Update-PSPet -Pet $Pet
} catch {
    Write-Host ("Exception occurred when calling Update-PSPet: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[http_signature_test](../README.md#http_signature_test), [petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Update-PSPetWithForm"></a>
# **Update-PSPetWithForm**
> void Update-PSPetWithForm<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-PetId] <Int64><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Name] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Status] <String><br>

Updates a pet in the store with form data



### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration.AccessToken = "YOUR_ACCESS_TOKEN"

$PetId = 789 # Int64 | ID of pet that needs to be updated
$Name = "MyName" # String | Updated name of the pet (optional)
$Status = "MyStatus" # String | Updated status of the pet (optional)

# Updates a pet in the store with form data
try {
    $Result = Update-PSPetWithForm -PetId $PetId -Name $Name -Status $Status
} catch {
    Write-Host ("Exception occurred when calling Update-PSPetWithForm: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **PetId** | **Int64**| ID of pet that needs to be updated | 
 **Name** | **String**| Updated name of the pet | [optional] 
 **Status** | **String**| Updated status of the pet | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Invoke-PSUploadFile"></a>
# **Invoke-PSUploadFile**
> ApiResponse Invoke-PSUploadFile<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-PetId] <Int64><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-AdditionalMetadata] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-File] <System.IO.FileInfo><br>

uploads an image



### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration.AccessToken = "YOUR_ACCESS_TOKEN"

$PetId = 789 # Int64 | ID of pet to update
$AdditionalMetadata = "MyAdditionalMetadata" # String | Additional data to pass to server (optional)
$File =  # System.IO.FileInfo | file to upload (optional)

# uploads an image
try {
    $Result = Invoke-PSUploadFile -PetId $PetId -AdditionalMetadata $AdditionalMetadata -File $File
} catch {
    Write-Host ("Exception occurred when calling Invoke-PSUploadFile: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **PetId** | **Int64**| ID of pet to update | 
 **AdditionalMetadata** | **String**| Additional data to pass to server | [optional] 
 **File** | **System.IO.FileInfo****System.IO.FileInfo**| file to upload | [optional] 

### Return type

[**ApiResponse**](ApiResponse.md) (PSCustomObject)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Invoke-PSUploadFileWithRequiredFile"></a>
# **Invoke-PSUploadFileWithRequiredFile**
> ApiResponse Invoke-PSUploadFileWithRequiredFile<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-PetId] <Int64><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-RequiredFile] <System.IO.FileInfo><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-AdditionalMetadata] <String><br>

uploads an image (required)



### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration.AccessToken = "YOUR_ACCESS_TOKEN"

$PetId = 789 # Int64 | ID of pet to update
$RequiredFile =  # System.IO.FileInfo | file to upload
$AdditionalMetadata = "MyAdditionalMetadata" # String | Additional data to pass to server (optional)

# uploads an image (required)
try {
    $Result = Invoke-PSUploadFileWithRequiredFile -PetId $PetId -RequiredFile $RequiredFile -AdditionalMetadata $AdditionalMetadata
} catch {
    Write-Host ("Exception occurred when calling Invoke-PSUploadFileWithRequiredFile: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **PetId** | **Int64**| ID of pet to update | 
 **RequiredFile** | **System.IO.FileInfo****System.IO.FileInfo**| file to upload | 
 **AdditionalMetadata** | **String**| Additional data to pass to server | [optional] 

### Return type

[**ApiResponse**](ApiResponse.md) (PSCustomObject)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

