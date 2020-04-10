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


<a name="Add-PSPet"></a>
# **Add-PSPet**
> Pet Add-PSPet<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Pet] <PSCustomObject><br>

Add a new pet to the store

### Example
```powershell
Import-Module -Name PSPetstore

$Configuration = Get-PSPetstoreConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$Pet = (Initialize-Pet-Id 123 -Category (Initialize-Category-Id 123 -Name "Name_example") -Name "Name_example" -PhotoUrls @("PhotoUrls_example") -Tags @((Initialize-Tag-Id 123 -Name "Name_example")) -Status "Status_example") # Pet | Pet object that needs to be added to the store

# Add a new pet to the store
try {
    Pet $Result = Add-PSPet -Pet $Pet
} catch {
    Write-Host ("Exception occured when calling Add-PSPet: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

[**Pet**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="remove-pet"></a>
# **Remove-Pet**
> void Remove-Pet<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-PetId] <Int64><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-ApiKey] <String><br>

Deletes a pet

### Example
```powershell
Import-Module -Name PSPetstore

$Configuration = Get-PSPetstoreConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$PetId = 987 # Int64 | Pet id to delete (default to null)
$ApiKey = "ApiKey_example" # String |  (optional) (default to null)

# Deletes a pet
try {
    Remove-Pet -PetId $PetId -ApiKey $ApiKey
} catch {
    Write-Host ("Exception occured when calling Remove-Pet: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **PetId** | **Int64**| Pet id to delete | [default to null]
 **ApiKey** | **String**|  | [optional] [default to null]

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
Import-Module -Name PSPetstore

$Configuration = Get-PSPetstoreConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$Status = @("Status_example") # String[] | Status values that need to be considered for filter (default to null)

# Finds Pets by status
try {
    Pet[] $Result = Find-PSPetsByStatus -Status $Status
} catch {
    Write-Host ("Exception occured when calling Find-PSPetsByStatus: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Status** | [**String[]**](String.md)| Status values that need to be considered for filter | [default to null]

### Return type

[**Pet[]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

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
Import-Module -Name PSPetstore

$Configuration = Get-PSPetstoreConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$Tags = @("Inner_example") # String[] | Tags to filter by (default to null)

# Finds Pets by tags
try {
    Pet[] $Result = Find-PSPetsByTags -Tags $Tags
} catch {
    Write-Host ("Exception occured when calling Find-PSPetsByTags: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Tags** | [**String[]**](String.md)| Tags to filter by | [default to null]

### Return type

[**Pet[]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

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
Import-Module -Name PSPetstore

$Configuration = Get-PSPetstoreConfiguration
# Configure API key authorization: api_key
$Configuration["ApiKey"]["api_key"] = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration["ApiKeyPrefix"]["api_key"] = "Bearer"

$PetId = 987 # Int64 | ID of pet to return (default to null)

# Find pet by ID
try {
    Pet $Result = Get-PSPetById -PetId $PetId
} catch {
    Write-Host ("Exception occured when calling Get-PSPetById: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **PetId** | **Int64**| ID of pet to return | [default to null]

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="Update-PSPet"></a>
# **Update-PSPet**
> Pet Update-PSPet<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Pet] <PSCustomObject><br>

Update an existing pet

### Example
```powershell
Import-Module -Name PSPetstore

$Configuration = Get-PSPetstoreConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$Pet = (Initialize-Pet-Id 123 -Category (Initialize-Category-Id 123 -Name "Name_example") -Name "Name_example" -PhotoUrls @("PhotoUrls_example") -Tags @((Initialize-Tag-Id 123 -Name "Name_example")) -Status "Status_example") # Pet | Pet object that needs to be added to the store

# Update an existing pet
try {
    Pet $Result = Update-PSPet -Pet $Pet
} catch {
    Write-Host ("Exception occured when calling Update-PSPet: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

[**Pet**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/xml, application/json

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
Import-Module -Name PSPetstore

$Configuration = Get-PSPetstoreConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$PetId = 987 # Int64 | ID of pet that needs to be updated (default to null)
$Name = "Name_example" # String | Updated name of the pet (optional) (default to null)
$Status = "Status_example" # String | Updated status of the pet (optional) (default to null)

# Updates a pet in the store with form data
try {
    Update-PSPetWithForm -PetId $PetId -Name $Name -Status $Status
} catch {
    Write-Host ("Exception occured when calling Update-PSPetWithForm: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **PetId** | **Int64**| ID of pet that needs to be updated | [default to null]
 **Name** | **String**| Updated name of the pet | [optional] [default to null]
 **Status** | **String**| Updated status of the pet | [optional] [default to null]

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
Import-Module -Name PSPetstore

$Configuration = Get-PSPetstoreConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$PetId = 987 # Int64 | ID of pet to update (default to null)
$AdditionalMetadata = "AdditionalMetadata_example" # String | Additional data to pass to server (optional) (default to null)
$File = 987 # System.IO.FileInfo | file to upload (optional) (default to null)

# uploads an image
try {
    ApiResponse $Result = Invoke-PSUploadFile -PetId $PetId -AdditionalMetadata $AdditionalMetadata -File $File
} catch {
    Write-Host ("Exception occured when calling Invoke-PSUploadFile: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **PetId** | **Int64**| ID of pet to update | [default to null]
 **AdditionalMetadata** | **String**| Additional data to pass to server | [optional] [default to null]
 **File** | **System.IO.FileInfo****System.IO.FileInfo**| file to upload | [optional] [default to null]

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

