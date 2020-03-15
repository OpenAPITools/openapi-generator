# PSOpenAPITools.PSOpenAPITools/API.PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Add-Pet**](PetApi.md#add-pet) | **POST** /pet | Add a new pet to the store
[**Remove-Pet**](PetApi.md#remove-pet) | **DELETE** /pet/{petId} | Deletes a pet
[**Find-PetsByStatus**](PetApi.md#find-petsbystatus) | **GET** /pet/findByStatus | Finds Pets by status
[**Find-PetsByTags**](PetApi.md#find-petsbytags) | **GET** /pet/findByTags | Finds Pets by tags
[**Get-PetById**](PetApi.md#get-petbyid) | **GET** /pet/{petId} | Find pet by ID
[**Update-Pet**](PetApi.md#update-pet) | **PUT** /pet | Update an existing pet
[**Update-PetWithForm**](PetApi.md#update-petwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**Invoke-UploadFile**](PetApi.md#invoke-uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image


<a name="add-pet"></a>
# **Add-Pet**
> Pet Add-Pet
>    [-Pet] <Pet>

Add a new pet to the store

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$Pet = (New-Pet -Id 123  -Category (New-Category -Id 123  -Name "Name_example")  -Name "Name_example"  -PhotoUrls @("PhotoUrls_example")  -Tags @((New-Tag -Id 123  -Name "Name_example"))  -Status "Status_example") # Pet | Pet object that needs to be added to the store

# Add a new pet to the store
Pet $Result = Add-Pet -Pet $Pet
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
> void Remove-Pet
>    [-PetId] <Int64>
>    [-ApiKey] <String>

Deletes a pet

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$PetId = 987 # Int64 | Pet id to delete (default to null)
$ApiKey = "ApiKey_example" # String |  (optional) (default to null)

# Deletes a pet
Remove-Pet -PetId $PetId -ApiKey $ApiKey
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

<a name="find-petsbystatus"></a>
# **Find-PetsByStatus**
> Pet[] Find-PetsByStatus
>    [-Status] <String[]>

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$Status = @("Status_example") # String[] | Status values that need to be considered for filter (default to null)

# Finds Pets by status
Pet[] $Result = Find-PetsByStatus -Status $Status
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

<a name="find-petsbytags"></a>
# **Find-PetsByTags**
> Pet[] Find-PetsByTags
>    [-Tags] <String[]>

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$Tags = @("Inner_example") # String[] | Tags to filter by (default to null)

# Finds Pets by tags
Pet[] $Result = Find-PetsByTags -Tags $Tags
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

<a name="get-petbyid"></a>
# **Get-PetById**
> Pet Get-PetById
>    [-PetId] <Int64>

Find pet by ID

Returns a single pet

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure API key authorization: api_key
$Configuration["ApiKey"]["api_key"] = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration["ApiKeyPrefix"]["api_key"] = "Bearer"

$PetId = 987 # Int64 | ID of pet to return (default to null)

# Find pet by ID
Pet $Result = Get-PetById -PetId $PetId
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

<a name="update-pet"></a>
# **Update-Pet**
> Pet Update-Pet
>    [-Pet] <Pet>

Update an existing pet

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$Pet = (New-Pet -Id 123  -Category (New-Category -Id 123  -Name "Name_example")  -Name "Name_example"  -PhotoUrls @("PhotoUrls_example")  -Tags @((New-Tag -Id 123  -Name "Name_example"))  -Status "Status_example") # Pet | Pet object that needs to be added to the store

# Update an existing pet
Pet $Result = Update-Pet -Pet $Pet
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

<a name="update-petwithform"></a>
# **Update-PetWithForm**
> void Update-PetWithForm
>    [-PetId] <Int64>
>    [-Name] <String>
>    [-Status] <String>

Updates a pet in the store with form data

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$PetId = 987 # Int64 | ID of pet that needs to be updated (default to null)
$Name = "Name_example" # String | Updated name of the pet (optional) (default to null)
$Status = "Status_example" # String | Updated status of the pet (optional) (default to null)

# Updates a pet in the store with form data
Update-PetWithForm -PetId $PetId -Name $Name -Status $Status
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

<a name="invoke-uploadfile"></a>
# **Invoke-UploadFile**
> ApiResponse Invoke-UploadFile
>    [-PetId] <Int64>
>    [-AdditionalMetadata] <String>
>    [-File] <System.IO.FileInfo>

uploads an image

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$PetId = 987 # Int64 | ID of pet to update (default to null)
$AdditionalMetadata = "AdditionalMetadata_example" # String | Additional data to pass to server (optional) (default to null)
$File = 987 # System.IO.FileInfo | file to upload (optional) (default to null)

# uploads an image
ApiResponse $Result = Invoke-UploadFile -PetId $PetId -AdditionalMetadata $AdditionalMetadata -File $File
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

