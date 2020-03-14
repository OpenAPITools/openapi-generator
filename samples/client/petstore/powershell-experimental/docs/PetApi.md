# PSOpenAPITools.PSOpenAPITools/API.PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**AddPet**](PetApi.md#addpet) | **POST** /pet | Add a new pet to the store
[**DeletePet**](PetApi.md#deletepet) | **DELETE** /pet/{petId} | Deletes a pet
[**FindPetsByStatus**](PetApi.md#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status
[**FindPetsByTags**](PetApi.md#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags
[**GetPetById**](PetApi.md#getpetbyid) | **GET** /pet/{petId} | Find pet by ID
[**UpdatePet**](PetApi.md#updatepet) | **PUT** /pet | Update an existing pet
[**UpdatePetWithForm**](PetApi.md#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**UploadFile**](PetApi.md#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image


<a id="addpet"></a>
# **Invoker-PSOpenAPIToolsAddPet**
> void Invoker-PSOpenAPIToolsAddPet
    -body <Pet>

Add a new pet to the store

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$body = (New-Pet -Id 123  -Category (New-Category -Id 123  -Name "Name_example")  -Name "Name_example"  -PhotoUrls @("PhotoUrls_example")  -Tags @((New-Tag -Id 123  -Name "Name_example"))  -Status "Status_example") # Pet | Pet object that needs to be added to the store

# Add a new pet to the store
Invoker-PSOpenAPIToolsAddPet -body $body
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="deletepet"></a>
# **Invoker-PSOpenAPIToolsDeletePet**
> void Invoker-PSOpenAPIToolsDeletePet
    -petId <Int64>
    -apiKey <String>

Deletes a pet

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$petId = 987 # Int64 | Pet id to delete (default to null)
$apiKey = "apiKey_example" # String |  (optional) (default to null)

# Deletes a pet
Invoker-PSOpenAPIToolsDeletePet -petId $petId -apiKey $apiKey
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Int64**| Pet id to delete | [default to null]
 **apiKey** | **String**|  | [optional] [default to null]

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="findpetsbystatus"></a>
# **Invoker-PSOpenAPIToolsFindPetsByStatus**
> Pet[] Invoker-PSOpenAPIToolsFindPetsByStatus
    -status <String[]>

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$status = @("Status_example") # String[] | Status values that need to be considered for filter (default to null)

# Finds Pets by status
Pet[] $Result = Invoker-PSOpenAPIToolsFindPetsByStatus -status $status
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**String[]**](String.md)| Status values that need to be considered for filter | [default to null]

### Return type

[**Pet[]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="findpetsbytags"></a>
# **Invoker-PSOpenAPIToolsFindPetsByTags**
> Pet[] Invoker-PSOpenAPIToolsFindPetsByTags
    -tags <String[]>

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$tags = @("Inner_example") # String[] | Tags to filter by (default to null)

# Finds Pets by tags
Pet[] $Result = Invoker-PSOpenAPIToolsFindPetsByTags -tags $tags
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**String[]**](String.md)| Tags to filter by | [default to null]

### Return type

[**Pet[]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="getpetbyid"></a>
# **Invoker-PSOpenAPIToolsGetPetById**
> Pet Invoker-PSOpenAPIToolsGetPetById
    -petId <Int64>
    -testHeader <String>
    -testQuery <String>

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

$petId = 987 # Int64 | ID of pet to return (default to null)
$testHeader = "testHeader_example" # String | ID of pet to return (optional) (default to null)
$testQuery = "testQuery_example" # String | ID of pet to return (optional) (default to null)

# Find pet by ID
Pet $Result = Invoker-PSOpenAPIToolsGetPetById -petId $petId -testHeader $testHeader -testQuery $testQuery
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Int64**| ID of pet to return | [default to null]
 **testHeader** | **String**| ID of pet to return | [optional] [default to null]
 **testQuery** | **String**| ID of pet to return | [optional] [default to null]

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="updatepet"></a>
# **Invoker-PSOpenAPIToolsUpdatePet**
> void Invoker-PSOpenAPIToolsUpdatePet
    -body <Pet>

Update an existing pet

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$body = (New-Pet -Id 123  -Category (New-Category -Id 123  -Name "Name_example")  -Name "Name_example"  -PhotoUrls @("PhotoUrls_example")  -Tags @((New-Tag -Id 123  -Name "Name_example"))  -Status "Status_example") # Pet | Pet object that needs to be added to the store

# Update an existing pet
Invoker-PSOpenAPIToolsUpdatePet -body $body
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="updatepetwithform"></a>
# **Invoker-PSOpenAPIToolsUpdatePetWithForm**
> void Invoker-PSOpenAPIToolsUpdatePetWithForm
    -petId <Int64>
    -name <String>
    -status <String>

Updates a pet in the store with form data

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$petId = 987 # Int64 | ID of pet that needs to be updated (default to null)
$name = "name_example" # String | Updated name of the pet (optional) (default to null)
$status = "status_example" # String | Updated status of the pet (optional) (default to null)

# Updates a pet in the store with form data
Invoker-PSOpenAPIToolsUpdatePetWithForm -petId $petId -name $name -status $status
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Int64**| ID of pet that needs to be updated | [default to null]
 **name** | **String**| Updated name of the pet | [optional] [default to null]
 **status** | **String**| Updated status of the pet | [optional] [default to null]

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="uploadfile"></a>
# **Invoker-PSOpenAPIToolsUploadFile**
> ApiResponse Invoker-PSOpenAPIToolsUploadFile
    -petId <Int64>
    -additionalMetadata <String>
    -file <System.IO.FileInfo>

uploads an image

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure OAuth2 access token for authorization: petstore_auth
$Configuration["AccessToken"] = "YOUR_ACCESS_TOKEN";

$petId = 987 # Int64 | ID of pet to update (default to null)
$additionalMetadata = "additionalMetadata_example" # String | Additional data to pass to server (optional) (default to null)
$file = 987 # System.IO.FileInfo | file to upload (optional) (default to null)

# uploads an image
ApiResponse $Result = Invoker-PSOpenAPIToolsUploadFile -petId $petId -additionalMetadata $additionalMetadata -file $file
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Int64**| ID of pet to update | [default to null]
 **additionalMetadata** | **String**| Additional data to pass to server | [optional] [default to null]
 **file** | **System.IO.FileInfo****System.IO.FileInfo**| file to upload | [optional] [default to null]

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

