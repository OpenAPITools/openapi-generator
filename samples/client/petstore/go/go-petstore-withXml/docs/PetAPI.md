# \PetAPI

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**AddPet**](PetAPI.md#AddPet) | **Post** /pet | Add a new pet to the store
[**DeletePet**](PetAPI.md#DeletePet) | **Delete** /pet/{petId} | Deletes a pet
[**FindPetsByStatus**](PetAPI.md#FindPetsByStatus) | **Get** /pet/findByStatus | Finds Pets by status
[**FindPetsByTags**](PetAPI.md#FindPetsByTags) | **Get** /pet/findByTags | Finds Pets by tags
[**GetPetByID**](PetAPI.md#GetPetByID) | **Get** /pet/{petId} | Find pet by ID
[**UpdatePet**](PetAPI.md#UpdatePet) | **Put** /pet | Update an existing pet
[**UpdatePetWithForm**](PetAPI.md#UpdatePetWithForm) | **Post** /pet/{petId} | Updates a pet in the store with form data
[**UploadFile**](PetAPI.md#UploadFile) | **Post** /pet/{petId}/uploadImage | uploads an image
[**UploadFileWithRequiredFile**](PetAPI.md#UploadFileWithRequiredFile) | **Post** /fake/{petId}/uploadImageWithRequiredFile | uploads an image (required)



## AddPet

> AddPet(ctx, body)

Add a new pet to the store

### Required Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

 (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: application/json, application/xml
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## DeletePet

> DeletePet(ctx, petID, optional)

Deletes a pet

### Required Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**petID** | **int64**| Pet id to delete | 
 **optional** | ***DeletePetOpts** | optional parameters | nil if no parameters

### Optional Parameters

Optional parameters are passed through a pointer to a DeletePetOpts struct


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **aPIKey** | **optional.String**|  | 

### Return type

 (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## FindPetsByStatus

> []Pet FindPetsByStatus(ctx, status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Required Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**status** | [**[]string**](string.md)| Status values that need to be considered for filter | 

### Return type

[**[]Pet**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## FindPetsByTags

> []Pet FindPetsByTags(ctx, tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Required Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**tags** | [**[]string**](string.md)| Tags to filter by | 

### Return type

[**[]Pet**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## GetPetByID

> Pet GetPetByID(ctx, petID)

Find pet by ID

Returns a single pet

### Required Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**petID** | **int64**| ID of pet to return | 

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## UpdatePet

> UpdatePet(ctx, body)

Update an existing pet

### Required Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

 (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: application/json, application/xml
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## UpdatePetWithForm

> UpdatePetWithForm(ctx, petID, optional)

Updates a pet in the store with form data

### Required Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**petID** | **int64**| ID of pet that needs to be updated | 
 **optional** | ***UpdatePetWithFormOpts** | optional parameters | nil if no parameters

### Optional Parameters

Optional parameters are passed through a pointer to a UpdatePetWithFormOpts struct


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **name** | **optional.String**| Updated name of the pet | 
 **status** | **optional.String**| Updated status of the pet | 

### Return type

 (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## UploadFile

> APIResponse UploadFile(ctx, petID, optional)

uploads an image

### Required Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**petID** | **int64**| ID of pet to update | 
 **optional** | ***UploadFileOpts** | optional parameters | nil if no parameters

### Optional Parameters

Optional parameters are passed through a pointer to a UploadFileOpts struct


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **additionalMetadata** | **optional.String**| Additional data to pass to server | 
 **file** | **optional.Interface of *os.File****optional.*os.File**| file to upload | 

### Return type

[**APIResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## UploadFileWithRequiredFile

> APIResponse UploadFileWithRequiredFile(ctx, petID, requiredFile, optional)

uploads an image (required)

### Required Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**petID** | **int64**| ID of pet to update | 
**requiredFile** | ***os.File*****os.File**| file to upload | 
 **optional** | ***UploadFileWithRequiredFileOpts** | optional parameters | nil if no parameters

### Optional Parameters

Optional parameters are passed through a pointer to a UploadFileWithRequiredFileOpts struct


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------


 **additionalMetadata** | **optional.String**| Additional data to pass to server | 

### Return type

[**APIResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)

