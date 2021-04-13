# PET_API

All URIs are relative to *http://petstore.swagger.io:80/v2*

Feature | HTTP request | Description
------------- | ------------- | -------------
[**add_pet**](PET_API.md#add_pet) | **Post** /pet | Add a new pet to the store
[**delete_pet**](PET_API.md#delete_pet) | **Delete** /pet/{petId} | Deletes a pet
[**find_pets_by_status**](PET_API.md#find_pets_by_status) | **Get** /pet/findByStatus | Finds Pets by status
[**find_pets_by_tags**](PET_API.md#find_pets_by_tags) | **Get** /pet/findByTags | Finds Pets by tags
[**pet_by_id**](PET_API.md#pet_by_id) | **Get** /pet/{petId} | Find pet by ID
[**update_pet**](PET_API.md#update_pet) | **Put** /pet | Update an existing pet
[**update_pet_with_form**](PET_API.md#update_pet_with_form) | **Post** /pet/{petId} | Updates a pet in the store with form data
[**upload_file**](PET_API.md#upload_file) | **Post** /pet/{petId}/uploadImage | uploads an image
[**upload_file_with_required_file**](PET_API.md#upload_file_with_required_file) | **Post** /fake/{petId}/uploadImageWithRequiredFile | uploads an image (required)


# **add_pet**
> add_pet (body: PET )
	

Add a new pet to the store


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**PET**](PET.md)| Pet object that needs to be added to the store | 

### Return type

{empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_pet**
> delete_pet (pet_id: INTEGER_64 ; api_key:  detachable STRING_32 )
	

Deletes a pet


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **INTEGER_64**| Pet id to delete | [default to null]
 **api_key** | **STRING_32**|  | [optional] [default to null]

### Return type

{empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **find_pets_by_status**
> find_pets_by_status (status: LIST [STRING_32] ): detachable LIST [PET]
	

Finds Pets by status

Multiple status values can be provided with comma separated strings


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**LIST [STRING_32]**](STRING_32.md)| Status values that need to be considered for filter | [default to null]

### Return type

[**LIST [PET]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **find_pets_by_tags**
> find_pets_by_tags (tags: LIST [STRING_32] ): detachable LIST [PET]
	

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**LIST [STRING_32]**](STRING_32.md)| Tags to filter by | [default to null]

### Return type

[**LIST [PET]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **pet_by_id**
> pet_by_id (pet_id: INTEGER_64 ): detachable PET
	

Find pet by ID

Returns a single pet


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **INTEGER_64**| ID of pet to return | [default to null]

### Return type

[**PET**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_pet**
> update_pet (body: PET )
	

Update an existing pet


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**PET**](PET.md)| Pet object that needs to be added to the store | 

### Return type

{empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_pet_with_form**
> update_pet_with_form (pet_id: INTEGER_64 ; name:  detachable STRING_32 ; status:  detachable STRING_32 )
	

Updates a pet in the store with form data


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **INTEGER_64**| ID of pet that needs to be updated | [default to null]
 **name** | **STRING_32**| Updated name of the pet | [optional] [default to null]
 **status** | **STRING_32**| Updated status of the pet | [optional] [default to null]

### Return type

{empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **upload_file**
> upload_file (pet_id: INTEGER_64 ; additional_metadata:  detachable STRING_32 ; file:  detachable FILE ): detachable API_RESPONSE
	

uploads an image


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **INTEGER_64**| ID of pet to update | [default to null]
 **additional_metadata** | **STRING_32**| Additional data to pass to server | [optional] [default to null]
 **file** | **FILE**| file to upload | [optional] [default to null]

### Return type

[**API_RESPONSE**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **upload_file_with_required_file**
> upload_file_with_required_file (pet_id: INTEGER_64 ; required_file: FILE ; additional_metadata:  detachable STRING_32 ): detachable API_RESPONSE
	

uploads an image (required)


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **INTEGER_64**| ID of pet to update | [default to null]
 **required_file** | **FILE**| file to upload | [default to null]
 **additional_metadata** | **STRING_32**| Additional data to pass to server | [optional] [default to null]

### Return type

[**API_RESPONSE**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

