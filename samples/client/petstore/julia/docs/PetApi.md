# PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**add_pet**](PetApi.md#add_pet) | **POST** /pet | Add a new pet to the store
[**delete_pet**](PetApi.md#delete_pet) | **DELETE** /pet/{petId} | Deletes a pet
[**find_pets_by_status**](PetApi.md#find_pets_by_status) | **GET** /pet/findByStatus | Finds Pets by status
[**find_pets_by_tags**](PetApi.md#find_pets_by_tags) | **GET** /pet/findByTags | Finds Pets by tags
[**get_pet_by_id**](PetApi.md#get_pet_by_id) | **GET** /pet/{petId} | Find pet by ID
[**update_pet**](PetApi.md#update_pet) | **PUT** /pet | Update an existing pet
[**update_pet_with_form**](PetApi.md#update_pet_with_form) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**upload_file**](PetApi.md#upload_file) | **POST** /pet/{petId}/uploadImage | uploads an image


# **add_pet**
> add_pet(_api::PetApi, pet::Pet; _mediaType=nothing) -> Pet, OpenAPI.Clients.ApiResponse <br/>
> add_pet(_api::PetApi, response_stream::Channel, pet::Pet; _mediaType=nothing) -> Channel{ Pet }, OpenAPI.Clients.ApiResponse

Add a new pet to the store



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **PetApi** | API context | 
**pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

[**Pet**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **delete_pet**
> delete_pet(_api::PetApi, pet_id::Int64; api_key=nothing, _mediaType=nothing) -> Nothing, OpenAPI.Clients.ApiResponse <br/>
> delete_pet(_api::PetApi, response_stream::Channel, pet_id::Int64; api_key=nothing, _mediaType=nothing) -> Channel{ Nothing }, OpenAPI.Clients.ApiResponse

Deletes a pet



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **PetApi** | API context | 
**pet_id** | **Int64**| Pet id to delete | [default to nothing]

### Optional Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **api_key** | **String**|  | [default to nothing]

### Return type

Nothing

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **find_pets_by_status**
> find_pets_by_status(_api::PetApi, status::Vector{String}; _mediaType=nothing) -> Vector{Pet}, OpenAPI.Clients.ApiResponse <br/>
> find_pets_by_status(_api::PetApi, response_stream::Channel, status::Vector{String}; _mediaType=nothing) -> Channel{ Vector{Pet} }, OpenAPI.Clients.ApiResponse

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **PetApi** | API context | 
**status** | [**Vector{String}**](String.md)| Status values that need to be considered for filter | [default to nothing]

### Return type

[**Vector{Pet}**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **find_pets_by_tags**
> find_pets_by_tags(_api::PetApi, tags::Vector{String}; _mediaType=nothing) -> Vector{Pet}, OpenAPI.Clients.ApiResponse <br/>
> find_pets_by_tags(_api::PetApi, response_stream::Channel, tags::Vector{String}; _mediaType=nothing) -> Channel{ Vector{Pet} }, OpenAPI.Clients.ApiResponse

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **PetApi** | API context | 
**tags** | [**Vector{String}**](String.md)| Tags to filter by | [default to nothing]

### Return type

[**Vector{Pet}**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **get_pet_by_id**
> get_pet_by_id(_api::PetApi, pet_id::Int64; _mediaType=nothing) -> Pet, OpenAPI.Clients.ApiResponse <br/>
> get_pet_by_id(_api::PetApi, response_stream::Channel, pet_id::Int64; _mediaType=nothing) -> Channel{ Pet }, OpenAPI.Clients.ApiResponse

Find pet by ID

Returns a single pet

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **PetApi** | API context | 
**pet_id** | **Int64**| ID of pet to return | [default to nothing]

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **update_pet**
> update_pet(_api::PetApi, pet::Pet; _mediaType=nothing) -> Pet, OpenAPI.Clients.ApiResponse <br/>
> update_pet(_api::PetApi, response_stream::Channel, pet::Pet; _mediaType=nothing) -> Channel{ Pet }, OpenAPI.Clients.ApiResponse

Update an existing pet



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **PetApi** | API context | 
**pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

[**Pet**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **update_pet_with_form**
> update_pet_with_form(_api::PetApi, pet_id::Int64; name=nothing, status=nothing, _mediaType=nothing) -> Nothing, OpenAPI.Clients.ApiResponse <br/>
> update_pet_with_form(_api::PetApi, response_stream::Channel, pet_id::Int64; name=nothing, status=nothing, _mediaType=nothing) -> Channel{ Nothing }, OpenAPI.Clients.ApiResponse

Updates a pet in the store with form data



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **PetApi** | API context | 
**pet_id** | **Int64**| ID of pet that needs to be updated | [default to nothing]

### Optional Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **name** | **String**| Updated name of the pet | [default to nothing]
 **status** | **String**| Updated status of the pet | [default to nothing]

### Return type

Nothing

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **upload_file**
> upload_file(_api::PetApi, pet_id::Int64; additional_metadata=nothing, file=nothing, _mediaType=nothing) -> ApiResponse, OpenAPI.Clients.ApiResponse <br/>
> upload_file(_api::PetApi, response_stream::Channel, pet_id::Int64; additional_metadata=nothing, file=nothing, _mediaType=nothing) -> Channel{ ApiResponse }, OpenAPI.Clients.ApiResponse

uploads an image



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **PetApi** | API context | 
**pet_id** | **Int64**| ID of pet to update | [default to nothing]

### Optional Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **additional_metadata** | **String**| Additional data to pass to server | [default to nothing]
 **file** | **String****String**| file to upload | [default to nothing]

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

