# Org.OpenAPITools.Api.PetApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**AddPet**](PetApi.md#addpet) | **POST** /pet | Add a new pet to the store |
| [**DeletePet**](PetApi.md#deletepet) | **DELETE** /pet/{petId} | Deletes a pet |
| [**FindPetsByStatus**](PetApi.md#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status |
| [**FindPetsByTags**](PetApi.md#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags |
| [**GetPetById**](PetApi.md#getpetbyid) | **GET** /pet/{petId} | Find pet by ID |
| [**UpdatePet**](PetApi.md#updatepet) | **PUT** /pet | Update an existing pet |
| [**UpdatePetWithForm**](PetApi.md#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data |
| [**UploadFile**](PetApi.md#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image |
| [**UploadFileWithRequiredFile**](PetApi.md#uploadfilewithrequiredfile) | **POST** /fake/{petId}/uploadImageWithRequiredFile | uploads an image (required) |

<a id="addpet"></a>
# **AddPet**
> void AddPet (Pet pet)

Add a new pet to the store


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **pet** | [**Pet**](Pet.md) | Pet object that needs to be added to the store |  |

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth), [http_signature_test](../README.md#http_signature_test)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **405** | Invalid input |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="deletepet"></a>
# **DeletePet**
> void DeletePet (long petId, string apiKey = null)

Deletes a pet


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **petId** | **long** | Pet id to delete |  |
| **apiKey** | **string** |  | [optional]  |

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid pet value |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="findpetsbystatus"></a>
# **FindPetsByStatus**
> List&lt;Pet&gt; FindPetsByStatus (List<FindPetsByStatusStatusParameterInner> status)

Finds Pets by status

Multiple status values can be provided with comma separated strings


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **status** | [**List&lt;FindPetsByStatusStatusParameterInner&gt;**](FindPetsByStatusStatusParameterInner.md) | Status values that need to be considered for filter |  |

### Return type

[**List&lt;Pet&gt;**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth), [http_signature_test](../README.md#http_signature_test)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid status value |  -  |
| **2XX** | Anything within 200-299 |  -  |
| **4XX** | Anything within 400-499 |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="findpetsbytags"></a>
# **FindPetsByTags**
> List&lt;Pet&gt; FindPetsByTags (List<string> tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **tags** | [**List&lt;string&gt;**](string.md) | Tags to filter by |  |

### Return type

[**List&lt;Pet&gt;**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth), [http_signature_test](../README.md#http_signature_test)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid tag value |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="getpetbyid"></a>
# **GetPetById**
> Pet GetPetById (long petId)

Find pet by ID

Returns a single pet


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **petId** | **long** | ID of pet to return |  |

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key), [api_key_query](../README.md#api_key_query)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid ID supplied |  -  |
| **404** | Pet not found |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="updatepet"></a>
# **UpdatePet**
> void UpdatePet (Pet pet)

Update an existing pet


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **pet** | [**Pet**](Pet.md) | Pet object that needs to be added to the store |  |

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth), [http_signature_test](../README.md#http_signature_test)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid ID supplied |  -  |
| **404** | Pet not found |  -  |
| **405** | Validation exception |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="updatepetwithform"></a>
# **UpdatePetWithForm**
> void UpdatePetWithForm (long petId, string name = null, string status = null)

Updates a pet in the store with form data


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **petId** | **long** | ID of pet that needs to be updated |  |
| **name** | **string** | Updated name of the pet | [optional]  |
| **status** | **string** | Updated status of the pet | [optional]  |

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **405** | Invalid input |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="uploadfile"></a>
# **UploadFile**
> ApiResponse UploadFile (long petId, string additionalMetadata = null, System.IO.Stream file = null)

uploads an image


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **petId** | **long** | ID of pet to update |  |
| **additionalMetadata** | **string** | Additional data to pass to server | [optional]  |
| **file** | **System.IO.Stream****System.IO.Stream** | file to upload | [optional]  |

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="uploadfilewithrequiredfile"></a>
# **UploadFileWithRequiredFile**
> ApiResponse UploadFileWithRequiredFile (long petId, System.IO.Stream requiredFile, string additionalMetadata = null)

uploads an image (required)


### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **petId** | **long** | ID of pet to update |  |
| **requiredFile** | **System.IO.Stream****System.IO.Stream** | file to upload |  |
| **additionalMetadata** | **string** | Additional data to pass to server | [optional]  |

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json, application/vnd.openxmlformats-officedocument.spreadsheetml.sheet


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

