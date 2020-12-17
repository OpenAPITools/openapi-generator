# \PetApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**AddPet**](PetApi.md#AddPet) | **Post** /pet | Add a new pet to the store
[**DeletePet**](PetApi.md#DeletePet) | **Delete** /pet/{petId} | Deletes a pet
[**FindPetsByStatus**](PetApi.md#FindPetsByStatus) | **Get** /pet/findByStatus | Finds Pets by status
[**FindPetsByTags**](PetApi.md#FindPetsByTags) | **Get** /pet/findByTags | Finds Pets by tags
[**GetPetById**](PetApi.md#GetPetById) | **Get** /pet/{petId} | Find pet by ID
[**UpdatePet**](PetApi.md#UpdatePet) | **Put** /pet | Update an existing pet
[**UpdatePetWithForm**](PetApi.md#UpdatePetWithForm) | **Post** /pet/{petId} | Updates a pet in the store with form data
[**UploadFile**](PetApi.md#UploadFile) | **Post** /pet/{petId}/uploadImage | uploads an image
[**UploadFileWithRequiredFile**](PetApi.md#UploadFileWithRequiredFile) | **Post** /fake/{petId}/uploadImageWithRequiredFile | uploads an image (required)



## AddPet

> AddPet(ctx).Pet(pet).Execute()

Add a new pet to the store

### Example

```go
package main

import (
    "context"
    "fmt"
    "os"
    openapiclient "./openapi"
)

func main() {
    pet := *openapiclient.NewPet("doggie", []string{"PhotoUrls_example"}) // Pet | Pet object that needs to be added to the store

    configuration := openapiclient.NewConfiguration()
    api_client := openapiclient.NewAPIClient(configuration)
    resp, r, err := api_client.PetApi.AddPet(context.Background()).Pet(pet).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `PetApi.AddPet``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiAddPetRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md) | Pet object that needs to be added to the store | 

### Return type

 (empty response body)

### Authorization

[http_signature_test](../README.md#http_signature_test), [petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: application/json, application/xml
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## DeletePet

> DeletePet(ctx, petId).ApiKey(apiKey).Execute()

Deletes a pet

### Example

```go
package main

import (
    "context"
    "fmt"
    "os"
    openapiclient "./openapi"
)

func main() {
    petId := int64(789) // int64 | Pet id to delete
    apiKey := "apiKey_example" // string |  (optional)

    configuration := openapiclient.NewConfiguration()
    api_client := openapiclient.NewAPIClient(configuration)
    resp, r, err := api_client.PetApi.DeletePet(context.Background(), petId).ApiKey(apiKey).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `PetApi.DeletePet``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**petId** | **int64** | Pet id to delete | 

### Other Parameters

Other parameters are passed through a pointer to a apiDeletePetRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **apiKey** | **string** |  | 

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

> []Pet FindPetsByStatus(ctx).Status(status).Execute()

Finds Pets by status



### Example

```go
package main

import (
    "context"
    "fmt"
    "os"
    openapiclient "./openapi"
)

func main() {
    status := []string{"Status_example"} // []string | Status values that need to be considered for filter

    configuration := openapiclient.NewConfiguration()
    api_client := openapiclient.NewAPIClient(configuration)
    resp, r, err := api_client.PetApi.FindPetsByStatus(context.Background()).Status(status).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `PetApi.FindPetsByStatus``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `FindPetsByStatus`: []Pet
    fmt.Fprintf(os.Stdout, "Response from `PetApi.FindPetsByStatus`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiFindPetsByStatusRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | **[]string** | Status values that need to be considered for filter | 

### Return type

[**[]Pet**](Pet.md)

### Authorization

[http_signature_test](../README.md#http_signature_test), [petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## FindPetsByTags

> []Pet FindPetsByTags(ctx).Tags(tags).Execute()

Finds Pets by tags



### Example

```go
package main

import (
    "context"
    "fmt"
    "os"
    openapiclient "./openapi"
)

func main() {
    tags := []string{"Inner_example"} // []string | Tags to filter by

    configuration := openapiclient.NewConfiguration()
    api_client := openapiclient.NewAPIClient(configuration)
    resp, r, err := api_client.PetApi.FindPetsByTags(context.Background()).Tags(tags).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `PetApi.FindPetsByTags``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `FindPetsByTags`: []Pet
    fmt.Fprintf(os.Stdout, "Response from `PetApi.FindPetsByTags`: %v\n", resp)
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiFindPetsByTagsRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | **[]string** | Tags to filter by | 

### Return type

[**[]Pet**](Pet.md)

### Authorization

[http_signature_test](../README.md#http_signature_test), [petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## GetPetById

> Pet GetPetById(ctx, petId).Execute()

Find pet by ID



### Example

```go
package main

import (
    "context"
    "fmt"
    "os"
    openapiclient "./openapi"
)

func main() {
    petId := int64(789) // int64 | ID of pet to return

    configuration := openapiclient.NewConfiguration()
    api_client := openapiclient.NewAPIClient(configuration)
    resp, r, err := api_client.PetApi.GetPetById(context.Background(), petId).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `PetApi.GetPetById``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `GetPetById`: Pet
    fmt.Fprintf(os.Stdout, "Response from `PetApi.GetPetById`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**petId** | **int64** | ID of pet to return | 

### Other Parameters

Other parameters are passed through a pointer to a apiGetPetByIdRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------


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

> UpdatePet(ctx).Pet(pet).Execute()

Update an existing pet

### Example

```go
package main

import (
    "context"
    "fmt"
    "os"
    openapiclient "./openapi"
)

func main() {
    pet := *openapiclient.NewPet("doggie", []string{"PhotoUrls_example"}) // Pet | Pet object that needs to be added to the store

    configuration := openapiclient.NewConfiguration()
    api_client := openapiclient.NewAPIClient(configuration)
    resp, r, err := api_client.PetApi.UpdatePet(context.Background()).Pet(pet).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `PetApi.UpdatePet``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
}
```

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiUpdatePetRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md) | Pet object that needs to be added to the store | 

### Return type

 (empty response body)

### Authorization

[http_signature_test](../README.md#http_signature_test), [petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: application/json, application/xml
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## UpdatePetWithForm

> UpdatePetWithForm(ctx, petId).Name(name).Status(status).Execute()

Updates a pet in the store with form data

### Example

```go
package main

import (
    "context"
    "fmt"
    "os"
    openapiclient "./openapi"
)

func main() {
    petId := int64(789) // int64 | ID of pet that needs to be updated
    name := "name_example" // string | Updated name of the pet (optional)
    status := "status_example" // string | Updated status of the pet (optional)

    configuration := openapiclient.NewConfiguration()
    api_client := openapiclient.NewAPIClient(configuration)
    resp, r, err := api_client.PetApi.UpdatePetWithForm(context.Background(), petId).Name(name).Status(status).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `PetApi.UpdatePetWithForm``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**petId** | **int64** | ID of pet that needs to be updated | 

### Other Parameters

Other parameters are passed through a pointer to a apiUpdatePetWithFormRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **name** | **string** | Updated name of the pet | 
 **status** | **string** | Updated status of the pet | 

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

> ApiResponse UploadFile(ctx, petId).AdditionalMetadata(additionalMetadata).File(file).Execute()

uploads an image

### Example

```go
package main

import (
    "context"
    "fmt"
    "os"
    openapiclient "./openapi"
)

func main() {
    petId := int64(789) // int64 | ID of pet to update
    additionalMetadata := "additionalMetadata_example" // string | Additional data to pass to server (optional)
    file := os.NewFile(1234, "some_file") // *os.File | file to upload (optional)

    configuration := openapiclient.NewConfiguration()
    api_client := openapiclient.NewAPIClient(configuration)
    resp, r, err := api_client.PetApi.UploadFile(context.Background(), petId).AdditionalMetadata(additionalMetadata).File(file).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `PetApi.UploadFile``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `UploadFile`: ApiResponse
    fmt.Fprintf(os.Stdout, "Response from `PetApi.UploadFile`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**petId** | **int64** | ID of pet to update | 

### Other Parameters

Other parameters are passed through a pointer to a apiUploadFileRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **additionalMetadata** | **string** | Additional data to pass to server | 
 **file** | ***os.File** | file to upload | 

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## UploadFileWithRequiredFile

> ApiResponse UploadFileWithRequiredFile(ctx, petId).RequiredFile(requiredFile).AdditionalMetadata(additionalMetadata).Execute()

uploads an image (required)

### Example

```go
package main

import (
    "context"
    "fmt"
    "os"
    openapiclient "./openapi"
)

func main() {
    petId := int64(789) // int64 | ID of pet to update
    requiredFile := os.NewFile(1234, "some_file") // *os.File | file to upload
    additionalMetadata := "additionalMetadata_example" // string | Additional data to pass to server (optional)

    configuration := openapiclient.NewConfiguration()
    api_client := openapiclient.NewAPIClient(configuration)
    resp, r, err := api_client.PetApi.UploadFileWithRequiredFile(context.Background(), petId).RequiredFile(requiredFile).AdditionalMetadata(additionalMetadata).Execute()
    if err != nil {
        fmt.Fprintf(os.Stderr, "Error when calling `PetApi.UploadFileWithRequiredFile``: %v\n", err)
        fmt.Fprintf(os.Stderr, "Full HTTP response: %v\n", r)
    }
    // response from `UploadFileWithRequiredFile`: ApiResponse
    fmt.Fprintf(os.Stdout, "Response from `PetApi.UploadFileWithRequiredFile`: %v\n", resp)
}
```

### Path Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**ctx** | **context.Context** | context for authentication, logging, cancellation, deadlines, tracing, etc.
**petId** | **int64** | ID of pet to update | 

### Other Parameters

Other parameters are passed through a pointer to a apiUploadFileWithRequiredFileRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

 **requiredFile** | ***os.File** | file to upload | 
 **additionalMetadata** | **string** | Additional data to pass to server | 

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)

