# PetAPI

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](PetAPI.md#addpet) | **POST** /pet | Add a new pet to the store
[**deletePet**](PetAPI.md#deletepet) | **DELETE** /pet/{petId} | Deletes a pet
[**findPetsByStatus**](PetAPI.md#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status
[**findPetsByTags**](PetAPI.md#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags
[**getPetById**](PetAPI.md#getpetbyid) | **GET** /pet/{petId} | Find pet by ID
[**updatePet**](PetAPI.md#updatepet) | **PUT** /pet | Update an existing pet
[**updatePetWithForm**](PetAPI.md#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**uploadFile**](PetAPI.md#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image
[**uploadFileWithRequiredFile**](PetAPI.md#uploadfilewithrequiredfile) | **POST** /fake/{petId}/uploadImageWithRequiredFile | uploads an image (required)


# **addPet**
```swift
    internal class func addPet(body: Pet, completion: @escaping (_ data: Void?, _ error: Error?) -> Void)
```

Add a new pet to the store

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let body = Pet(id: 123, category: Category(id: 123, name: "name_example"), name: "name_example", photoUrls: ["photoUrls_example"], tags: [Tag(id: 123, name: "name_example")], status: "status_example") // Pet | Pet object that needs to be added to the store

// Add a new pet to the store
PetAPI.addPet(body: body) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md) | Pet object that needs to be added to the store | 

### Return type

Void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **deletePet**
```swift
    internal class func deletePet(apiKey: String? = nil, petId: Int64, completion: @escaping (_ data: Void?, _ error: Error?) -> Void)
```

Deletes a pet

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let apiKey = "apiKey_example" // String |  (optional)
let petId = 987 // Int64 | Pet id to delete

// Deletes a pet
PetAPI.deletePet(apiKey: apiKey, petId: petId) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **apiKey** | **String** |  | [optional] 
 **petId** | **Int64** | Pet id to delete | 

### Return type

Void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **findPetsByStatus**
```swift
    internal class func findPetsByStatus(status: [String], completion: @escaping (_ data: [Pet]?, _ error: Error?) -> Void)
```

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let status = ["status_example"] // [String] | Status values that need to be considered for filter

// Finds Pets by status
PetAPI.findPetsByStatus(status: status) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**[String]**](String.md) | Status values that need to be considered for filter | 

### Return type

[**[Pet]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **findPetsByTags**
```swift
    internal class func findPetsByTags(tags: [String], completion: @escaping (_ data: [Pet]?, _ error: Error?) -> Void)
```

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let tags = ["inner_example"] // [String] | Tags to filter by

// Finds Pets by tags
PetAPI.findPetsByTags(tags: tags) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**[String]**](String.md) | Tags to filter by | 

### Return type

[**[Pet]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getPetById**
```swift
    internal class func getPetById(petId: Int64, completion: @escaping (_ data: Pet?, _ error: Error?) -> Void)
```

Find pet by ID

Returns a single pet

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let petId = 987 // Int64 | ID of pet to return

// Find pet by ID
PetAPI.getPetById(petId: petId) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Int64** | ID of pet to return | 

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updatePet**
```swift
    internal class func updatePet(body: Pet, completion: @escaping (_ data: Void?, _ error: Error?) -> Void)
```

Update an existing pet

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let body = Pet(id: 123, category: Category(id: 123, name: "name_example"), name: "name_example", photoUrls: ["photoUrls_example"], tags: [Tag(id: 123, name: "name_example")], status: "status_example") // Pet | Pet object that needs to be added to the store

// Update an existing pet
PetAPI.updatePet(body: body) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md) | Pet object that needs to be added to the store | 

### Return type

Void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updatePetWithForm**
```swift
    internal class func updatePetWithForm(petId: Int64, name: String? = nil, status: String? = nil, completion: @escaping (_ data: Void?, _ error: Error?) -> Void)
```

Updates a pet in the store with form data

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let petId = 987 // Int64 | ID of pet that needs to be updated
let name = "name_example" // String | Updated name of the pet (optional)
let status = "status_example" // String | Updated status of the pet (optional)

// Updates a pet in the store with form data
PetAPI.updatePetWithForm(petId: petId, name: name, status: status) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Int64** | ID of pet that needs to be updated | 
 **name** | **String** | Updated name of the pet | [optional] 
 **status** | **String** | Updated status of the pet | [optional] 

### Return type

Void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **uploadFile**
```swift
    internal class func uploadFile(petId: Int64, additionalMetadata: String? = nil, file: URL? = nil, completion: @escaping (_ data: ApiResponse?, _ error: Error?) -> Void)
```

uploads an image

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let petId = 987 // Int64 | ID of pet to update
let additionalMetadata = "additionalMetadata_example" // String | Additional data to pass to server (optional)
let file = URL(string: "https://example.com")! // URL | file to upload (optional)

// uploads an image
PetAPI.uploadFile(petId: petId, additionalMetadata: additionalMetadata, file: file) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Int64** | ID of pet to update | 
 **additionalMetadata** | **String** | Additional data to pass to server | [optional] 
 **file** | **URL** | file to upload | [optional] 

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **uploadFileWithRequiredFile**
```swift
    internal class func uploadFileWithRequiredFile(petId: Int64, additionalMetadata: String? = nil, requiredFile: URL, completion: @escaping (_ data: ApiResponse?, _ error: Error?) -> Void)
```

uploads an image (required)

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let petId = 987 // Int64 | ID of pet to update
let additionalMetadata = "additionalMetadata_example" // String | Additional data to pass to server (optional)
let requiredFile = URL(string: "https://example.com")! // URL | file to upload

// uploads an image (required)
PetAPI.uploadFileWithRequiredFile(petId: petId, additionalMetadata: additionalMetadata, requiredFile: requiredFile) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Int64** | ID of pet to update | 
 **additionalMetadata** | **String** | Additional data to pass to server | [optional] 
 **requiredFile** | **URL** | file to upload | 

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

