# PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](PetApi.md#addPet) | **POST** /pet | Add a new pet to the store
[**deletePet**](PetApi.md#deletePet) | **DELETE** /pet/{petId} | Deletes a pet
[**findPetsByStatus**](PetApi.md#findPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
[**findPetsByTags**](PetApi.md#findPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
[**getPetById**](PetApi.md#getPetById) | **GET** /pet/{petId} | Find pet by ID
[**updatePet**](PetApi.md#updatePet) | **PUT** /pet | Update an existing pet
[**updatePetWithForm**](PetApi.md#updatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**uploadFile**](PetApi.md#uploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image



## addPet

> addPet(body)

Add a new pet to the store

### Example

```java
// Import classes:
//import org.openapitools.client.api.PetApi;

PetApi apiInstance = new PetApi();
Pet body = new Pet(); // Pet | Pet object that needs to be added to the store
try {
    apiInstance.addPet(body);
} catch (ApiException e) {
    System.err.println("Exception when calling PetApi#addPet");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store |

### Return type

null (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: application/json, application/xml
- **Accept**: Not defined


## deletePet

> deletePet(petId, apiKey)

Deletes a pet

### Example

```java
// Import classes:
//import org.openapitools.client.api.PetApi;

PetApi apiInstance = new PetApi();
Long petId = null; // Long | Pet id to delete
String apiKey = null; // String | 
try {
    apiInstance.deletePet(petId, apiKey);
} catch (ApiException e) {
    System.err.println("Exception when calling PetApi#deletePet");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Long**| Pet id to delete | [default to null]
 **apiKey** | **String**|  | [optional] [default to null]

### Return type

null (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


## findPetsByStatus

> List&lt;Pet&gt; findPetsByStatus(status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example

```java
// Import classes:
//import org.openapitools.client.api.PetApi;

PetApi apiInstance = new PetApi();
List<String> status = null; // List<String> | Status values that need to be considered for filter
try {
    List<Pet> result = apiInstance.findPetsByStatus(status);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling PetApi#findPetsByStatus");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**List&lt;String&gt;**](String.md)| Status values that need to be considered for filter | [default to null] [enum: available, pending, sold]

### Return type

[**List&lt;Pet&gt;**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xml, application/json


## findPetsByTags

> List&lt;Pet&gt; findPetsByTags(tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example

```java
// Import classes:
//import org.openapitools.client.api.PetApi;

PetApi apiInstance = new PetApi();
List<String> tags = null; // List<String> | Tags to filter by
try {
    List<Pet> result = apiInstance.findPetsByTags(tags);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling PetApi#findPetsByTags");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**List&lt;String&gt;**](String.md)| Tags to filter by | [default to null]

### Return type

[**List&lt;Pet&gt;**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xml, application/json


## getPetById

> Pet getPetById(petId)

Find pet by ID

Returns a single pet

### Example

```java
// Import classes:
//import org.openapitools.client.api.PetApi;

PetApi apiInstance = new PetApi();
Long petId = null; // Long | ID of pet to return
try {
    Pet result = apiInstance.getPetById(petId);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling PetApi#getPetById");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Long**| ID of pet to return | [default to null]

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xml, application/json


## updatePet

> updatePet(body)

Update an existing pet

### Example

```java
// Import classes:
//import org.openapitools.client.api.PetApi;

PetApi apiInstance = new PetApi();
Pet body = new Pet(); // Pet | Pet object that needs to be added to the store
try {
    apiInstance.updatePet(body);
} catch (ApiException e) {
    System.err.println("Exception when calling PetApi#updatePet");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store |

### Return type

null (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: application/json, application/xml
- **Accept**: Not defined


## updatePetWithForm

> updatePetWithForm(petId, name, status)

Updates a pet in the store with form data

### Example

```java
// Import classes:
//import org.openapitools.client.api.PetApi;

PetApi apiInstance = new PetApi();
Long petId = null; // Long | ID of pet that needs to be updated
String name = null; // String | Updated name of the pet
String status = null; // String | Updated status of the pet
try {
    apiInstance.updatePetWithForm(petId, name, status);
} catch (ApiException e) {
    System.err.println("Exception when calling PetApi#updatePetWithForm");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Long**| ID of pet that needs to be updated | [default to null]
 **name** | **String**| Updated name of the pet | [optional] [default to null]
 **status** | **String**| Updated status of the pet | [optional] [default to null]

### Return type

null (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined


## uploadFile

> ApiResponse uploadFile(petId, additionalMetadata, file)

uploads an image

### Example

```java
// Import classes:
//import org.openapitools.client.api.PetApi;

PetApi apiInstance = new PetApi();
Long petId = null; // Long | ID of pet to update
String additionalMetadata = null; // String | Additional data to pass to server
File file = null; // File | file to upload
try {
    ApiResponse result = apiInstance.uploadFile(petId, additionalMetadata, file);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling PetApi#uploadFile");
    e.printStackTrace();
}
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Long**| ID of pet to update | [default to null]
 **additionalMetadata** | **String**| Additional data to pass to server | [optional] [default to null]
 **file** | **File**| file to upload | [optional] [default to null]

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

