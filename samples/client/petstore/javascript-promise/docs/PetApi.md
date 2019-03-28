# OpenApiPetstore.PetApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

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
[**uploadFileWithRequiredFile**](PetApi.md#uploadFileWithRequiredFile) | **POST** /fake/{petId}/uploadImageWithRequiredFile | uploads an image (required)



## addPet

> addPet(body)

Add a new pet to the store

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');
var defaultClient = OpenApiPetstore.ApiClient.instance;
// Configure OAuth2 access token for authorization: petstore_auth
var petstore_auth = defaultClient.authentications['petstore_auth'];
petstore_auth.accessToken = 'YOUR ACCESS TOKEN';

var apiInstance = new OpenApiPetstore.PetApi();
var body = new OpenApiPetstore.Pet(); // Pet | Pet object that needs to be added to the store
apiInstance.addPet(body).then(function() {
  console.log('API called successfully.');
}, function(error) {
  console.error(error);
});

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

> deletePet(petId, opts)

Deletes a pet

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');
var defaultClient = OpenApiPetstore.ApiClient.instance;
// Configure OAuth2 access token for authorization: petstore_auth
var petstore_auth = defaultClient.authentications['petstore_auth'];
petstore_auth.accessToken = 'YOUR ACCESS TOKEN';

var apiInstance = new OpenApiPetstore.PetApi();
var petId = 789; // Number | Pet id to delete
var opts = {
  'apiKey': "apiKey_example" // String | 
};
apiInstance.deletePet(petId, opts).then(function() {
  console.log('API called successfully.');
}, function(error) {
  console.error(error);
});

```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Number**| Pet id to delete | 
 **apiKey** | **String**|  | [optional] 

### Return type

null (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


## findPetsByStatus

> [Pet] findPetsByStatus(status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');
var defaultClient = OpenApiPetstore.ApiClient.instance;
// Configure OAuth2 access token for authorization: petstore_auth
var petstore_auth = defaultClient.authentications['petstore_auth'];
petstore_auth.accessToken = 'YOUR ACCESS TOKEN';

var apiInstance = new OpenApiPetstore.PetApi();
var status = ["'available'"]; // [String] | Status values that need to be considered for filter
apiInstance.findPetsByStatus(status).then(function(data) {
  console.log('API called successfully. Returned data: ' + data);
}, function(error) {
  console.error(error);
});

```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**[String]**](String.md)| Status values that need to be considered for filter | 

### Return type

[**[Pet]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xml, application/json


## findPetsByTags

> [Pet] findPetsByTags(tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');
var defaultClient = OpenApiPetstore.ApiClient.instance;
// Configure OAuth2 access token for authorization: petstore_auth
var petstore_auth = defaultClient.authentications['petstore_auth'];
petstore_auth.accessToken = 'YOUR ACCESS TOKEN';

var apiInstance = new OpenApiPetstore.PetApi();
var tags = ["null"]; // [String] | Tags to filter by
apiInstance.findPetsByTags(tags).then(function(data) {
  console.log('API called successfully. Returned data: ' + data);
}, function(error) {
  console.error(error);
});

```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**[String]**](String.md)| Tags to filter by | 

### Return type

[**[Pet]**](Pet.md)

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

```javascript
var OpenApiPetstore = require('open_api_petstore');
var defaultClient = OpenApiPetstore.ApiClient.instance;
// Configure API key authorization: api_key
var api_key = defaultClient.authentications['api_key'];
api_key.apiKey = 'YOUR API KEY';
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//api_key.apiKeyPrefix = 'Token';

var apiInstance = new OpenApiPetstore.PetApi();
var petId = 789; // Number | ID of pet to return
apiInstance.getPetById(petId).then(function(data) {
  console.log('API called successfully. Returned data: ' + data);
}, function(error) {
  console.error(error);
});

```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Number**| ID of pet to return | 

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

```javascript
var OpenApiPetstore = require('open_api_petstore');
var defaultClient = OpenApiPetstore.ApiClient.instance;
// Configure OAuth2 access token for authorization: petstore_auth
var petstore_auth = defaultClient.authentications['petstore_auth'];
petstore_auth.accessToken = 'YOUR ACCESS TOKEN';

var apiInstance = new OpenApiPetstore.PetApi();
var body = new OpenApiPetstore.Pet(); // Pet | Pet object that needs to be added to the store
apiInstance.updatePet(body).then(function() {
  console.log('API called successfully.');
}, function(error) {
  console.error(error);
});

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

> updatePetWithForm(petId, opts)

Updates a pet in the store with form data

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');
var defaultClient = OpenApiPetstore.ApiClient.instance;
// Configure OAuth2 access token for authorization: petstore_auth
var petstore_auth = defaultClient.authentications['petstore_auth'];
petstore_auth.accessToken = 'YOUR ACCESS TOKEN';

var apiInstance = new OpenApiPetstore.PetApi();
var petId = 789; // Number | ID of pet that needs to be updated
var opts = {
  'name': "name_example", // String | Updated name of the pet
  'status': "status_example" // String | Updated status of the pet
};
apiInstance.updatePetWithForm(petId, opts).then(function() {
  console.log('API called successfully.');
}, function(error) {
  console.error(error);
});

```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Number**| ID of pet that needs to be updated | 
 **name** | **String**| Updated name of the pet | [optional] 
 **status** | **String**| Updated status of the pet | [optional] 

### Return type

null (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined


## uploadFile

> ApiResponse uploadFile(petId, opts)

uploads an image

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');
var defaultClient = OpenApiPetstore.ApiClient.instance;
// Configure OAuth2 access token for authorization: petstore_auth
var petstore_auth = defaultClient.authentications['petstore_auth'];
petstore_auth.accessToken = 'YOUR ACCESS TOKEN';

var apiInstance = new OpenApiPetstore.PetApi();
var petId = 789; // Number | ID of pet to update
var opts = {
  'additionalMetadata': "additionalMetadata_example", // String | Additional data to pass to server
  'file': "/path/to/file" // File | file to upload
};
apiInstance.uploadFile(petId, opts).then(function(data) {
  console.log('API called successfully. Returned data: ' + data);
}, function(error) {
  console.error(error);
});

```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Number**| ID of pet to update | 
 **additionalMetadata** | **String**| Additional data to pass to server | [optional] 
 **file** | **File**| file to upload | [optional] 

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json


## uploadFileWithRequiredFile

> ApiResponse uploadFileWithRequiredFile(petId, requiredFile, opts)

uploads an image (required)

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');
var defaultClient = OpenApiPetstore.ApiClient.instance;
// Configure OAuth2 access token for authorization: petstore_auth
var petstore_auth = defaultClient.authentications['petstore_auth'];
petstore_auth.accessToken = 'YOUR ACCESS TOKEN';

var apiInstance = new OpenApiPetstore.PetApi();
var petId = 789; // Number | ID of pet to update
var requiredFile = "/path/to/file"; // File | file to upload
var opts = {
  'additionalMetadata': "additionalMetadata_example" // String | Additional data to pass to server
};
apiInstance.uploadFileWithRequiredFile(petId, requiredFile, opts).then(function(data) {
  console.log('API called successfully. Returned data: ' + data);
}, function(error) {
  console.error(error);
});

```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Number**| ID of pet to update | 
 **requiredFile** | **File**| file to upload | 
 **additionalMetadata** | **String**| Additional data to pass to server | [optional] 

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

