# SwaggerPetstore.PetApi

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


<a name="addPet"></a>
# **addPet**
> addPet(body)

Add a new pet to the store



### Example
```javascript
import SwaggerPetstore from 'swagger_petstore';
let defaultClient = SwaggerPetstore.ApiClient.instance;

// Configure OAuth2 access token for authorization: petstore_auth
let petstore_auth = defaultClient.authentications['petstore_auth'];
petstore_auth.accessToken = 'YOUR ACCESS TOKEN';

let apiInstance = new SwaggerPetstore.PetApi();

let body = new SwaggerPetstore.Pet(); // Pet | Pet object that needs to be added to the store

apiInstance.addPet(body).then(() => {
  console.log('API called successfully.');
}, (error) => {
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
 - **Accept**: application/xml, application/json

<a name="deletePet"></a>
# **deletePet**
> deletePet(petId, opts)

Deletes a pet



### Example
```javascript
import SwaggerPetstore from 'swagger_petstore';
let defaultClient = SwaggerPetstore.ApiClient.instance;

// Configure OAuth2 access token for authorization: petstore_auth
let petstore_auth = defaultClient.authentications['petstore_auth'];
petstore_auth.accessToken = 'YOUR ACCESS TOKEN';

let apiInstance = new SwaggerPetstore.PetApi();

let petId = 789; // Number | Pet id to delete

let opts = { 
  'apiKey': "apiKey_example" // String | 
};
apiInstance.deletePet(petId, opts).then(() => {
  console.log('API called successfully.');
}, (error) => {
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
 - **Accept**: application/xml, application/json

<a name="findPetsByStatus"></a>
# **findPetsByStatus**
> [Pet] findPetsByStatus(status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```javascript
import SwaggerPetstore from 'swagger_petstore';
let defaultClient = SwaggerPetstore.ApiClient.instance;

// Configure OAuth2 access token for authorization: petstore_auth
let petstore_auth = defaultClient.authentications['petstore_auth'];
petstore_auth.accessToken = 'YOUR ACCESS TOKEN';

let apiInstance = new SwaggerPetstore.PetApi();

let status = ["status_example"]; // [String] | Status values that need to be considered for filter

apiInstance.findPetsByStatus(status).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
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

<a name="findPetsByTags"></a>
# **findPetsByTags**
> [Pet] findPetsByTags(tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```javascript
import SwaggerPetstore from 'swagger_petstore';
let defaultClient = SwaggerPetstore.ApiClient.instance;

// Configure OAuth2 access token for authorization: petstore_auth
let petstore_auth = defaultClient.authentications['petstore_auth'];
petstore_auth.accessToken = 'YOUR ACCESS TOKEN';

let apiInstance = new SwaggerPetstore.PetApi();

let tags = ["tags_example"]; // [String] | Tags to filter by

apiInstance.findPetsByTags(tags).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
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

<a name="getPetById"></a>
# **getPetById**
> Pet getPetById(petId)

Find pet by ID

Returns a single pet

### Example
```javascript
import SwaggerPetstore from 'swagger_petstore';
let defaultClient = SwaggerPetstore.ApiClient.instance;

// Configure API key authorization: api_key
let api_key = defaultClient.authentications['api_key'];
api_key.apiKey = 'YOUR API KEY';
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//api_key.apiKeyPrefix = 'Token';

let apiInstance = new SwaggerPetstore.PetApi();

let petId = 789; // Number | ID of pet to return

apiInstance.getPetById(petId).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
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

<a name="updatePet"></a>
# **updatePet**
> updatePet(body)

Update an existing pet



### Example
```javascript
import SwaggerPetstore from 'swagger_petstore';
let defaultClient = SwaggerPetstore.ApiClient.instance;

// Configure OAuth2 access token for authorization: petstore_auth
let petstore_auth = defaultClient.authentications['petstore_auth'];
petstore_auth.accessToken = 'YOUR ACCESS TOKEN';

let apiInstance = new SwaggerPetstore.PetApi();

let body = new SwaggerPetstore.Pet(); // Pet | Pet object that needs to be added to the store

apiInstance.updatePet(body).then(() => {
  console.log('API called successfully.');
}, (error) => {
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
 - **Accept**: application/xml, application/json

<a name="updatePetWithForm"></a>
# **updatePetWithForm**
> updatePetWithForm(petId, opts)

Updates a pet in the store with form data



### Example
```javascript
import SwaggerPetstore from 'swagger_petstore';
let defaultClient = SwaggerPetstore.ApiClient.instance;

// Configure OAuth2 access token for authorization: petstore_auth
let petstore_auth = defaultClient.authentications['petstore_auth'];
petstore_auth.accessToken = 'YOUR ACCESS TOKEN';

let apiInstance = new SwaggerPetstore.PetApi();

let petId = 789; // Number | ID of pet that needs to be updated

let opts = { 
  'name': "name_example", // String | Updated name of the pet
  'status': "status_example" // String | Updated status of the pet
};
apiInstance.updatePetWithForm(petId, opts).then(() => {
  console.log('API called successfully.');
}, (error) => {
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
 - **Accept**: application/xml, application/json

<a name="uploadFile"></a>
# **uploadFile**
> ApiResponse uploadFile(petId, opts)

uploads an image



### Example
```javascript
import SwaggerPetstore from 'swagger_petstore';
let defaultClient = SwaggerPetstore.ApiClient.instance;

// Configure OAuth2 access token for authorization: petstore_auth
let petstore_auth = defaultClient.authentications['petstore_auth'];
petstore_auth.accessToken = 'YOUR ACCESS TOKEN';

let apiInstance = new SwaggerPetstore.PetApi();

let petId = 789; // Number | ID of pet to update

let opts = { 
  'additionalMetadata': "additionalMetadata_example", // String | Additional data to pass to server
  'file': "/path/to/file.txt" // File | file to upload
};
apiInstance.uploadFile(petId, opts).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
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

