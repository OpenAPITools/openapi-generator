# SwagPetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](SwagPetApi.md#addPet) | **POST** /pet | Add a new pet to the store
[**deletePet**](SwagPetApi.md#deletePet) | **DELETE** /pet/{petId} | Deletes a pet
[**findPetsByStatus**](SwagPetApi.md#findPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
[**findPetsByTags**](SwagPetApi.md#findPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
[**getPetById**](SwagPetApi.md#getPetById) | **GET** /pet/{petId} | Find pet by ID
[**updatePet**](SwagPetApi.md#updatePet) | **PUT** /pet | Update an existing pet
[**updatePetWithForm**](SwagPetApi.md#updatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**uploadFile**](SwagPetApi.md#uploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image


<a name="addPet"></a>
# **addPet**
> addPet(body)

Add a new pet to the store



### Example
```java
SwagPetApi api = new SwagPetApi();
SwagClient client = api.getClient();

// Configure OAuth2 access token for authorization: petstore_auth
Swagger.OAuth petstore_auth = (Swagger.OAuth) client.getAuthentication('petstore_auth');
petstore_auth.setAccessToken('YOUR ACCESS TOKEN');

Map<String, Object> params = new Map<String, Object>{
    'body' => SwagPet.getExample()
};

try {
    // cross your fingers
    api.addPet(params);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**SwagPet**](SwagPet.md)| Pet object that needs to be added to the store |

### Return type

null (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="deletePet"></a>
# **deletePet**
> deletePet(petId, apiKey)

Deletes a pet



### Example
```java
SwagPetApi api = new SwagPetApi();
SwagClient client = api.getClient();

// Configure OAuth2 access token for authorization: petstore_auth
Swagger.OAuth petstore_auth = (Swagger.OAuth) client.getAuthentication('petstore_auth');
petstore_auth.setAccessToken('YOUR ACCESS TOKEN');

Map<String, Object> params = new Map<String, Object>{
    'petId' => 2147483648L,
    'apiKey' => 'apiKey_example'
};

try {
    // cross your fingers
    api.deletePet(params);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Long**| Pet id to delete |
 **apiKey** | **String**|  | [optional]

### Return type

null (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="findPetsByStatus"></a>
# **findPetsByStatus**
> List&lt;SwagPet&gt; findPetsByStatus(status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```java
SwagPetApi api = new SwagPetApi();
SwagClient client = api.getClient();

// Configure OAuth2 access token for authorization: petstore_auth
Swagger.OAuth petstore_auth = (Swagger.OAuth) client.getAuthentication('petstore_auth');
petstore_auth.setAccessToken('YOUR ACCESS TOKEN');

Map<String, Object> params = new Map<String, Object>{
    'status' => new List<String>{'available'}
};

try {
    // cross your fingers
    List<SwagPet> result = api.findPetsByStatus(params);
    System.debug(result);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**List&lt;String&gt;**](String.md)| Status values that need to be considered for filter | [enum: available, pending, sold]

### Return type

[**List&lt;SwagPet&gt;**](SwagPet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="findPetsByTags"></a>
# **findPetsByTags**
> List&lt;SwagPet&gt; findPetsByTags(tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```java
SwagPetApi api = new SwagPetApi();
SwagClient client = api.getClient();

// Configure OAuth2 access token for authorization: petstore_auth
Swagger.OAuth petstore_auth = (Swagger.OAuth) client.getAuthentication('petstore_auth');
petstore_auth.setAccessToken('YOUR ACCESS TOKEN');

Map<String, Object> params = new Map<String, Object>{
    'tags' => new List<String>{'aeiou'}
};

try {
    // cross your fingers
    List<SwagPet> result = api.findPetsByTags(params);
    System.debug(result);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**List&lt;String&gt;**](String.md)| Tags to filter by |

### Return type

[**List&lt;SwagPet&gt;**](SwagPet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="getPetById"></a>
# **getPetById**
> SwagPet getPetById(petId)

Find pet by ID

Returns a single pet

### Example
```java
SwagPetApi api = new SwagPetApi();
SwagClient client = api.getClient();

// Configure API key authorization: api_key
ApiKeyAuth api_key = (ApiKeyAuth) client.getAuthentication('api_key');
api_key.setApiKey('YOUR API KEY');

Map<String, Object> params = new Map<String, Object>{
    'petId' => 2147483648L
};

try {
    // cross your fingers
    SwagPet result = api.getPetById(params);
    System.debug(result);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Long**| ID of pet to return |

### Return type

[**SwagPet**](SwagPet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="updatePet"></a>
# **updatePet**
> updatePet(body)

Update an existing pet



### Example
```java
SwagPetApi api = new SwagPetApi();
SwagClient client = api.getClient();

// Configure OAuth2 access token for authorization: petstore_auth
Swagger.OAuth petstore_auth = (Swagger.OAuth) client.getAuthentication('petstore_auth');
petstore_auth.setAccessToken('YOUR ACCESS TOKEN');

Map<String, Object> params = new Map<String, Object>{
    'body' => SwagPet.getExample()
};

try {
    // cross your fingers
    api.updatePet(params);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**SwagPet**](SwagPet.md)| Pet object that needs to be added to the store |

### Return type

null (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="updatePetWithForm"></a>
# **updatePetWithForm**
> updatePetWithForm(petId, name, status)

Updates a pet in the store with form data



### Example
```java
SwagPetApi api = new SwagPetApi();
SwagClient client = api.getClient();

// Configure OAuth2 access token for authorization: petstore_auth
Swagger.OAuth petstore_auth = (Swagger.OAuth) client.getAuthentication('petstore_auth');
petstore_auth.setAccessToken('YOUR ACCESS TOKEN');

Map<String, Object> params = new Map<String, Object>{
    'petId' => 2147483648L,
    'name' => 'name_example',
    'status' => 'status_example'
};

try {
    // cross your fingers
    api.updatePetWithForm(params);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Long**| ID of pet that needs to be updated |
 **name** | **String**| Updated name of the pet | [optional]
 **status** | **String**| Updated status of the pet | [optional]

### Return type

null (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: application/json

<a name="uploadFile"></a>
# **uploadFile**
> SwagApiResponse uploadFile(petId, additionalMetadata, file)

uploads an image



### Example
```java
SwagPetApi api = new SwagPetApi();
SwagClient client = api.getClient();

// Configure OAuth2 access token for authorization: petstore_auth
Swagger.OAuth petstore_auth = (Swagger.OAuth) client.getAuthentication('petstore_auth');
petstore_auth.setAccessToken('YOUR ACCESS TOKEN');

Map<String, Object> params = new Map<String, Object>{
    'petId' => 2147483648L,
    'additionalMetadata' => 'additionalMetadata_example',
    'file' => Blob.valueOf('Sample text file\nContents')
};

try {
    // cross your fingers
    SwagApiResponse result = api.uploadFile(params);
    System.debug(result);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **Long**| ID of pet to update |
 **additionalMetadata** | **String**| Additional data to pass to server | [optional]
 **file** | **Blob**| file to upload | [optional]

### Return type

[**SwagApiResponse**](SwagApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: application/json

