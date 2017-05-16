# SWGPetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](SWGPetApi.md#addpet) | **POST** /pet | Add a new pet to the store
[**deletePet**](SWGPetApi.md#deletepet) | **DELETE** /pet/{petId} | Deletes a pet
[**findPetsByStatus**](SWGPetApi.md#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status
[**findPetsByTags**](SWGPetApi.md#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags
[**getPetById**](SWGPetApi.md#getpetbyid) | **GET** /pet/{petId} | Find pet by ID
[**updatePet**](SWGPetApi.md#updatepet) | **PUT** /pet | Update an existing pet
[**updatePetWithForm**](SWGPetApi.md#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**uploadFile**](SWGPetApi.md#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image


# **addPet**
```objc
-(NSURLSessionTask*) addPetWithBody: (SWGPet*) body
        completionHandler: (void (^)(NSError* error)) handler;
```

Add a new pet to the store



### Example 
```objc
SWGDefaultConfiguration *apiConfig = [SWGDefaultConfiguration sharedConfig];

// Configure OAuth2 access token for authorization: (authentication scheme: petstore_auth)
[apiConfig setAccessToken:@"YOUR_ACCESS_TOKEN"];


SWGPet* body = [[SWGPet alloc] init]; // Pet object that needs to be added to the store

SWGPetApi*apiInstance = [[SWGPetApi alloc] init];

// Add a new pet to the store
[apiInstance addPetWithBody:body
          completionHandler: ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling SWGPetApi->addPet: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**SWGPet***](SWGPet*.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **deletePet**
```objc
-(NSURLSessionTask*) deletePetWithPetId: (NSNumber*) petId
    apiKey: (NSString*) apiKey
        completionHandler: (void (^)(NSError* error)) handler;
```

Deletes a pet



### Example 
```objc
SWGDefaultConfiguration *apiConfig = [SWGDefaultConfiguration sharedConfig];

// Configure OAuth2 access token for authorization: (authentication scheme: petstore_auth)
[apiConfig setAccessToken:@"YOUR_ACCESS_TOKEN"];


NSNumber* petId = @789; // Pet id to delete
NSString* apiKey = @"apiKey_example"; //  (optional)

SWGPetApi*apiInstance = [[SWGPetApi alloc] init];

// Deletes a pet
[apiInstance deletePetWithPetId:petId
              apiKey:apiKey
          completionHandler: ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling SWGPetApi->deletePet: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **NSNumber***| Pet id to delete | 
 **apiKey** | **NSString***|  | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **findPetsByStatus**
```objc
-(NSURLSessionTask*) findPetsByStatusWithStatus: (NSArray<NSString*>*) status
        completionHandler: (void (^)(NSArray<SWGPet>* output, NSError* error)) handler;
```

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example 
```objc
SWGDefaultConfiguration *apiConfig = [SWGDefaultConfiguration sharedConfig];

// Configure OAuth2 access token for authorization: (authentication scheme: petstore_auth)
[apiConfig setAccessToken:@"YOUR_ACCESS_TOKEN"];


NSArray<NSString*>* status = @[@"status_example"]; // Status values that need to be considered for filter

SWGPetApi*apiInstance = [[SWGPetApi alloc] init];

// Finds Pets by status
[apiInstance findPetsByStatusWithStatus:status
          completionHandler: ^(NSArray<SWGPet>* output, NSError* error) {
                        if (output) {
                            NSLog(@"%@", output);
                        }
                        if (error) {
                            NSLog(@"Error calling SWGPetApi->findPetsByStatus: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**NSArray&lt;NSString*&gt;***](NSString*.md)| Status values that need to be considered for filter | 

### Return type

[**NSArray<SWGPet>***](SWGPet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **findPetsByTags**
```objc
-(NSURLSessionTask*) findPetsByTagsWithTags: (NSArray<NSString*>*) tags
        completionHandler: (void (^)(NSArray<SWGPet>* output, NSError* error)) handler;
```

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example 
```objc
SWGDefaultConfiguration *apiConfig = [SWGDefaultConfiguration sharedConfig];

// Configure OAuth2 access token for authorization: (authentication scheme: petstore_auth)
[apiConfig setAccessToken:@"YOUR_ACCESS_TOKEN"];


NSArray<NSString*>* tags = @[@"tags_example"]; // Tags to filter by

SWGPetApi*apiInstance = [[SWGPetApi alloc] init];

// Finds Pets by tags
[apiInstance findPetsByTagsWithTags:tags
          completionHandler: ^(NSArray<SWGPet>* output, NSError* error) {
                        if (output) {
                            NSLog(@"%@", output);
                        }
                        if (error) {
                            NSLog(@"Error calling SWGPetApi->findPetsByTags: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**NSArray&lt;NSString*&gt;***](NSString*.md)| Tags to filter by | 

### Return type

[**NSArray<SWGPet>***](SWGPet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getPetById**
```objc
-(NSURLSessionTask*) getPetByIdWithPetId: (NSNumber*) petId
        completionHandler: (void (^)(SWGPet* output, NSError* error)) handler;
```

Find pet by ID

Returns a single pet

### Example 
```objc
SWGDefaultConfiguration *apiConfig = [SWGDefaultConfiguration sharedConfig];

// Configure API key authorization: (authentication scheme: api_key)
[apiConfig setApiKey:@"YOUR_API_KEY" forApiKeyIdentifier:@"api_key"];
// Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
//[apiConfig setApiKeyPrefix:@"Bearer" forApiKeyIdentifier:@"api_key"];


NSNumber* petId = @789; // ID of pet to return

SWGPetApi*apiInstance = [[SWGPetApi alloc] init];

// Find pet by ID
[apiInstance getPetByIdWithPetId:petId
          completionHandler: ^(SWGPet* output, NSError* error) {
                        if (output) {
                            NSLog(@"%@", output);
                        }
                        if (error) {
                            NSLog(@"Error calling SWGPetApi->getPetById: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **NSNumber***| ID of pet to return | 

### Return type

[**SWGPet***](SWGPet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updatePet**
```objc
-(NSURLSessionTask*) updatePetWithBody: (SWGPet*) body
        completionHandler: (void (^)(NSError* error)) handler;
```

Update an existing pet



### Example 
```objc
SWGDefaultConfiguration *apiConfig = [SWGDefaultConfiguration sharedConfig];

// Configure OAuth2 access token for authorization: (authentication scheme: petstore_auth)
[apiConfig setAccessToken:@"YOUR_ACCESS_TOKEN"];


SWGPet* body = [[SWGPet alloc] init]; // Pet object that needs to be added to the store

SWGPetApi*apiInstance = [[SWGPetApi alloc] init];

// Update an existing pet
[apiInstance updatePetWithBody:body
          completionHandler: ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling SWGPetApi->updatePet: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**SWGPet***](SWGPet*.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updatePetWithForm**
```objc
-(NSURLSessionTask*) updatePetWithFormWithPetId: (NSNumber*) petId
    name: (NSString*) name
    status: (NSString*) status
        completionHandler: (void (^)(NSError* error)) handler;
```

Updates a pet in the store with form data



### Example 
```objc
SWGDefaultConfiguration *apiConfig = [SWGDefaultConfiguration sharedConfig];

// Configure OAuth2 access token for authorization: (authentication scheme: petstore_auth)
[apiConfig setAccessToken:@"YOUR_ACCESS_TOKEN"];


NSNumber* petId = @789; // ID of pet that needs to be updated
NSString* name = @"name_example"; // Updated name of the pet (optional)
NSString* status = @"status_example"; // Updated status of the pet (optional)

SWGPetApi*apiInstance = [[SWGPetApi alloc] init];

// Updates a pet in the store with form data
[apiInstance updatePetWithFormWithPetId:petId
              name:name
              status:status
          completionHandler: ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling SWGPetApi->updatePetWithForm: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **NSNumber***| ID of pet that needs to be updated | 
 **name** | **NSString***| Updated name of the pet | [optional] 
 **status** | **NSString***| Updated status of the pet | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **uploadFile**
```objc
-(NSURLSessionTask*) uploadFileWithPetId: (NSNumber*) petId
    additionalMetadata: (NSString*) additionalMetadata
    file: (NSURL*) file
        completionHandler: (void (^)(SWGApiResponse* output, NSError* error)) handler;
```

uploads an image



### Example 
```objc
SWGDefaultConfiguration *apiConfig = [SWGDefaultConfiguration sharedConfig];

// Configure OAuth2 access token for authorization: (authentication scheme: petstore_auth)
[apiConfig setAccessToken:@"YOUR_ACCESS_TOKEN"];


NSNumber* petId = @789; // ID of pet to update
NSString* additionalMetadata = @"additionalMetadata_example"; // Additional data to pass to server (optional)
NSURL* file = [NSURL fileURLWithPath:@"/path/to/file.txt"]; // file to upload (optional)

SWGPetApi*apiInstance = [[SWGPetApi alloc] init];

// uploads an image
[apiInstance uploadFileWithPetId:petId
              additionalMetadata:additionalMetadata
              file:file
          completionHandler: ^(SWGApiResponse* output, NSError* error) {
                        if (output) {
                            NSLog(@"%@", output);
                        }
                        if (error) {
                            NSLog(@"Error calling SWGPetApi->uploadFile: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **NSNumber***| ID of pet to update | 
 **additionalMetadata** | **NSString***| Additional data to pass to server | [optional] 
 **file** | **NSURL***| file to upload | [optional] 

### Return type

[**SWGApiResponse***](SWGApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

