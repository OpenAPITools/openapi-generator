# OAIPetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](OAIPetApi.md#addpet) | **POST** /pet | Add a new pet to the store
[**deletePet**](OAIPetApi.md#deletepet) | **DELETE** /pet/{petId} | Deletes a pet
[**findPetsByStatus**](OAIPetApi.md#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status
[**findPetsByTags**](OAIPetApi.md#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags
[**getPetById**](OAIPetApi.md#getpetbyid) | **GET** /pet/{petId} | Find pet by ID
[**updatePet**](OAIPetApi.md#updatepet) | **PUT** /pet | Update an existing pet
[**updatePetWithForm**](OAIPetApi.md#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**uploadFile**](OAIPetApi.md#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image


# **addPet**
```objc
-(NSURLSessionTask*) addPetWithPet: (OAIPet*) pet
        completionHandler: (void (^)(NSError* error)) handler;
```

Add a new pet to the store

### Example 
```objc
OAIDefaultConfiguration *apiConfig = [OAIDefaultConfiguration sharedConfig];

// Configure OAuth2 access token for authorization: (authentication scheme: petstore_auth)
[apiConfig setAccessToken:@"YOUR_ACCESS_TOKEN"];


OAIPet* pet = [[OAIPet alloc] init]; // Pet object that needs to be added to the store (optional)

OAIPetApi*apiInstance = [[OAIPetApi alloc] init];

// Add a new pet to the store
[apiInstance addPetWithPet:pet
          completionHandler: ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling OAIPetApi->addPet: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**OAIPet***](OAIPet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

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
OAIDefaultConfiguration *apiConfig = [OAIDefaultConfiguration sharedConfig];

// Configure OAuth2 access token for authorization: (authentication scheme: petstore_auth)
[apiConfig setAccessToken:@"YOUR_ACCESS_TOKEN"];


NSNumber* petId = @56; // Pet id to delete
NSString* apiKey = @"apiKey_example"; //  (optional)

OAIPetApi*apiInstance = [[OAIPetApi alloc] init];

// Deletes a pet
[apiInstance deletePetWithPetId:petId
              apiKey:apiKey
          completionHandler: ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling OAIPetApi->deletePet: %@", error);
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
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **findPetsByStatus**
```objc
-(NSURLSessionTask*) findPetsByStatusWithXHeaderTest: (NSString*) xHeaderTest
    status: (NSArray<NSString*>*) status
        completionHandler: (void (^)(NSArray<OAIPet>* output, NSError* error)) handler;
```

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example 
```objc
OAIDefaultConfiguration *apiConfig = [OAIDefaultConfiguration sharedConfig];

// Configure OAuth2 access token for authorization: (authentication scheme: petstore_auth)
[apiConfig setAccessToken:@"YOUR_ACCESS_TOKEN"];


NSString* xHeaderTest = @"xHeaderTest_example"; // A header to test if headers work.
NSArray<NSString*>* status = @[@"status_example"]; // Status values that need to be considered for filter (optional)

OAIPetApi*apiInstance = [[OAIPetApi alloc] init];

// Finds Pets by status
[apiInstance findPetsByStatusWithXHeaderTest:xHeaderTest
              status:status
          completionHandler: ^(NSArray<OAIPet>* output, NSError* error) {
                        if (output) {
                            NSLog(@"%@", output);
                        }
                        if (error) {
                            NSLog(@"Error calling OAIPetApi->findPetsByStatus: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xHeaderTest** | **NSString***| A header to test if headers work. | 
 **status** | [**NSArray&lt;NSString*&gt;***](NSString*.md)| Status values that need to be considered for filter | [optional] 

### Return type

[**NSArray<OAIPet>***](OAIPet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **findPetsByTags**
```objc
-(NSURLSessionTask*) findPetsByTagsWithTags: (NSArray<NSString*>*) tags
        completionHandler: (void (^)(NSArray<OAIPet>* output, NSError* error)) handler;
```

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example 
```objc
OAIDefaultConfiguration *apiConfig = [OAIDefaultConfiguration sharedConfig];

// Configure OAuth2 access token for authorization: (authentication scheme: petstore_auth)
[apiConfig setAccessToken:@"YOUR_ACCESS_TOKEN"];


NSArray<NSString*>* tags = @[@"tags_example"]; // Tags to filter by (optional)

OAIPetApi*apiInstance = [[OAIPetApi alloc] init];

// Finds Pets by tags
[apiInstance findPetsByTagsWithTags:tags
          completionHandler: ^(NSArray<OAIPet>* output, NSError* error) {
                        if (output) {
                            NSLog(@"%@", output);
                        }
                        if (error) {
                            NSLog(@"Error calling OAIPetApi->findPetsByTags: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**NSArray&lt;NSString*&gt;***](NSString*.md)| Tags to filter by | [optional] 

### Return type

[**NSArray<OAIPet>***](OAIPet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getPetById**
```objc
-(NSURLSessionTask*) getPetByIdWithPetId: (NSNumber*) petId
        completionHandler: (void (^)(OAIPet* output, NSError* error)) handler;
```

Find pet by ID

Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

### Example 
```objc
OAIDefaultConfiguration *apiConfig = [OAIDefaultConfiguration sharedConfig];

// Configure API key authorization: (authentication scheme: api_key)
[apiConfig setApiKey:@"YOUR_API_KEY" forApiKeyIdentifier:@"api_key"];
// Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
//[apiConfig setApiKeyPrefix:@"Bearer" forApiKeyIdentifier:@"api_key"];

// Configure OAuth2 access token for authorization: (authentication scheme: petstore_auth)
[apiConfig setAccessToken:@"YOUR_ACCESS_TOKEN"];


NSNumber* petId = @56; // ID of pet that needs to be fetched

OAIPetApi*apiInstance = [[OAIPetApi alloc] init];

// Find pet by ID
[apiInstance getPetByIdWithPetId:petId
          completionHandler: ^(OAIPet* output, NSError* error) {
                        if (output) {
                            NSLog(@"%@", output);
                        }
                        if (error) {
                            NSLog(@"Error calling OAIPetApi->getPetById: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **NSNumber***| ID of pet that needs to be fetched | 

### Return type

[**OAIPet***](OAIPet.md)

### Authorization

[api_key](../README.md#api_key), [petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updatePet**
```objc
-(NSURLSessionTask*) updatePetWithPet: (OAIPet*) pet
        completionHandler: (void (^)(NSError* error)) handler;
```

Update an existing pet

### Example 
```objc
OAIDefaultConfiguration *apiConfig = [OAIDefaultConfiguration sharedConfig];

// Configure OAuth2 access token for authorization: (authentication scheme: petstore_auth)
[apiConfig setAccessToken:@"YOUR_ACCESS_TOKEN"];


OAIPet* pet = [[OAIPet alloc] init]; // Pet object that needs to be added to the store (optional)

OAIPetApi*apiInstance = [[OAIPetApi alloc] init];

// Update an existing pet
[apiInstance updatePetWithPet:pet
          completionHandler: ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling OAIPetApi->updatePet: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**OAIPet***](OAIPet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updatePetWithForm**
```objc
-(NSURLSessionTask*) updatePetWithFormWithPetId: (NSString*) petId
    name: (NSString*) name
    status: (NSString*) status
        completionHandler: (void (^)(NSError* error)) handler;
```

Updates a pet in the store with form data

### Example 
```objc
OAIDefaultConfiguration *apiConfig = [OAIDefaultConfiguration sharedConfig];

// Configure OAuth2 access token for authorization: (authentication scheme: petstore_auth)
[apiConfig setAccessToken:@"YOUR_ACCESS_TOKEN"];


NSString* petId = @"petId_example"; // ID of pet that needs to be updated
NSString* name = @"name_example"; // Updated name of the pet (optional)
NSString* status = @"status_example"; // Updated status of the pet (optional)

OAIPetApi*apiInstance = [[OAIPetApi alloc] init];

// Updates a pet in the store with form data
[apiInstance updatePetWithFormWithPetId:petId
              name:name
              status:status
          completionHandler: ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling OAIPetApi->updatePetWithForm: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **NSString***| ID of pet that needs to be updated | 
 **name** | **NSString***| Updated name of the pet | [optional] 
 **status** | **NSString***| Updated status of the pet | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **uploadFile**
```objc
-(NSURLSessionTask*) uploadFileWithPetId: (NSNumber*) petId
    additionalMetadata: (NSString*) additionalMetadata
    file: (NSURL*) file
        completionHandler: (void (^)(NSError* error)) handler;
```

uploads an image

### Example 
```objc
OAIDefaultConfiguration *apiConfig = [OAIDefaultConfiguration sharedConfig];

// Configure OAuth2 access token for authorization: (authentication scheme: petstore_auth)
[apiConfig setAccessToken:@"YOUR_ACCESS_TOKEN"];


NSNumber* petId = @56; // ID of pet to update
NSString* additionalMetadata = @"additionalMetadata_example"; // Additional data to pass to server (optional)
NSURL* file = [NSURL fileURLWithPath:@"/path/to/file"]; // file to upload (optional)

OAIPetApi*apiInstance = [[OAIPetApi alloc] init];

// uploads an image
[apiInstance uploadFileWithPetId:petId
              additionalMetadata:additionalMetadata
              file:file
          completionHandler: ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling OAIPetApi->uploadFile: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **NSNumber***| ID of pet to update | 
 **additionalMetadata** | **NSString***| Additional data to pass to server | [optional] 
 **file** | **NSURL*****NSURL***| file to upload | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

