# SWGUserApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createUser**](SWGUserApi.md#createuser) | **POST** /user | Create user
[**createUsersWithArrayInput**](SWGUserApi.md#createuserswitharrayinput) | **POST** /user/createWithArray | Creates list of users with given input array
[**createUsersWithListInput**](SWGUserApi.md#createuserswithlistinput) | **POST** /user/createWithList | Creates list of users with given input array
[**deleteUser**](SWGUserApi.md#deleteuser) | **DELETE** /user/{username} | Delete user
[**getUserByName**](SWGUserApi.md#getuserbyname) | **GET** /user/{username} | Get user by user name
[**loginUser**](SWGUserApi.md#loginuser) | **GET** /user/login | Logs user into the system
[**logoutUser**](SWGUserApi.md#logoutuser) | **GET** /user/logout | Logs out current logged in user session
[**updateUser**](SWGUserApi.md#updateuser) | **PUT** /user/{username} | Updated user


# **createUser**
```objc
-(NSURLSessionTask*) createUserWithUser: (SWGUser*) user
        completionHandler: (void (^)(NSError* error)) handler;
```

Create user

This can only be done by the logged in user.

### Example
```objc

SWGUser* user = [[SWGUser alloc] init]; // Created user object (optional)

SWGUserApi*apiInstance = [[SWGUserApi alloc] init];

// Create user
[apiInstance createUserWithUser:user
          completionHandler: ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling SWGUserApi->createUser: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | [**SWGUser***](SWGUser.md)| Created user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **createUsersWithArrayInput**
```objc
-(NSURLSessionTask*) createUsersWithArrayInputWithUser: (NSArray<SWGUser>*) user
        completionHandler: (void (^)(NSError* error)) handler;
```

Creates list of users with given input array

### Example
```objc

NSArray<SWGUser>* user = @[[[SWGUser alloc] init]]; // List of user object (optional)

SWGUserApi*apiInstance = [[SWGUserApi alloc] init];

// Creates list of users with given input array
[apiInstance createUsersWithArrayInputWithUser:user
          completionHandler: ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling SWGUserApi->createUsersWithArrayInput: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | [**NSArray&lt;SWGUser&gt;***](SWGUser.md)| List of user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **createUsersWithListInput**
```objc
-(NSURLSessionTask*) createUsersWithListInputWithUser: (NSArray<SWGUser>*) user
        completionHandler: (void (^)(NSError* error)) handler;
```

Creates list of users with given input array

### Example
```objc

NSArray<SWGUser>* user = @[[[SWGUser alloc] init]]; // List of user object (optional)

SWGUserApi*apiInstance = [[SWGUserApi alloc] init];

// Creates list of users with given input array
[apiInstance createUsersWithListInputWithUser:user
          completionHandler: ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling SWGUserApi->createUsersWithListInput: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | [**NSArray&lt;SWGUser&gt;***](SWGUser.md)| List of user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **deleteUser**
```objc
-(NSURLSessionTask*) deleteUserWithUsername: (NSString*) username
        completionHandler: (void (^)(NSError* error)) handler;
```

Delete user

This can only be done by the logged in user.

### Example
```objc

NSString* username = @"username_example"; // The name that needs to be deleted

SWGUserApi*apiInstance = [[SWGUserApi alloc] init];

// Delete user
[apiInstance deleteUserWithUsername:username
          completionHandler: ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling SWGUserApi->deleteUser: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **NSString***| The name that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getUserByName**
```objc
-(NSURLSessionTask*) getUserByNameWithUsername: (NSString*) username
        completionHandler: (void (^)(SWGUser* output, NSError* error)) handler;
```

Get user by user name

### Example
```objc

NSString* username = @"username_example"; // The name that needs to be fetched. Use user1 for testing. 

SWGUserApi*apiInstance = [[SWGUserApi alloc] init];

// Get user by user name
[apiInstance getUserByNameWithUsername:username
          completionHandler: ^(SWGUser* output, NSError* error) {
                        if (output) {
                            NSLog(@"%@", output);
                        }
                        if (error) {
                            NSLog(@"Error calling SWGUserApi->getUserByName: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **NSString***| The name that needs to be fetched. Use user1 for testing.  | 

### Return type

[**SWGUser***](SWGUser.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **loginUser**
```objc
-(NSURLSessionTask*) loginUserWithUsername: (NSString*) username
    password: (NSString*) password
        completionHandler: (void (^)(NSString* output, NSError* error)) handler;
```

Logs user into the system

### Example
```objc

NSString* username = @"username_example"; // The user name for login (optional)
NSString* password = @"password_example"; // The password for login in clear text (optional)

SWGUserApi*apiInstance = [[SWGUserApi alloc] init];

// Logs user into the system
[apiInstance loginUserWithUsername:username
              password:password
          completionHandler: ^(NSString* output, NSError* error) {
                        if (output) {
                            NSLog(@"%@", output);
                        }
                        if (error) {
                            NSLog(@"Error calling SWGUserApi->loginUser: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **NSString***| The user name for login | [optional] 
 **password** | **NSString***| The password for login in clear text | [optional] 

### Return type

**NSString***

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **logoutUser**
```objc
-(NSURLSessionTask*) logoutUserWithCompletionHandler: 
        (void (^)(NSError* error)) handler;
```

Logs out current logged in user session

### Example
```objc


SWGUserApi*apiInstance = [[SWGUserApi alloc] init];

// Logs out current logged in user session
[apiInstance logoutUserWithCompletionHandler: 
          ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling SWGUserApi->logoutUser: %@", error);
                        }
                    }];
```

### Parameters
This endpoint does not need any parameter.

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updateUser**
```objc
-(NSURLSessionTask*) updateUserWithUsername: (NSString*) username
    user: (SWGUser*) user
        completionHandler: (void (^)(NSError* error)) handler;
```

Updated user

This can only be done by the logged in user.

### Example
```objc

NSString* username = @"username_example"; // name that need to be deleted
SWGUser* user = [[SWGUser alloc] init]; // Updated user object (optional)

SWGUserApi*apiInstance = [[SWGUserApi alloc] init];

// Updated user
[apiInstance updateUserWithUsername:username
              user:user
          completionHandler: ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling SWGUserApi->updateUser: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **NSString***| name that need to be deleted | 
 **user** | [**SWGUser***](SWGUser.md)| Updated user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

