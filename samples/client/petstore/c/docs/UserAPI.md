# UserAPI

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**UserAPI_createUser**](UserAPI.md#UserAPI_createUser) | **POST** /user | Create user
[**UserAPI_createUsersWithArrayInput**](UserAPI.md#UserAPI_createUsersWithArrayInput) | **POST** /user/createWithArray | Creates list of users with given input array
[**UserAPI_createUsersWithListInput**](UserAPI.md#UserAPI_createUsersWithListInput) | **POST** /user/createWithList | Creates list of users with given input array
[**UserAPI_deleteUser**](UserAPI.md#UserAPI_deleteUser) | **DELETE** /user/{username} | Delete user
[**UserAPI_getUserByName**](UserAPI.md#UserAPI_getUserByName) | **GET** /user/{username} | Get user by user name
[**UserAPI_loginUser**](UserAPI.md#UserAPI_loginUser) | **GET** /user/login | Logs user into the system
[**UserAPI_logoutUser**](UserAPI.md#UserAPI_logoutUser) | **GET** /user/logout | Logs out current logged in user session
[**UserAPI_updateUser**](UserAPI.md#UserAPI_updateUser) | **PUT** /user/{username} | Updated user


# **UserAPI_createUser**
```c
// Create user
//
// This can only be done by the logged in user.
//
void UserAPI_createUser(apiClient_t *apiClient, user_t * body);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**body** | **[user_t](user.md) \*** | Created user object | 

### Return type

void

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **UserAPI_createUsersWithArrayInput**
```c
// Creates list of users with given input array
//
void UserAPI_createUsersWithArrayInput(apiClient_t *apiClient, list_t * body);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**body** | **[list_t](user.md) \*** | List of user object | 

### Return type

void

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **UserAPI_createUsersWithListInput**
```c
// Creates list of users with given input array
//
void UserAPI_createUsersWithListInput(apiClient_t *apiClient, list_t * body);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**body** | **[list_t](user.md) \*** | List of user object | 

### Return type

void

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **UserAPI_deleteUser**
```c
// Delete user
//
// This can only be done by the logged in user.
//
void UserAPI_deleteUser(apiClient_t *apiClient, char * username);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**username** | **char \*** | The name that needs to be deleted | 

### Return type

void

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **UserAPI_getUserByName**
```c
// Get user by user name
//
user_t* UserAPI_getUserByName(apiClient_t *apiClient, char * username);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**username** | **char \*** | The name that needs to be fetched. Use user1 for testing. | 

### Return type

[user_t](user.md) *


### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **UserAPI_loginUser**
```c
// Logs user into the system
//
char* UserAPI_loginUser(apiClient_t *apiClient, char * username, char * password);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**username** | **char \*** | The user name for login | 
**password** | **char \*** | The password for login in clear text | 

### Return type

char*



### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **UserAPI_logoutUser**
```c
// Logs out current logged in user session
//
void UserAPI_logoutUser(apiClient_t *apiClient);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |

### Return type

void

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **UserAPI_updateUser**
```c
// Updated user
//
// This can only be done by the logged in user.
//
void UserAPI_updateUser(apiClient_t *apiClient, char * username, user_t * body);
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**apiClient** | **apiClient_t \*** | context containing the client configuration |
**username** | **char \*** | name that need to be deleted | 
**body** | **[user_t](user.md) \*** | Updated user object | 

### Return type

void

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

