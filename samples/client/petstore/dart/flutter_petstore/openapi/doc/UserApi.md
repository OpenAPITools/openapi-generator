# openapi.api.UserApi

## Load the API package
```dart
import 'package:openapi/api.dart';
```

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createUser**](UserApi.md#createUser) | **POST** /user | Create user
[**createUsersWithArrayInput**](UserApi.md#createUsersWithArrayInput) | **POST** /user/createWithArray | Creates list of users with given input array
[**createUsersWithListInput**](UserApi.md#createUsersWithListInput) | **POST** /user/createWithList | Creates list of users with given input array
[**deleteUser**](UserApi.md#deleteUser) | **DELETE** /user/{username} | Delete user
[**getUserByName**](UserApi.md#getUserByName) | **GET** /user/{username} | Get user by user name
[**loginUser**](UserApi.md#loginUser) | **GET** /user/login | Logs user into the system
[**logoutUser**](UserApi.md#logoutUser) | **GET** /user/logout | Logs out current logged in user session
[**updateUser**](UserApi.md#updateUser) | **PUT** /user/{username} | Updated user


# **createUser**
> createUser(user)

Create user

This can only be done by the logged in user.

### Example 
```dart
import 'package:openapi/api.dart';
// TODO Configure API key authorization: api_key
//openapi.api.Configuration.apiKey{'api_key'} = 'YOUR_API_KEY';
// uncomment below to setup prefix (e.g. Bearer) for API key, if needed
//openapi.api.Configuration.apiKeyPrefix{'api_key'} = "Bearer";

var api_instance = new UserApi();
var user = new User(); // User | Created user object

try { 
    api_instance.createUser(user);
} catch (e) {
    print("Exception when calling UserApi->createUser: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | [**User**](User.md)| Created user object | 

### Return type

void (empty response body)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **createUsersWithArrayInput**
> createUsersWithArrayInput(user)

Creates list of users with given input array

### Example 
```dart
import 'package:openapi/api.dart';
// TODO Configure API key authorization: api_key
//openapi.api.Configuration.apiKey{'api_key'} = 'YOUR_API_KEY';
// uncomment below to setup prefix (e.g. Bearer) for API key, if needed
//openapi.api.Configuration.apiKeyPrefix{'api_key'} = "Bearer";

var api_instance = new UserApi();
var user = [new List&lt;User&gt;()]; // List<User> | List of user object

try { 
    api_instance.createUsersWithArrayInput(user);
} catch (e) {
    print("Exception when calling UserApi->createUsersWithArrayInput: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | [**List&lt;User&gt;**](User.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **createUsersWithListInput**
> createUsersWithListInput(user)

Creates list of users with given input array

### Example 
```dart
import 'package:openapi/api.dart';
// TODO Configure API key authorization: api_key
//openapi.api.Configuration.apiKey{'api_key'} = 'YOUR_API_KEY';
// uncomment below to setup prefix (e.g. Bearer) for API key, if needed
//openapi.api.Configuration.apiKeyPrefix{'api_key'} = "Bearer";

var api_instance = new UserApi();
var user = [new List&lt;User&gt;()]; // List<User> | List of user object

try { 
    api_instance.createUsersWithListInput(user);
} catch (e) {
    print("Exception when calling UserApi->createUsersWithListInput: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | [**List&lt;User&gt;**](User.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **deleteUser**
> deleteUser(username)

Delete user

This can only be done by the logged in user.

### Example 
```dart
import 'package:openapi/api.dart';
// TODO Configure API key authorization: api_key
//openapi.api.Configuration.apiKey{'api_key'} = 'YOUR_API_KEY';
// uncomment below to setup prefix (e.g. Bearer) for API key, if needed
//openapi.api.Configuration.apiKeyPrefix{'api_key'} = "Bearer";

var api_instance = new UserApi();
var username = username_example; // String | The name that needs to be deleted

try { 
    api_instance.deleteUser(username);
} catch (e) {
    print("Exception when calling UserApi->deleteUser: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The name that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getUserByName**
> User getUserByName(username)

Get user by user name

### Example 
```dart
import 'package:openapi/api.dart';

var api_instance = new UserApi();
var username = username_example; // String | The name that needs to be fetched. Use user1 for testing.

try { 
    var result = api_instance.getUserByName(username);
    print(result);
} catch (e) {
    print("Exception when calling UserApi->getUserByName: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The name that needs to be fetched. Use user1 for testing. | 

### Return type

[**User**](User.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **loginUser**
> String loginUser(username, password)

Logs user into the system

### Example 
```dart
import 'package:openapi/api.dart';

var api_instance = new UserApi();
var username = username_example; // String | The user name for login
var password = password_example; // String | The password for login in clear text

try { 
    var result = api_instance.loginUser(username, password);
    print(result);
} catch (e) {
    print("Exception when calling UserApi->loginUser: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The user name for login | 
 **password** | **String**| The password for login in clear text | 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **logoutUser**
> logoutUser()

Logs out current logged in user session

### Example 
```dart
import 'package:openapi/api.dart';
// TODO Configure API key authorization: api_key
//openapi.api.Configuration.apiKey{'api_key'} = 'YOUR_API_KEY';
// uncomment below to setup prefix (e.g. Bearer) for API key, if needed
//openapi.api.Configuration.apiKeyPrefix{'api_key'} = "Bearer";

var api_instance = new UserApi();

try { 
    api_instance.logoutUser();
} catch (e) {
    print("Exception when calling UserApi->logoutUser: $e\n");
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

void (empty response body)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updateUser**
> updateUser(username, user)

Updated user

This can only be done by the logged in user.

### Example 
```dart
import 'package:openapi/api.dart';
// TODO Configure API key authorization: api_key
//openapi.api.Configuration.apiKey{'api_key'} = 'YOUR_API_KEY';
// uncomment below to setup prefix (e.g. Bearer) for API key, if needed
//openapi.api.Configuration.apiKeyPrefix{'api_key'} = "Bearer";

var api_instance = new UserApi();
var username = username_example; // String | name that need to be deleted
var user = new User(); // User | Updated user object

try { 
    api_instance.updateUser(username, user);
} catch (e) {
    print("Exception when calling UserApi->updateUser: $e\n");
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| name that need to be deleted | 
 **user** | [**User**](User.md)| Updated user object | 

### Return type

void (empty response body)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

