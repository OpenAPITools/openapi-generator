# PSOpenAPITools.PSOpenAPITools/API.UserApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**CreateUser**](UserApi.md#createuser) | **POST** /user | Create user
[**CreateUsersWithArrayInput**](UserApi.md#createuserswitharrayinput) | **POST** /user/createWithArray | Creates list of users with given input array
[**CreateUsersWithListInput**](UserApi.md#createuserswithlistinput) | **POST** /user/createWithList | Creates list of users with given input array
[**DeleteUser**](UserApi.md#deleteuser) | **DELETE** /user/{username} | Delete user
[**GetUserByName**](UserApi.md#getuserbyname) | **GET** /user/{username} | Get user by user name
[**LoginUser**](UserApi.md#loginuser) | **GET** /user/login | Logs user into the system
[**LogoutUser**](UserApi.md#logoutuser) | **GET** /user/logout | Logs out current logged in user session
[**UpdateUser**](UserApi.md#updateuser) | **PUT** /user/{username} | Updated user


<a id="createuser"></a>
# **Invoker-PSOpenAPIToolsCreateUser**
> void Invoker-PSOpenAPIToolsCreateUser
    -body <User>

Create user

This can only be done by the logged in user.

### Example
```powershell
Import-Module -Name PSOpenAPITools

$body = (New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123) # User | Created user object

# Create user
Invoker-PSOpenAPIToolsCreateUser -body $body
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**User**](User.md)| Created user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="createuserswitharrayinput"></a>
# **Invoker-PSOpenAPIToolsCreateUsersWithArrayInput**
> void Invoker-PSOpenAPIToolsCreateUsersWithArrayInput
    -body <User[]>

Creates list of users with given input array

### Example
```powershell
Import-Module -Name PSOpenAPITools

$body = @((New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123)) # User[] | List of user object

# Creates list of users with given input array
Invoker-PSOpenAPIToolsCreateUsersWithArrayInput -body $body
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**User[]**](User.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="createuserswithlistinput"></a>
# **Invoker-PSOpenAPIToolsCreateUsersWithListInput**
> void Invoker-PSOpenAPIToolsCreateUsersWithListInput
    -body <User[]>

Creates list of users with given input array

### Example
```powershell
Import-Module -Name PSOpenAPITools

$body = @((New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123)) # User[] | List of user object

# Creates list of users with given input array
Invoker-PSOpenAPIToolsCreateUsersWithListInput -body $body
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**User[]**](User.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="deleteuser"></a>
# **Invoker-PSOpenAPIToolsDeleteUser**
> void Invoker-PSOpenAPIToolsDeleteUser
    -username <String>

Delete user

This can only be done by the logged in user.

### Example
```powershell
Import-Module -Name PSOpenAPITools

$username = "username_example" # String | The name that needs to be deleted (default to null)

# Delete user
Invoker-PSOpenAPIToolsDeleteUser -username $username
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The name that needs to be deleted | [default to null]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="getuserbyname"></a>
# **Invoker-PSOpenAPIToolsGetUserByName**
> User Invoker-PSOpenAPIToolsGetUserByName
    -username <String>

Get user by user name

### Example
```powershell
Import-Module -Name PSOpenAPITools

$username = "username_example" # String | The name that needs to be fetched. Use user1 for testing. (default to null)

# Get user by user name
User $Result = Invoker-PSOpenAPIToolsGetUserByName -username $username
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The name that needs to be fetched. Use user1 for testing. | [default to null]

### Return type

[**User**](User.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="loginuser"></a>
# **Invoker-PSOpenAPIToolsLoginUser**
> String Invoker-PSOpenAPIToolsLoginUser
    -username <String>
    -password <String>

Logs user into the system

### Example
```powershell
Import-Module -Name PSOpenAPITools

$username = "username_example" # String | The user name for login (default to null)
$password = "password_example" # String | The password for login in clear text (default to null)

# Logs user into the system
String $Result = Invoker-PSOpenAPIToolsLoginUser -username $username -password $password
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The user name for login | [default to null]
 **password** | **String**| The password for login in clear text | [default to null]

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="logoutuser"></a>
# **Invoker-PSOpenAPIToolsLogoutUser**
> void Invoker-PSOpenAPIToolsLogoutUser

Logs out current logged in user session

### Example
```powershell
Import-Module -Name PSOpenAPITools


# Logs out current logged in user session
Invoker-PSOpenAPIToolsLogoutUser
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

<a id="updateuser"></a>
# **Invoker-PSOpenAPIToolsUpdateUser**
> void Invoker-PSOpenAPIToolsUpdateUser
    -username <String>
    -body <User>

Updated user

This can only be done by the logged in user.

### Example
```powershell
Import-Module -Name PSOpenAPITools

$username = "username_example" # String | name that need to be deleted (default to null)
$body = (New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123) # User | Updated user object

# Updated user
Invoker-PSOpenAPIToolsUpdateUser -username $username -body $body
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| name that need to be deleted | [default to null]
 **body** | [**User**](User.md)| Updated user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

