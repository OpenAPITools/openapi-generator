# PSOpenAPITools.PSOpenAPITools/API.UserApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Invoke-CreateUser**](UserApi.md#invoke-createuser) | **POST** /user | Create user
[**Invoke-CreateUsersWithArrayInput**](UserApi.md#invoke-createuserswitharrayinput) | **POST** /user/createWithArray | Creates list of users with given input array
[**Invoke-CreateUsersWithListInput**](UserApi.md#invoke-createuserswithlistinput) | **POST** /user/createWithList | Creates list of users with given input array
[**Invoke-DeleteUser**](UserApi.md#invoke-deleteuser) | **DELETE** /user/{username} | Delete user
[**Get-UserByName**](UserApi.md#get-userbyname) | **GET** /user/{username} | Get user by user name
[**Invoke-LoginUser**](UserApi.md#invoke-loginuser) | **GET** /user/login | Logs user into the system
[**Invoke-LogoutUser**](UserApi.md#invoke-logoutuser) | **GET** /user/logout | Logs out current logged in user session
[**Update-User**](UserApi.md#update-user) | **PUT** /user/{username} | Updated user


<a name="invoke-createuser"></a>
# **Invoke-CreateUser**
> void Invoke-CreateUser<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-User] <PSCustomObject><br>

Create user

This can only be done by the logged in user.

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure API key authorization: auth_cookie
$Configuration["ApiKey"]["AUTH_KEY"] = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration["ApiKeyPrefix"]["AUTH_KEY"] = "Bearer"

$User = (New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123) # User | Created user object

# Create user
Invoke-CreateUser -User $User
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **User** | [**User**](User.md)| Created user object | 

### Return type

void (empty response body)

### Authorization

[auth_cookie](../README.md#auth_cookie)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="invoke-createuserswitharrayinput"></a>
# **Invoke-CreateUsersWithArrayInput**
> void Invoke-CreateUsersWithArrayInput<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-User] <PSCustomObject[]><br>

Creates list of users with given input array

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure API key authorization: auth_cookie
$Configuration["ApiKey"]["AUTH_KEY"] = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration["ApiKeyPrefix"]["AUTH_KEY"] = "Bearer"

$User = @((New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123)) # User[] | List of user object

# Creates list of users with given input array
Invoke-CreateUsersWithArrayInput -User $User
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **User** | [**User[]**](User.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

[auth_cookie](../README.md#auth_cookie)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="invoke-createuserswithlistinput"></a>
# **Invoke-CreateUsersWithListInput**
> void Invoke-CreateUsersWithListInput<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-User] <PSCustomObject[]><br>

Creates list of users with given input array

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure API key authorization: auth_cookie
$Configuration["ApiKey"]["AUTH_KEY"] = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration["ApiKeyPrefix"]["AUTH_KEY"] = "Bearer"

$User = @((New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123)) # User[] | List of user object

# Creates list of users with given input array
Invoke-CreateUsersWithListInput -User $User
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **User** | [**User[]**](User.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

[auth_cookie](../README.md#auth_cookie)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="invoke-deleteuser"></a>
# **Invoke-DeleteUser**
> void Invoke-DeleteUser<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Username] <String><br>

Delete user

This can only be done by the logged in user.

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure API key authorization: auth_cookie
$Configuration["ApiKey"]["AUTH_KEY"] = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration["ApiKeyPrefix"]["AUTH_KEY"] = "Bearer"

$Username = "Username_example" # String | The name that needs to be deleted (default to null)

# Delete user
Invoke-DeleteUser -Username $Username
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Username** | **String**| The name that needs to be deleted | [default to null]

### Return type

void (empty response body)

### Authorization

[auth_cookie](../README.md#auth_cookie)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="get-userbyname"></a>
# **Get-UserByName**
> User Get-UserByName<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Username] <String><br>

Get user by user name

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Username = "Username_example" # String | The name that needs to be fetched. Use user1 for testing. (default to null)

# Get user by user name
User $Result = Get-UserByName -Username $Username
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Username** | **String**| The name that needs to be fetched. Use user1 for testing. | [default to null]

### Return type

[**User**](User.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="invoke-loginuser"></a>
# **Invoke-LoginUser**
> String Invoke-LoginUser<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Username] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Password] <String><br>

Logs user into the system

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Username = "Username_example" # String | The user name for login (default to null)
$Password = "Password_example" # String | The password for login in clear text (default to null)

# Logs user into the system
String $Result = Invoke-LoginUser -Username $Username -Password $Password
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Username** | **String**| The user name for login | [default to null]
 **Password** | **String**| The password for login in clear text | [default to null]

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="invoke-logoutuser"></a>
# **Invoke-LogoutUser**
> void Invoke-LogoutUser<br>

Logs out current logged in user session

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure API key authorization: auth_cookie
$Configuration["ApiKey"]["AUTH_KEY"] = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration["ApiKeyPrefix"]["AUTH_KEY"] = "Bearer"


# Logs out current logged in user session
Invoke-LogoutUser
```

### Parameters
This endpoint does not need any parameter.

### Return type

void (empty response body)

### Authorization

[auth_cookie](../README.md#auth_cookie)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="update-user"></a>
# **Update-User**
> void Update-User<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Username] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-User] <PSCustomObject><br>

Updated user

This can only be done by the logged in user.

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Configuration = Get-PSOpenAPIToolsConfiguration
# Configure API key authorization: auth_cookie
$Configuration["ApiKey"]["AUTH_KEY"] = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration["ApiKeyPrefix"]["AUTH_KEY"] = "Bearer"

$Username = "Username_example" # String | name that need to be deleted (default to null)
$User = (New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123) # User | Updated user object

# Updated user
Update-User -Username $Username -User $User
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Username** | **String**| name that need to be deleted | [default to null]
 **User** | [**User**](User.md)| Updated user object | 

### Return type

void (empty response body)

### Authorization

[auth_cookie](../README.md#auth_cookie)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

