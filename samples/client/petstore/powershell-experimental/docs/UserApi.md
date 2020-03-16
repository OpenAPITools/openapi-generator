# PSPetstore.PSPetstore/API.UserApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**Invoke-PSCreateUser**](UserApi.md#Invoke-PSCreateUser) | **POST** /user | Create user
[**Invoke-PSCreateUsersWithArrayInput**](UserApi.md#Invoke-PSCreateUsersWithArrayInput) | **POST** /user/createWithArray | Creates list of users with given input array
[**Invoke-PSCreateUsersWithListInput**](UserApi.md#Invoke-PSCreateUsersWithListInput) | **POST** /user/createWithList | Creates list of users with given input array
[**Invoke-PSDeleteUser**](UserApi.md#Invoke-PSDeleteUser) | **DELETE** /user/{username} | Delete user
[**Get-PSUserByName**](UserApi.md#Get-PSUserByName) | **GET** /user/{username} | Get user by user name
[**Invoke-PSLoginUser**](UserApi.md#Invoke-PSLoginUser) | **GET** /user/login | Logs user into the system
[**Invoke-PSLogoutUser**](UserApi.md#Invoke-PSLogoutUser) | **GET** /user/logout | Logs out current logged in user session
[**Update-PSUser**](UserApi.md#Update-PSUser) | **PUT** /user/{username} | Updated user


<a name="Invoke-PSCreateUser"></a>
# **Invoke-PSCreateUser**
> void Invoke-PSCreateUser<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-User] <PSCustomObject><br>

Create user

This can only be done by the logged in user.

### Example
```powershell
Import-Module -Name PSPetstore

$Configuration = Get-PSPetstoreConfiguration
# Configure API key authorization: auth_cookie
$Configuration["ApiKey"]["AUTH_KEY"] = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration["ApiKeyPrefix"]["AUTH_KEY"] = "Bearer"

$User = (New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123) # User | Created user object

# Create user
Invoke-PSCreateUser -User $User
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

<a name="Invoke-PSCreateUsersWithArrayInput"></a>
# **Invoke-PSCreateUsersWithArrayInput**
> void Invoke-PSCreateUsersWithArrayInput<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-User] <PSCustomObject[]><br>

Creates list of users with given input array

### Example
```powershell
Import-Module -Name PSPetstore

$Configuration = Get-PSPetstoreConfiguration
# Configure API key authorization: auth_cookie
$Configuration["ApiKey"]["AUTH_KEY"] = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration["ApiKeyPrefix"]["AUTH_KEY"] = "Bearer"

$User = @((New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123)) # User[] | List of user object

# Creates list of users with given input array
Invoke-PSCreateUsersWithArrayInput -User $User
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

<a name="Invoke-PSCreateUsersWithListInput"></a>
# **Invoke-PSCreateUsersWithListInput**
> void Invoke-PSCreateUsersWithListInput<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-User] <PSCustomObject[]><br>

Creates list of users with given input array

### Example
```powershell
Import-Module -Name PSPetstore

$Configuration = Get-PSPetstoreConfiguration
# Configure API key authorization: auth_cookie
$Configuration["ApiKey"]["AUTH_KEY"] = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration["ApiKeyPrefix"]["AUTH_KEY"] = "Bearer"

$User = @((New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123)) # User[] | List of user object

# Creates list of users with given input array
Invoke-PSCreateUsersWithListInput -User $User
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

<a name="Invoke-PSDeleteUser"></a>
# **Invoke-PSDeleteUser**
> void Invoke-PSDeleteUser<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Username] <String><br>

Delete user

This can only be done by the logged in user.

### Example
```powershell
Import-Module -Name PSPetstore

$Configuration = Get-PSPetstoreConfiguration
# Configure API key authorization: auth_cookie
$Configuration["ApiKey"]["AUTH_KEY"] = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration["ApiKeyPrefix"]["AUTH_KEY"] = "Bearer"

$Username = "Username_example" # String | The name that needs to be deleted (default to null)

# Delete user
Invoke-PSDeleteUser -Username $Username
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

<a name="Get-PSUserByName"></a>
# **Get-PSUserByName**
> User Get-PSUserByName<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Username] <String><br>

Get user by user name

### Example
```powershell
Import-Module -Name PSPetstore

$Username = "Username_example" # String | The name that needs to be fetched. Use user1 for testing. (default to null)

# Get user by user name
User $Result = Get-PSUserByName -Username $Username
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

<a name="Invoke-PSLoginUser"></a>
# **Invoke-PSLoginUser**
> String Invoke-PSLoginUser<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Username] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Password] <String><br>

Logs user into the system

### Example
```powershell
Import-Module -Name PSPetstore

$Username = "Username_example" # String | The user name for login (default to null)
$Password = "Password_example" # String | The password for login in clear text (default to null)

# Logs user into the system
String $Result = Invoke-PSLoginUser -Username $Username -Password $Password
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

<a name="Invoke-PSLogoutUser"></a>
# **Invoke-PSLogoutUser**
> void Invoke-PSLogoutUser<br>

Logs out current logged in user session

### Example
```powershell
Import-Module -Name PSPetstore

$Configuration = Get-PSPetstoreConfiguration
# Configure API key authorization: auth_cookie
$Configuration["ApiKey"]["AUTH_KEY"] = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration["ApiKeyPrefix"]["AUTH_KEY"] = "Bearer"


# Logs out current logged in user session
Invoke-PSLogoutUser
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

<a name="Update-PSUser"></a>
# **Update-PSUser**
> void Update-PSUser<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Username] <String><br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-User] <PSCustomObject><br>

Updated user

This can only be done by the logged in user.

### Example
```powershell
Import-Module -Name PSPetstore

$Configuration = Get-PSPetstoreConfiguration
# Configure API key authorization: auth_cookie
$Configuration["ApiKey"]["AUTH_KEY"] = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration["ApiKeyPrefix"]["AUTH_KEY"] = "Bearer"

$Username = "Username_example" # String | name that need to be deleted (default to null)
$User = (New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123) # User | Updated user object

# Updated user
Update-PSUser -Username $Username -User $User
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

