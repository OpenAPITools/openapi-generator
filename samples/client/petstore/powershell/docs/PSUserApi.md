# PSPetstore.PSPetstore/Api.PSUserApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**New-PSUser**](PSUserApi.md#New-PSUser) | **POST** /user | Create user
[**New-PSUsersWithArrayInput**](PSUserApi.md#New-PSUsersWithArrayInput) | **POST** /user/createWithArray | Creates list of users with given input array
[**New-PSUsersWithListInput**](PSUserApi.md#New-PSUsersWithListInput) | **POST** /user/createWithList | Creates list of users with given input array
[**Remove-PSUser**](PSUserApi.md#Remove-PSUser) | **DELETE** /user/{username} | Delete user
[**Get-PSUserByName**](PSUserApi.md#Get-PSUserByName) | **GET** /user/{username} | Get user by user name
[**Invoke-PSLoginUser**](PSUserApi.md#Invoke-PSLoginUser) | **GET** /user/login | Logs user into the system
[**Invoke-PSLogoutUser**](PSUserApi.md#Invoke-PSLogoutUser) | **GET** /user/logout | Logs out current logged in user session
[**Update-PSUser**](PSUserApi.md#Update-PSUser) | **PUT** /user/{username} | Updated user


<a name="New-PSUser"></a>
# **New-PSUser**
> void New-PSUser<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-User] <PSCustomObject><br>

Create user

This can only be done by the logged in user.

### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure API key authorization: auth_cookie
$Configuration.ApiKey.AUTH_KEY = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration.ApiKeyPrefix.AUTH_KEY = "Bearer"

$User = Initialize-User -Id 0 -Username "MyUsername" -FirstName "MyFirstName" -LastName "MyLastName" -Email "MyEmail" -Password "MyPassword" -Phone "MyPhone" -UserStatus 0 # User | Created user object

# Create user
try {
    $Result = New-PSUser -User $User
} catch {
    Write-Host ("Exception occurred when calling New-PSUser: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
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

<a name="New-PSUsersWithArrayInput"></a>
# **New-PSUsersWithArrayInput**
> void New-PSUsersWithArrayInput<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-User] <PSCustomObject[]><br>

Creates list of users with given input array



### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure API key authorization: auth_cookie
$Configuration.ApiKey.AUTH_KEY = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration.ApiKeyPrefix.AUTH_KEY = "Bearer"

$User = Initialize-User -Id 0 -Username "MyUsername" -FirstName "MyFirstName" -LastName "MyLastName" -Email "MyEmail" -Password "MyPassword" -Phone "MyPhone" -UserStatus 0 # User[] | List of user object

# Creates list of users with given input array
try {
    $Result = New-PSUsersWithArrayInput -User $User
} catch {
    Write-Host ("Exception occurred when calling New-PSUsersWithArrayInput: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
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

<a name="New-PSUsersWithListInput"></a>
# **New-PSUsersWithListInput**
> void New-PSUsersWithListInput<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-User] <PSCustomObject[]><br>

Creates list of users with given input array



### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure API key authorization: auth_cookie
$Configuration.ApiKey.AUTH_KEY = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration.ApiKeyPrefix.AUTH_KEY = "Bearer"

$User = Initialize-User -Id 0 -Username "MyUsername" -FirstName "MyFirstName" -LastName "MyLastName" -Email "MyEmail" -Password "MyPassword" -Phone "MyPhone" -UserStatus 0 # User[] | List of user object

# Creates list of users with given input array
try {
    $Result = New-PSUsersWithListInput -User $User
} catch {
    Write-Host ("Exception occurred when calling New-PSUsersWithListInput: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
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

<a name="Remove-PSUser"></a>
# **Remove-PSUser**
> void Remove-PSUser<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[-Username] <String><br>

Delete user

This can only be done by the logged in user.

### Example
```powershell
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure API key authorization: auth_cookie
$Configuration.ApiKey.AUTH_KEY = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration.ApiKeyPrefix.AUTH_KEY = "Bearer"

$Username = "MyUsername" # String | The name that needs to be deleted

# Delete user
try {
    $Result = Remove-PSUser -Username $Username
} catch {
    Write-Host ("Exception occurred when calling Remove-PSUser: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Username** | **String**| The name that needs to be deleted | 

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
$Username = "MyUsername" # String | The name that needs to be fetched. Use user1 for testing.

# Get user by user name
try {
    $Result = Get-PSUserByName -Username $Username
} catch {
    Write-Host ("Exception occurred when calling Get-PSUserByName: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Username** | **String**| The name that needs to be fetched. Use user1 for testing. | 

### Return type

[**User**](User.md) (PSCustomObject)

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
$Username = "MyUsername" # String | The user name for login
$Password = "MyPassword" # String | The password for login in clear text

# Logs user into the system
try {
    $Result = Invoke-PSLoginUser -Username $Username -Password $Password
} catch {
    Write-Host ("Exception occurred when calling Invoke-PSLoginUser: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Username** | **String**| The user name for login | 
 **Password** | **String**| The password for login in clear text | 

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
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure API key authorization: auth_cookie
$Configuration.ApiKey.AUTH_KEY = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration.ApiKeyPrefix.AUTH_KEY = "Bearer"


# Logs out current logged in user session
try {
    $Result = Invoke-PSLogoutUser
} catch {
    Write-Host ("Exception occurred when calling Invoke-PSLogoutUser: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
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
# general setting of the PowerShell module, e.g. base URL, authentication, etc
$Configuration = Get-Configuration
# Configure API key authorization: auth_cookie
$Configuration.ApiKey.AUTH_KEY = "YOUR_API_KEY"
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$Configuration.ApiKeyPrefix.AUTH_KEY = "Bearer"

$Username = "MyUsername" # String | name that need to be deleted
$User = Initialize-User -Id 0 -Username "MyUsername" -FirstName "MyFirstName" -LastName "MyLastName" -Email "MyEmail" -Password "MyPassword" -Phone "MyPhone" -UserStatus 0 # User | Updated user object

# Updated user
try {
    $Result = Update-PSUser -Username $Username -User $User
} catch {
    Write-Host ("Exception occurred when calling Update-PSUser: {0}" -f ($_.ErrorDetails | ConvertFrom-Json))
    Write-Host ("Response headers: {0}" -f ($_.Exception.Response.Headers | ConvertTo-Json))
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Username** | **String**| name that need to be deleted | 
 **User** | [**User**](User.md)| Updated user object | 

### Return type

void (empty response body)

### Authorization

[auth_cookie](../README.md#auth_cookie)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

