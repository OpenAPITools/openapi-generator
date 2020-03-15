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


<a id="invoke-createuser"></a>
# ****
> void Invoke-CreateUser
    -Body <User>

Create user

This can only be done by the logged in user.

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Body = (New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123) # User | Created user object

# Create user
Invoke-CreateUser -Body $Body
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Body** | [**User**](User.md)| Created user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="invoke-createuserswitharrayinput"></a>
# ****
> void Invoke-CreateUsersWithArrayInput
    -Body <User[]>

Creates list of users with given input array

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Body = @((New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123)) # User[] | List of user object

# Creates list of users with given input array
Invoke-CreateUsersWithArrayInput -Body $Body
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Body** | [**User[]**](User.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="invoke-createuserswithlistinput"></a>
# ****
> void Invoke-CreateUsersWithListInput
    -Body <User[]>

Creates list of users with given input array

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Body = @((New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123)) # User[] | List of user object

# Creates list of users with given input array
Invoke-CreateUsersWithListInput -Body $Body
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Body** | [**User[]**](User.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="invoke-deleteuser"></a>
# ****
> void Invoke-DeleteUser
    -Username <String>

Delete user

This can only be done by the logged in user.

### Example
```powershell
Import-Module -Name PSOpenAPITools

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

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a id="get-userbyname"></a>
# ****
> User Get-UserByName
    -Username <String>

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

<a id="invoke-loginuser"></a>
# ****
> String Invoke-LoginUser
    -Username <String>
    -Password <String>

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

<a id="invoke-logoutuser"></a>
# ****
> void Invoke-LogoutUser

Logs out current logged in user session

### Example
```powershell
Import-Module -Name PSOpenAPITools


# Logs out current logged in user session
Invoke-LogoutUser
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

<a id="update-user"></a>
# ****
> void Update-User
    -Username <String>
    -Body <User>

Updated user

This can only be done by the logged in user.

### Example
```powershell
Import-Module -Name PSOpenAPITools

$Username = "Username_example" # String | name that need to be deleted (default to null)
$Body = (New-User -Id 123  -Username "Username_example"  -FirstName "FirstName_example"  -LastName "LastName_example"  -Email "Email_example"  -Password "Password_example"  -Phone "Phone_example"  -UserStatus 123) # User | Updated user object

# Updated user
Update-User -Username $Username -Body $Body
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **Username** | **String**| name that need to be deleted | [default to null]
 **Body** | [**User**](User.md)| Updated user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

