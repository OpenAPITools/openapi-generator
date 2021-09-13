# UserApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**CreateUser**](UserApi.md#CreateUser) | **POST** /user | Create user
[**CreateUsersWithArrayInput**](UserApi.md#CreateUsersWithArrayInput) | **POST** /user/createWithArray | Creates list of users with given input array
[**CreateUsersWithListInput**](UserApi.md#CreateUsersWithListInput) | **POST** /user/createWithList | Creates list of users with given input array
[**DeleteUser**](UserApi.md#DeleteUser) | **DELETE** /user/{username} | Delete user
[**GetUserByName**](UserApi.md#GetUserByName) | **GET** /user/{username} | Get user by user name
[**LoginUser**](UserApi.md#LoginUser) | **GET** /user/login | Logs user into the system
[**LogoutUser**](UserApi.md#LogoutUser) | **GET** /user/logout | Logs out current logged in user session
[**UpdateUser**](UserApi.md#UpdateUser) | **PUT** /user/{username} | Updated user


# **CreateUser**
> CreateUser(body)

Create user

This can only be done by the logged in user.

### Example
```R
library(petstore)

var.body <- User$new(123, "username_example", "firstName_example", "lastName_example", "email_example", "password_example", "phone_example", 123) # User | Created user object

#Create user
api.instance <- UserApi$new()
api.instance$CreateUser(var.body)
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

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | successful operation |  -  |

# **CreateUsersWithArrayInput**
> CreateUsersWithArrayInput(body)

Creates list of users with given input array

### Example
```R
library(petstore)

var.body <- list(User$new(123, "username_example", "firstName_example", "lastName_example", "email_example", "password_example", "phone_example", 123)) # array[User] | List of user object

#Creates list of users with given input array
api.instance <- UserApi$new()
api.instance$CreateUsersWithArrayInput(var.body)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | list( [**User**](User.md) )| List of user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | successful operation |  -  |

# **CreateUsersWithListInput**
> CreateUsersWithListInput(body)

Creates list of users with given input array

### Example
```R
library(petstore)

var.body <- list(User$new(123, "username_example", "firstName_example", "lastName_example", "email_example", "password_example", "phone_example", 123)) # array[User] | List of user object

#Creates list of users with given input array
api.instance <- UserApi$new()
api.instance$CreateUsersWithListInput(var.body)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | list( [**User**](User.md) )| List of user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | successful operation |  -  |

# **DeleteUser**
> DeleteUser(username)

Delete user

This can only be done by the logged in user.

### Example
```R
library(petstore)

var.username <- 'username_example' # character | The name that needs to be deleted

#Delete user
api.instance <- UserApi$new()
api.instance$DeleteUser(var.username)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **character**| The name that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid username supplied |  -  |
| **404** | User not found |  -  |

# **GetUserByName**
> User GetUserByName(username)

Get user by user name

### Example
```R
library(petstore)

var.username <- 'username_example' # character | The name that needs to be fetched. Use user1 for testing.

#Get user by user name
api.instance <- UserApi$new()
result <- api.instance$GetUserByName(var.username)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **character**| The name that needs to be fetched. Use user1 for testing. | 

### Return type

[**User**](User.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid username supplied |  -  |
| **404** | User not found |  -  |

# **LoginUser**
> character LoginUser(username, password)

Logs user into the system

### Example
```R
library(petstore)

var.username <- 'username_example' # character | The user name for login
var.password <- 'password_example' # character | The password for login in clear text

#Logs user into the system
api.instance <- UserApi$new()
result <- api.instance$LoginUser(var.username, var.password)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **character**| The user name for login | 
 **password** | **character**| The password for login in clear text | 

### Return type

**character**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  * X-Rate-Limit - calls per hour allowed by the user <br>  * X-Expires-After - date in UTC when token expires <br>  |
| **400** | Invalid username/password supplied |  -  |

# **LogoutUser**
> LogoutUser()

Logs out current logged in user session

### Example
```R
library(petstore)


#Logs out current logged in user session
api.instance <- UserApi$new()
api.instance$LogoutUser()
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

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | successful operation |  -  |

# **UpdateUser**
> UpdateUser(username, body)

Updated user

This can only be done by the logged in user.

### Example
```R
library(petstore)

var.username <- 'username_example' # character | name that need to be deleted
var.body <- User$new(123, "username_example", "firstName_example", "lastName_example", "email_example", "password_example", "phone_example", 123) # User | Updated user object

#Updated user
api.instance <- UserApi$new()
api.instance$UpdateUser(var.username, var.body)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **character**| name that need to be deleted | 
 **body** | [**User**](User.md)| Updated user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid user supplied |  -  |
| **404** | User not found |  -  |

