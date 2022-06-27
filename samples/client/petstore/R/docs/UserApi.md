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
> CreateUser(user)

Create user

This can only be done by the logged in user.

### Example
```R
library(petstore)

var_user <- User$new(123, "username_example", "firstName_example", "lastName_example", "email_example", "password_example", "phone_example", 123) # User | Created user object

#Create user
api_instance <- UserApi$new()
# Configure API key authorization: api_key
api_instance$api_client$api_keys['api_key'] <- 'TODO_YOUR_API_KEY';
result <- tryCatch(
             api_instance$CreateUser(var_user),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  cat(result$ApiException$toString())
} else {
  # response headers
  response.headers <- result$response$headers
  # response status code
  response.status.code <- result$response$status_code
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

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | successful operation |  -  |

# **CreateUsersWithArrayInput**
> CreateUsersWithArrayInput(user)

Creates list of users with given input array



### Example
```R
library(petstore)

var_user <- list(User$new(123, "username_example", "firstName_example", "lastName_example", "email_example", "password_example", "phone_example", 123)) # array[User] | List of user object

#Creates list of users with given input array
api_instance <- UserApi$new()
# Configure API key authorization: api_key
api_instance$api_client$api_keys['api_key'] <- 'TODO_YOUR_API_KEY';
result <- tryCatch(
             api_instance$CreateUsersWithArrayInput(var_user),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  cat(result$ApiException$toString())
} else {
  # response headers
  response.headers <- result$response$headers
  # response status code
  response.status.code <- result$response$status_code
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | list( [**User**](User.md) )| List of user object | 

### Return type

void (empty response body)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | successful operation |  -  |

# **CreateUsersWithListInput**
> CreateUsersWithListInput(user)

Creates list of users with given input array



### Example
```R
library(petstore)

var_user <- list(User$new(123, "username_example", "firstName_example", "lastName_example", "email_example", "password_example", "phone_example", 123)) # array[User] | List of user object

#Creates list of users with given input array
api_instance <- UserApi$new()
# Configure API key authorization: api_key
api_instance$api_client$api_keys['api_key'] <- 'TODO_YOUR_API_KEY';
result <- tryCatch(
             api_instance$CreateUsersWithListInput(var_user),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  cat(result$ApiException$toString())
} else {
  # response headers
  response.headers <- result$response$headers
  # response status code
  response.status.code <- result$response$status_code
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | list( [**User**](User.md) )| List of user object | 

### Return type

void (empty response body)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
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

var_username <- 'username_example' # character | The name that needs to be deleted

#Delete user
api_instance <- UserApi$new()
# Configure API key authorization: api_key
api_instance$api_client$api_keys['api_key'] <- 'TODO_YOUR_API_KEY';
result <- tryCatch(
             api_instance$DeleteUser(var_username),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  cat(result$ApiException$toString())
} else {
  # response headers
  response.headers <- result$response$headers
  # response status code
  response.status.code <- result$response$status_code
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **character**| The name that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

[api_key](../README.md#api_key)

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

var_username <- 'username_example' # character | The name that needs to be fetched. Use user1 for testing.

#Get user by user name
api_instance <- UserApi$new()
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$GetUserByName(var_username, data_file = "result.txt"),
             api_instance$GetUserByName(var_username),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  cat(result$ApiException$toString())
} else {
  # deserialized response object
  response.object <- result$content
  # response headers
  response.headers <- result$response$headers
  # response status code
  response.status.code <- result$response$status_code
}
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

var_username <- 'username_example' # character | The user name for login
var_password <- 'password_example' # character | The password for login in clear text

#Logs user into the system
api_instance <- UserApi$new()
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$LoginUser(var_username, var_password, data_file = "result.txt"),
             api_instance$LoginUser(var_username, var_password),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  cat(result$ApiException$toString())
} else {
  # deserialized response object
  response.object <- result$content
  # response headers
  response.headers <- result$response$headers
  # response status code
  response.status.code <- result$response$status_code
}
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
| **200** | successful operation |  * Set-Cookie - Cookie authentication key for use with the &#x60;api_key&#x60; apiKey authentication. <br>  * X-Rate-Limit - calls per hour allowed by the user <br>  * X-Expires-After - date in UTC when token expires <br>  |
| **400** | Invalid username/password supplied |  -  |

# **LogoutUser**
> LogoutUser()

Logs out current logged in user session



### Example
```R
library(petstore)


#Logs out current logged in user session
api_instance <- UserApi$new()
# Configure API key authorization: api_key
api_instance$api_client$api_keys['api_key'] <- 'TODO_YOUR_API_KEY';
result <- tryCatch(
             api_instance$LogoutUser(),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  cat(result$ApiException$toString())
} else {
  # response headers
  response.headers <- result$response$headers
  # response status code
  response.status.code <- result$response$status_code
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

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | successful operation |  -  |

# **UpdateUser**
> UpdateUser(username, user)

Updated user

This can only be done by the logged in user.

### Example
```R
library(petstore)

var_username <- 'username_example' # character | name that need to be deleted
var_user <- User$new(123, "username_example", "firstName_example", "lastName_example", "email_example", "password_example", "phone_example", 123) # User | Updated user object

#Updated user
api_instance <- UserApi$new()
# Configure API key authorization: api_key
api_instance$api_client$api_keys['api_key'] <- 'TODO_YOUR_API_KEY';
result <- tryCatch(
             api_instance$UpdateUser(var_username, var_user),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  cat(result$ApiException$toString())
} else {
  # response headers
  response.headers <- result$response$headers
  # response status code
  response.status.code <- result$response$status_code
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **character**| name that need to be deleted | 
 **user** | [**User**](User.md)| Updated user object | 

### Return type

void (empty response body)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid user supplied |  -  |
| **404** | User not found |  -  |

