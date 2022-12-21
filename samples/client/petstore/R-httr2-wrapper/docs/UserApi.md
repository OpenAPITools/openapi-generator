# UserApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**create_user**](UserApi.md#create_user) | **POST** /user | Create user
[**create_users_with_array_input**](UserApi.md#create_users_with_array_input) | **POST** /user/createWithArray | Creates list of users with given input array
[**create_users_with_list_input**](UserApi.md#create_users_with_list_input) | **POST** /user/createWithList | Creates list of users with given input array
[**delete_user**](UserApi.md#delete_user) | **DELETE** /user/{username} | Delete user
[**get_user_by_name**](UserApi.md#get_user_by_name) | **GET** /user/{username} | Get user by user name
[**login_user**](UserApi.md#login_user) | **GET** /user/login | Logs user into the system
[**logout_user**](UserApi.md#logout_user) | **GET** /user/logout | Logs out current logged in user session
[**update_user**](UserApi.md#update_user) | **PUT** /user/{username} | Updated user


# **create_user**
> create_user(user)

Create user

This can only be done by the logged in user.

### Example
```R
library(petstore)

# Create user
#
# prepare function argument(s)
var_user <- User$new(123, "username_example", "firstName_example", "lastName_example", "email_example", "password_example", "phone_example", 123) # User | Created user object

api_instance <- petstore_api$new()
# Configure API key authorization: api_key
api_instance$api_client$api_keys["api_key"] <- Sys.getenv("API_KEY")
result <- tryCatch(
             api_instance$user_api$create_user(var_user),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `create_user`:")
  dput(result$ApiException$toString())
  # error object
  dput(result$ApiException$error_object$toJSONString())
}
# This endpoint doesn't return data
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

# **create_users_with_array_input**
> create_users_with_array_input(user)

Creates list of users with given input array



### Example
```R
library(petstore)

# Creates list of users with given input array
#
# prepare function argument(s)
var_user <- c(User$new(123, "username_example", "firstName_example", "lastName_example", "email_example", "password_example", "phone_example", 123)) # array[User] | List of user object

api_instance <- petstore_api$new()
# Configure API key authorization: api_key
api_instance$api_client$api_keys["api_key"] <- Sys.getenv("API_KEY")
result <- tryCatch(
             api_instance$user_api$create_users_with_array_input(var_user),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `create_users_with_array_input`:")
  dput(result$ApiException$toString())
  # error object
  dput(result$ApiException$error_object$toJSONString())
}
# This endpoint doesn't return data
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

# **create_users_with_list_input**
> create_users_with_list_input(user)

Creates list of users with given input array



### Example
```R
library(petstore)

# Creates list of users with given input array
#
# prepare function argument(s)
var_user <- c(User$new(123, "username_example", "firstName_example", "lastName_example", "email_example", "password_example", "phone_example", 123)) # array[User] | List of user object

api_instance <- petstore_api$new()
# Configure API key authorization: api_key
api_instance$api_client$api_keys["api_key"] <- Sys.getenv("API_KEY")
result <- tryCatch(
             api_instance$user_api$create_users_with_list_input(var_user),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `create_users_with_list_input`:")
  dput(result$ApiException$toString())
  # error object
  dput(result$ApiException$error_object$toJSONString())
}
# This endpoint doesn't return data
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

# **delete_user**
> delete_user(username)

Delete user

This can only be done by the logged in user.

### Example
```R
library(petstore)

# Delete user
#
# prepare function argument(s)
var_username <- "username_example" # character | The name that needs to be deleted

api_instance <- petstore_api$new()
# Configure API key authorization: api_key
api_instance$api_client$api_keys["api_key"] <- Sys.getenv("API_KEY")
result <- tryCatch(
             api_instance$user_api$delete_user(var_username),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `delete_user`:")
  dput(result$ApiException$toString())
  # error object
  dput(result$ApiException$error_object$toJSONString())
}
# This endpoint doesn't return data
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

# **get_user_by_name**
> User get_user_by_name(username)

Get user by user name



### Example
```R
library(petstore)

# Get user by user name
#
# prepare function argument(s)
var_username <- "username_example" # character | The name that needs to be fetched. Use user1 for testing.

api_instance <- petstore_api$new()
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$user_api$get_user_by_name(var_username, data_file = "result.txt"),
             api_instance$user_api$get_user_by_name(var_username),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `get_user_by_name`:")
  dput(result$ApiException$toString())
  # error object
  dput(result$ApiException$error_object$toJSONString())
} else {
  # deserialized response object
  print("The response is ...")
  dput(result$toString())
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

# **login_user**
> character login_user(username, password)

Logs user into the system



### Example
```R
library(petstore)

# Logs user into the system
#
# prepare function argument(s)
var_username <- "username_example" # character | The user name for login
var_password <- "password_example" # character | The password for login in clear text

api_instance <- petstore_api$new()
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$user_api$login_user(var_username, var_password, data_file = "result.txt"),
             api_instance$user_api$login_user(var_username, var_password),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `login_user`:")
  dput(result$ApiException$toString())
  # error object
  dput(result$ApiException$error_object$toJSONString())
} else {
  # deserialized response object
  print("The response is ...")
  dput(result$toString())
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

# **logout_user**
> logout_user()

Logs out current logged in user session



### Example
```R
library(petstore)

# Logs out current logged in user session
#

api_instance <- petstore_api$new()
# Configure API key authorization: api_key
api_instance$api_client$api_keys["api_key"] <- Sys.getenv("API_KEY")
result <- tryCatch(
             api_instance$user_api$logout_user(),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `logout_user`:")
  dput(result$ApiException$toString())
  # error object
  dput(result$ApiException$error_object$toJSONString())
}
# This endpoint doesn't return data
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

# **update_user**
> update_user(username, user)

Updated user

This can only be done by the logged in user.

### Example
```R
library(petstore)

# Updated user
#
# prepare function argument(s)
var_username <- "username_example" # character | name that need to be deleted
var_user <- User$new(123, "username_example", "firstName_example", "lastName_example", "email_example", "password_example", "phone_example", 123) # User | Updated user object

api_instance <- petstore_api$new()
# Configure API key authorization: api_key
api_instance$api_client$api_keys["api_key"] <- Sys.getenv("API_KEY")
result <- tryCatch(
             api_instance$user_api$update_user(var_username, var_user),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `update_user`:")
  dput(result$ApiException$toString())
  # error object
  dput(result$ApiException$error_object$toJSONString())
}
# This endpoint doesn't return data
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

