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
> create_user(_api::UserApi, user::User; _mediaType=nothing) -> Nothing, OpenAPI.Clients.ApiResponse <br/>
> create_user(_api::UserApi, response_stream::Channel, user::User; _mediaType=nothing) -> Channel{ Nothing }, OpenAPI.Clients.ApiResponse

Create user

This can only be done by the logged in user.

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **UserApi** | API context | 
**user** | [**User**](User.md)| Created user object | 

### Return type

Nothing

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **create_users_with_array_input**
> create_users_with_array_input(_api::UserApi, user::Vector{User}; _mediaType=nothing) -> Nothing, OpenAPI.Clients.ApiResponse <br/>
> create_users_with_array_input(_api::UserApi, response_stream::Channel, user::Vector{User}; _mediaType=nothing) -> Channel{ Nothing }, OpenAPI.Clients.ApiResponse

Creates list of users with given input array



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **UserApi** | API context | 
**user** | [**Vector{User}**](User.md)| List of user object | 

### Return type

Nothing

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **create_users_with_list_input**
> create_users_with_list_input(_api::UserApi, user::Vector{User}; _mediaType=nothing) -> Nothing, OpenAPI.Clients.ApiResponse <br/>
> create_users_with_list_input(_api::UserApi, response_stream::Channel, user::Vector{User}; _mediaType=nothing) -> Channel{ Nothing }, OpenAPI.Clients.ApiResponse

Creates list of users with given input array



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **UserApi** | API context | 
**user** | [**Vector{User}**](User.md)| List of user object | 

### Return type

Nothing

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **delete_user**
> delete_user(_api::UserApi, username::String; _mediaType=nothing) -> Nothing, OpenAPI.Clients.ApiResponse <br/>
> delete_user(_api::UserApi, response_stream::Channel, username::String; _mediaType=nothing) -> Channel{ Nothing }, OpenAPI.Clients.ApiResponse

Delete user

This can only be done by the logged in user.

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **UserApi** | API context | 
**username** | **String**| The name that needs to be deleted | [default to nothing]

### Return type

Nothing

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **get_user_by_name**
> get_user_by_name(_api::UserApi, username::String; _mediaType=nothing) -> User, OpenAPI.Clients.ApiResponse <br/>
> get_user_by_name(_api::UserApi, response_stream::Channel, username::String; _mediaType=nothing) -> Channel{ User }, OpenAPI.Clients.ApiResponse

Get user by user name



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **UserApi** | API context | 
**username** | **String**| The name that needs to be fetched. Use user1 for testing. | [default to nothing]

### Return type

[**User**](User.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **login_user**
> login_user(_api::UserApi, username::String, password::String; _mediaType=nothing) -> String, OpenAPI.Clients.ApiResponse <br/>
> login_user(_api::UserApi, response_stream::Channel, username::String, password::String; _mediaType=nothing) -> Channel{ String }, OpenAPI.Clients.ApiResponse

Logs user into the system



### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **UserApi** | API context | 
**username** | **String**| The user name for login | [default to nothing]
**password** | **String**| The password for login in clear text | [default to nothing]

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **logout_user**
> logout_user(_api::UserApi; _mediaType=nothing) -> Nothing, OpenAPI.Clients.ApiResponse <br/>
> logout_user(_api::UserApi, response_stream::Channel; _mediaType=nothing) -> Channel{ Nothing }, OpenAPI.Clients.ApiResponse

Logs out current logged in user session



### Required Parameters
This endpoint does not need any parameter.

### Return type

Nothing

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

# **update_user**
> update_user(_api::UserApi, username::String, user::User; _mediaType=nothing) -> Nothing, OpenAPI.Clients.ApiResponse <br/>
> update_user(_api::UserApi, response_stream::Channel, username::String, user::User; _mediaType=nothing) -> Channel{ Nothing }, OpenAPI.Clients.ApiResponse

Updated user

This can only be done by the logged in user.

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_api** | **UserApi** | API context | 
**username** | **String**| name that need to be deleted | [default to nothing]
**user** | [**User**](User.md)| Updated user object | 

### Return type

Nothing

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

