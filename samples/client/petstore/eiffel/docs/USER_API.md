# USER_API

All URIs are relative to *http://petstore.swagger.io:80/v2*

Feature | HTTP request | Description
------------- | ------------- | -------------
[**create_user**](USER_API.md#create_user) | **Post** /user | Create user
[**create_users_with_array_input**](USER_API.md#create_users_with_array_input) | **Post** /user/createWithArray | Creates list of users with given input array
[**create_users_with_list_input**](USER_API.md#create_users_with_list_input) | **Post** /user/createWithList | Creates list of users with given input array
[**delete_user**](USER_API.md#delete_user) | **Delete** /user/{username} | Delete user
[**login_user**](USER_API.md#login_user) | **Get** /user/login | Logs user into the system
[**logout_user**](USER_API.md#logout_user) | **Get** /user/logout | Logs out current logged in user session
[**update_user**](USER_API.md#update_user) | **Put** /user/{username} | Updated user
[**user_by_name**](USER_API.md#user_by_name) | **Get** /user/{username} | Get user by user name


# **create_user**
> create_user (body: USER )
	

Create user

This can only be done by the logged in user.


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**USER**](USER.md)| Created user object | 

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **create_users_with_array_input**
> create_users_with_array_input (body: LIST [USER] )
	

Creates list of users with given input array




### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**LIST [USER]**](User.md)| List of user object | 

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **create_users_with_list_input**
> create_users_with_list_input (body: LIST [USER] )
	

Creates list of users with given input array




### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**LIST [USER]**](User.md)| List of user object | 

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_user**
> delete_user (username: STRING_32 )
	

Delete user

This can only be done by the logged in user.


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **STRING_32**| The name that needs to be deleted | 

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **login_user**
> login_user (username: STRING_32 ; password: STRING_32 ): detachable STRING_32
	

Logs user into the system




### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **STRING_32**| The user name for login | 
 **password** | **STRING_32**| The password for login in clear text | 

### Return type

[**STRING_32**](STRING_32.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **logout_user**
> logout_user 
	

Logs out current logged in user session




### Parameters
This endpoint does not need any parameter.

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_user**
> update_user (username: STRING_32 ; body: USER )
	

Updated user

This can only be done by the logged in user.


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **STRING_32**| name that need to be deleted | 
 **body** | [**USER**](USER.md)| Updated user object | 

### Return type

{empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **user_by_name**
> user_by_name (username: STRING_32 ): detachable USER
	

Get user by user name




### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **STRING_32**| The name that needs to be fetched. Use user1 for testing.  | 

### Return type

[**USER**](User.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

