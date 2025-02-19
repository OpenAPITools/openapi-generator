# user_api

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**CreateUser**](user_api.md#CreateUser) | **POST** /user | Create user
[**CreateUsersWithArrayInput**](user_api.md#CreateUsersWithArrayInput) | **POST** /user/createWithArray | Creates list of users with given input array
[**CreateUsersWithListInput**](user_api.md#CreateUsersWithListInput) | **POST** /user/createWithList | Creates list of users with given input array
[**DeleteUser**](user_api.md#DeleteUser) | **DELETE** /user/{username} | Delete user
[**GetUserByName**](user_api.md#GetUserByName) | **GET** /user/{username} | Get user by user name
[**LoginUser**](user_api.md#LoginUser) | **GET** /user/login | Logs user into the system
[**LogoutUser**](user_api.md#LogoutUser) | **GET** /user/logout | Logs out current logged in user session
[**UpdateUser**](user_api.md#UpdateUser) | **PUT** /user/{username} | Updated user


<a name="CreateUser"></a>
# **CreateUser**
> CreateUser(body)

Create user

This can only be done by the logged in user.
<a name="CreateUsersWithArrayInput"></a>
# **CreateUsersWithArrayInput**
> CreateUsersWithArrayInput(body)

Creates list of users with given input array
<a name="CreateUsersWithListInput"></a>
# **CreateUsersWithListInput**
> CreateUsersWithListInput(body)

Creates list of users with given input array
<a name="DeleteUser"></a>
# **DeleteUser**
> DeleteUser(username)

Delete user

This can only be done by the logged in user.
<a name="GetUserByName"></a>
# **GetUserByName**
> User GetUserByName(username)

Get user by user name
<a name="LoginUser"></a>
# **LoginUser**
> String! LoginUser(username, password)

Logs user into the system
<a name="LogoutUser"></a>
# **LogoutUser**
> LogoutUser()

Logs out current logged in user session
<a name="UpdateUser"></a>
# **UpdateUser**
> UpdateUser(username, body)

Updated user

This can only be done by the logged in user.
