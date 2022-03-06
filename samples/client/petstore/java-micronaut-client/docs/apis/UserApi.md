# UserApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createUser**](UserApi.md#createUser) | **POST** /user | Create user
[**createUsersWithArrayInput**](UserApi.md#createUsersWithArrayInput) | **POST** /user/createWithArray | Creates list of users with given input array
[**createUsersWithListInput**](UserApi.md#createUsersWithListInput) | **POST** /user/createWithList | Creates list of users with given input array
[**deleteUser**](UserApi.md#deleteUser) | **DELETE** /user/{username} | Delete user
[**getUserByName**](UserApi.md#getUserByName) | **GET** /user/{username} | Get user by user name
[**loginUser**](UserApi.md#loginUser) | **GET** /user/login | Logs user into the system
[**logoutUser**](UserApi.md#logoutUser) | **GET** /user/logout | Logs out current logged in user session
[**updateUser**](UserApi.md#updateUser) | **PUT** /user/{username} | Updated user


## Creating UserApi

To initiate an instance of `UserApi`, you can use micronaut's `ApplicationContext`:
```java
/* imports
import io.micronaut.runtime.Micronaut;
import io.micronaut.context.ApplicationContext;
import org.openapitools.api.UserApi;
*/

ApplicationContext context = Micronaut.run(/* ... */);
PetApi apiInstance = context.getBean(UserApi.class);
```

Or the `@Inject` annotation:
```java
@Singleton
class MyClass {
    @Inject
    UserApi userApi;

    /* ... use the injected variable */
}
```
Note that the class needs to be annotated with one of Micronaut's [scope annotations](https://docs.micronaut.io/latest/guide/#scopes) like `Singleton` in order to be processed.

More information can be found inside [Inversion of Control guide section](https://docs.micronaut.io/latest/guide/#ioc).

<a name="createUser"></a>
# **createUser**
```java
Mono<Void> UserApi.createUser(_body)
```

Create user

This can only be done by the logged in user.

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_body** | [**User**](User.md)| Created user object |






### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="createUsersWithArrayInput"></a>
# **createUsersWithArrayInput**
```java
Mono<Void> UserApi.createUsersWithArrayInput(_body)
```

Creates list of users with given input array

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_body** | [**List&lt;User&gt;**](User.md)| List of user object |






### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="createUsersWithListInput"></a>
# **createUsersWithListInput**
```java
Mono<Void> UserApi.createUsersWithListInput(_body)
```

Creates list of users with given input array

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_body** | [**List&lt;User&gt;**](User.md)| List of user object |






### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="deleteUser"></a>
# **deleteUser**
```java
Mono<Void> UserApi.deleteUser(username)
```

Delete user

This can only be done by the logged in user.

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | `String`| The name that needs to be deleted |






### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="getUserByName"></a>
# **getUserByName**
```java
Mono<User> UserApi.getUserByName(username)
```

Get user by user name

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | `String`| The name that needs to be fetched. Use user1 for testing. |


### Return type
[**User**](User.md)



### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: `application/xml`, `application/json`

<a name="loginUser"></a>
# **loginUser**
```java
Mono<String> UserApi.loginUser(usernamepassword)
```

Logs user into the system

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | `String`| The user name for login |
 **password** | `String`| The password for login in clear text |


### Return type
`String`



### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: `application/xml`, `application/json`

<a name="logoutUser"></a>
# **logoutUser**
```java
Mono<Void> UserApi.logoutUser()
```

Logs out current logged in user session







### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="updateUser"></a>
# **updateUser**
```java
Mono<Void> UserApi.updateUser(username_body)
```

Updated user

This can only be done by the logged in user.

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | `String`| name that need to be deleted |
 **_body** | [**User**](User.md)| Updated user object |






### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: Not defined

