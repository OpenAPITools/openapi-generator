# SwagUserApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createUser**](SwagUserApi.md#createUser) | **POST** /user | Create user
[**createUsersWithArrayInput**](SwagUserApi.md#createUsersWithArrayInput) | **POST** /user/createWithArray | Creates list of users with given input array
[**createUsersWithListInput**](SwagUserApi.md#createUsersWithListInput) | **POST** /user/createWithList | Creates list of users with given input array
[**deleteUser**](SwagUserApi.md#deleteUser) | **DELETE** /user/{username} | Delete user
[**getUserByName**](SwagUserApi.md#getUserByName) | **GET** /user/{username} | Get user by user name
[**loginUser**](SwagUserApi.md#loginUser) | **GET** /user/login | Logs user into the system
[**logoutUser**](SwagUserApi.md#logoutUser) | **GET** /user/logout | Logs out current logged in user session
[**updateUser**](SwagUserApi.md#updateUser) | **PUT** /user/{username} | Updated user


<a name="createUser"></a>
# **createUser**
> createUser(body)

Create user

This can only be done by the logged in user.

### Example
```java
SwagUserApi api = new SwagUserApi();

Map<String, Object> params = new Map<String, Object>{
    'body' => SwagUser.getExample()
};

try {
    // cross your fingers
    api.createUser(params);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**SwagUser**](User.md)| Created user object |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="createUsersWithArrayInput"></a>
# **createUsersWithArrayInput**
> createUsersWithArrayInput(body)

Creates list of users with given input array



### Example
```java
SwagUserApi api = new SwagUserApi();

Map<String, Object> params = new Map<String, Object>{
    'body' => new List<SwagUser>{SwagUser.getExample()}
};

try {
    // cross your fingers
    api.createUsersWithArrayInput(params);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**List&lt;SwagUser&gt;**](SwagUser.md)| List of user object |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="createUsersWithListInput"></a>
# **createUsersWithListInput**
> createUsersWithListInput(body)

Creates list of users with given input array



### Example
```java
SwagUserApi api = new SwagUserApi();

Map<String, Object> params = new Map<String, Object>{
    'body' => new List<SwagUser>{SwagUser.getExample()}
};

try {
    // cross your fingers
    api.createUsersWithListInput(params);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**List&lt;SwagUser&gt;**](SwagUser.md)| List of user object |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="deleteUser"></a>
# **deleteUser**
> deleteUser(username)

Delete user

This can only be done by the logged in user.

### Example
```java
SwagUserApi api = new SwagUserApi();

Map<String, Object> params = new Map<String, Object>{
    'username' => 'username_example'
};

try {
    // cross your fingers
    api.deleteUser(params);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The name that needs to be deleted |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="getUserByName"></a>
# **getUserByName**
> SwagUser getUserByName(username)

Get user by user name



### Example
```java
SwagUserApi api = new SwagUserApi();

Map<String, Object> params = new Map<String, Object>{
    'username' => 'username_example'
};

try {
    // cross your fingers
    SwagUser result = api.getUserByName(params);
    System.debug(result);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The name that needs to be fetched. Use user1 for testing.  |

### Return type

[**SwagUser**](SwagUser.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="loginUser"></a>
# **loginUser**
> String loginUser(username, password)

Logs user into the system



### Example
```java
SwagUserApi api = new SwagUserApi();

Map<String, Object> params = new Map<String, Object>{
    'username' => 'username_example',
    'password' => 'password_example'
};

try {
    // cross your fingers
    String result = api.loginUser(params);
    System.debug(result);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The user name for login |
 **password** | **String**| The password for login in clear text |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="logoutUser"></a>
# **logoutUser**
> logoutUser()

Logs out current logged in user session



### Example
```java
SwagUserApi api = new SwagUserApi();

try {
    // cross your fingers
    api.logoutUser();
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="updateUser"></a>
# **updateUser**
> updateUser(username, body)

Updated user

This can only be done by the logged in user.

### Example
```java
SwagUserApi api = new SwagUserApi();

Map<String, Object> params = new Map<String, Object>{
    'username' => 'username_example',
    'body' => SwagUser.getExample()
};

try {
    // cross your fingers
    api.updateUser(params);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| name that need to be deleted |
 **body** | [**SwagUser**](User.md)| Updated user object |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

