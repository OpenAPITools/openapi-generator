# petstore.UserApi

All URIs are relative to *http://petstore.swagger.io/v2*

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


# **createUser**
> createUser(user)

This can only be done by the logged in user.

### Example


```typescript
import { createConfiguration, UserApi } from 'ts-petstore-client';
import type { UserApiCreateUserRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new UserApi(configuration);

const request: UserApiCreateUserRequest = {
    // Created user object
  user: {
    id: 1,
    username: "username_example",
    firstName: "firstName_example",
    lastName: "lastName_example",
    email: "email_example",
    password: "password_example",
    phone: "phone_example",
    userStatus: 1,
  },
};

const data = await apiInstance.createUser(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | **User**| Created user object |


### Return type

void (empty response body)

### Authorization

[api_key](README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **createUsersWithArrayInput**
> createUsersWithArrayInput(user)



### Example


```typescript
import { createConfiguration, UserApi } from 'ts-petstore-client';
import type { UserApiCreateUsersWithArrayInputRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new UserApi(configuration);

const request: UserApiCreateUsersWithArrayInputRequest = {
    // List of user object
  user: [
    {
      id: 1,
      username: "username_example",
      firstName: "firstName_example",
      lastName: "lastName_example",
      email: "email_example",
      password: "password_example",
      phone: "phone_example",
      userStatus: 1,
    },
  ],
};

const data = await apiInstance.createUsersWithArrayInput(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | **Array<User>**| List of user object |


### Return type

void (empty response body)

### Authorization

[api_key](README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **createUsersWithListInput**
> createUsersWithListInput(user)



### Example


```typescript
import { createConfiguration, UserApi } from 'ts-petstore-client';
import type { UserApiCreateUsersWithListInputRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new UserApi(configuration);

const request: UserApiCreateUsersWithListInputRequest = {
    // List of user object
  user: [
    {
      id: 1,
      username: "username_example",
      firstName: "firstName_example",
      lastName: "lastName_example",
      email: "email_example",
      password: "password_example",
      phone: "phone_example",
      userStatus: 1,
    },
  ],
};

const data = await apiInstance.createUsersWithListInput(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | **Array<User>**| List of user object |


### Return type

void (empty response body)

### Authorization

[api_key](README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **deleteUser**
> deleteUser()

This can only be done by the logged in user.

### Example


```typescript
import { createConfiguration, UserApi } from 'ts-petstore-client';
import type { UserApiDeleteUserRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new UserApi(configuration);

const request: UserApiDeleteUserRequest = {
    // The name that needs to be deleted
  username: "username_example",
};

const data = await apiInstance.deleteUser(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | [**string**] | The name that needs to be deleted | defaults to undefined


### Return type

void (empty response body)

### Authorization

[api_key](README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**400** | Invalid username supplied |  -  |
**404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **getUserByName**
> User getUserByName()



### Example


```typescript
import { createConfiguration, UserApi } from 'ts-petstore-client';
import type { UserApiGetUserByNameRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new UserApi(configuration);

const request: UserApiGetUserByNameRequest = {
    // The name that needs to be fetched. Use user1 for testing.
  username: "username_example",
};

const data = await apiInstance.getUserByName(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | [**string**] | The name that needs to be fetched. Use user1 for testing. | defaults to undefined


### Return type

**User**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |
**400** | Invalid username supplied |  -  |
**404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **loginUser**
> string loginUser()



### Example


```typescript
import { createConfiguration, UserApi } from 'ts-petstore-client';
import type { UserApiLoginUserRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new UserApi(configuration);

const request: UserApiLoginUserRequest = {
    // The user name for login
  username: "CbUUGjjNSwg0_bs9ZayIMrKdgNvb6gvxmPb9GcsM61ate1RA89q3w1l4eH4XxEz.5awLMdeXylwK0lMGUSM4jsrh4dstlnQUN5vVdMLPA",
    // The password for login in clear text
  password: "password_example",
};

const data = await apiInstance.loginUser(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | [**string**] | The user name for login | defaults to undefined
 **password** | [**string**] | The password for login in clear text | defaults to undefined


### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  * Set-Cookie - Cookie authentication key for use with the &#x60;api_key&#x60; apiKey authentication. <br>  * X-Rate-Limit - calls per hour allowed by the user <br>  * X-Expires-After - date in UTC when token expires <br>  |
**400** | Invalid username/password supplied |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **logoutUser**
> logoutUser()



### Example


```typescript
import { createConfiguration, UserApi } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new UserApi(configuration);

const request = {};

const data = await apiInstance.logoutUser(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters
This endpoint does not need any parameter.


### Return type

void (empty response body)

### Authorization

[api_key](README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **updateUser**
> updateUser(user)

This can only be done by the logged in user.

### Example


```typescript
import { createConfiguration, UserApi } from 'ts-petstore-client';
import type { UserApiUpdateUserRequest } from 'ts-petstore-client';

const configuration = createConfiguration();
const apiInstance = new UserApi(configuration);

const request: UserApiUpdateUserRequest = {
    // name that need to be deleted
  username: "username_example",
    // Updated user object
  user: {
    id: 1,
    username: "username_example",
    firstName: "firstName_example",
    lastName: "lastName_example",
    email: "email_example",
    password: "password_example",
    phone: "phone_example",
    userStatus: 1,
  },
};

const data = await apiInstance.updateUser(request);
console.log('API called successfully. Returned data:', data);
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | **User**| Updated user object |
 **username** | [**string**] | name that need to be deleted | defaults to undefined


### Return type

void (empty response body)

### Authorization

[api_key](README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**400** | Invalid user supplied |  -  |
**404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


