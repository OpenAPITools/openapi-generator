# UserApi

All URIs are relative to *http://petstore.swagger.io/v2*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**createUser**](#createuser) | **POST** /user | Create user|
|[**createUsersWithArrayInput**](#createuserswitharrayinput) | **POST** /user/createWithArray | Creates list of users with given input array|
|[**createUsersWithListInput**](#createuserswithlistinput) | **POST** /user/createWithList | Creates list of users with given input array|
|[**deleteUser**](#deleteuser) | **DELETE** /user/{username} | Delete user|
|[**getUserByName**](#getuserbyname) | **GET** /user/{username} | Get user by user name|
|[**loginUser**](#loginuser) | **GET** /user/login | Logs user into the system|
|[**logoutUser**](#logoutuser) | **GET** /user/logout | Logs out current logged in user session|
|[**updateUser**](#updateuser) | **PUT** /user/{username} | Updated user|

# **createUser**
> createUser(body)

This can only be done by the logged in user.

### Example

```typescript
import {
    UserApi,
    Configuration,
    User
} from '@openapitools/typescript-axios-petstore';

const configuration = new Configuration();
const apiInstance = new UserApi(configuration);

let body: User; //Created user object

const { status, data } = await apiInstance.createUser(
    body
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **body** | **User**| Created user object | |


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
|**0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **createUsersWithArrayInput**
> createUsersWithArrayInput(body)


### Example

```typescript
import {
    UserApi,
    Configuration
} from '@openapitools/typescript-axios-petstore';

const configuration = new Configuration();
const apiInstance = new UserApi(configuration);

let body: Array<User>; //List of user object

const { status, data } = await apiInstance.createUsersWithArrayInput(
    body
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **body** | **Array<User>**| List of user object | |


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
|**0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **createUsersWithListInput**
> createUsersWithListInput(body)


### Example

```typescript
import {
    UserApi,
    Configuration
} from '@openapitools/typescript-axios-petstore';

const configuration = new Configuration();
const apiInstance = new UserApi(configuration);

let body: Array<User>; //List of user object

const { status, data } = await apiInstance.createUsersWithListInput(
    body
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **body** | **Array<User>**| List of user object | |


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
|**0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **deleteUser**
> deleteUser()

This can only be done by the logged in user.

### Example

```typescript
import {
    UserApi,
    Configuration
} from '@openapitools/typescript-axios-petstore';

const configuration = new Configuration();
const apiInstance = new UserApi(configuration);

let username: string; //The name that needs to be deleted (default to undefined)

const { status, data } = await apiInstance.deleteUser(
    username
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **username** | [**string**] | The name that needs to be deleted | defaults to undefined|


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
|**400** | Invalid username supplied |  -  |
|**404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getUserByName**
> User getUserByName()


### Example

```typescript
import {
    UserApi,
    Configuration
} from '@openapitools/typescript-axios-petstore';

const configuration = new Configuration();
const apiInstance = new UserApi(configuration);

let username: string; //The name that needs to be fetched. Use user1 for testing. (default to undefined)

const { status, data } = await apiInstance.getUserByName(
    username
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **username** | [**string**] | The name that needs to be fetched. Use user1 for testing. | defaults to undefined|


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
|**200** | successful operation |  -  |
|**400** | Invalid username supplied |  -  |
|**404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **loginUser**
> string loginUser()


### Example

```typescript
import {
    UserApi,
    Configuration
} from '@openapitools/typescript-axios-petstore';

const configuration = new Configuration();
const apiInstance = new UserApi(configuration);

let username: string; //The user name for login (default to undefined)
let password: string; //The password for login in clear text (default to undefined)

const { status, data } = await apiInstance.loginUser(
    username,
    password
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **username** | [**string**] | The user name for login | defaults to undefined|
| **password** | [**string**] | The password for login in clear text | defaults to undefined|


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
|**200** | successful operation |  * X-Rate-Limit - calls per hour allowed by the user <br>  * X-Expires-After - date in UTC when token expires <br>  |
|**400** | Invalid username/password supplied |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **logoutUser**
> logoutUser()


### Example

```typescript
import {
    UserApi,
    Configuration
} from '@openapitools/typescript-axios-petstore';

const configuration = new Configuration();
const apiInstance = new UserApi(configuration);

const { status, data } = await apiInstance.logoutUser();
```

### Parameters
This endpoint does not have any parameters.


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
|**0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updateUser**
> updateUser(body)

This can only be done by the logged in user.

### Example

```typescript
import {
    UserApi,
    Configuration,
    User
} from '@openapitools/typescript-axios-petstore';

const configuration = new Configuration();
const apiInstance = new UserApi(configuration);

let username: string; //name that need to be deleted (default to undefined)
let body: User; //Updated user object

const { status, data } = await apiInstance.updateUser(
    username,
    body
);
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **body** | **User**| Updated user object | |
| **username** | [**string**] | name that need to be deleted | defaults to undefined|


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
|**400** | Invalid user supplied |  -  |
|**404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

