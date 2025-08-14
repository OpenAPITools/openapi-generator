# UserApi

All URIs are relative to *http://petstore.swagger.io/v2*

| Method | HTTP request | Description |
|------------- | ------------- | -------------|
| [**createUser**](UserApi.md#createuser) | **POST** /user | Create user |
| [**createUsersWithArrayInput**](UserApi.md#createuserswitharrayinput) | **POST** /user/createWithArray | Creates list of users with given input array |
| [**createUsersWithListInput**](UserApi.md#createuserswithlistinput) | **POST** /user/createWithList | Creates list of users with given input array |
| [**deleteUser**](UserApi.md#deleteuser) | **DELETE** /user/{username} | Delete user |
| [**getUserByName**](UserApi.md#getuserbyname) | **GET** /user/{username} | Get user by user name |
| [**loginUser**](UserApi.md#loginuser) | **GET** /user/login | Logs user into the system |
| [**logoutUser**](UserApi.md#logoutuser) | **GET** /user/logout | Logs out current logged in user session |
| [**updateUser**](UserApi.md#updateuser) | **PUT** /user/{username} | Updated user |



## createUser

> createUser(body)

Create user

This can only be done by the logged in user.

### Example

```ts
import {
  Configuration,
  UserApi,
} from '@openapitools/typescript-fetch-petstore';
import type { CreateUserRequest } from '@openapitools/typescript-fetch-petstore';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-petstore SDK...");
  const api = new UserApi();

  const body = {
    // User | Created user object
    body: ...,
  } satisfies CreateUserRequest;

  try {
    const data = await api.createUser(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **body** | [User](User.md) | Created user object | |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## createUsersWithArrayInput

> createUsersWithArrayInput(body)

Creates list of users with given input array

### Example

```ts
import {
  Configuration,
  UserApi,
} from '@openapitools/typescript-fetch-petstore';
import type { CreateUsersWithArrayInputRequest } from '@openapitools/typescript-fetch-petstore';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-petstore SDK...");
  const api = new UserApi();

  const body = {
    // Array<User> | List of user object
    body: ...,
  } satisfies CreateUsersWithArrayInputRequest;

  try {
    const data = await api.createUsersWithArrayInput(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **body** | `Array<User>` | List of user object | |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## createUsersWithListInput

> createUsersWithListInput(body)

Creates list of users with given input array

### Example

```ts
import {
  Configuration,
  UserApi,
} from '@openapitools/typescript-fetch-petstore';
import type { CreateUsersWithListInputRequest } from '@openapitools/typescript-fetch-petstore';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-petstore SDK...");
  const api = new UserApi();

  const body = {
    // Array<User> | List of user object
    body: ...,
  } satisfies CreateUsersWithListInputRequest;

  try {
    const data = await api.createUsersWithListInput(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **body** | `Array<User>` | List of user object | |

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## deleteUser

> deleteUser(username)

Delete user

This can only be done by the logged in user.

### Example

```ts
import {
  Configuration,
  UserApi,
} from '@openapitools/typescript-fetch-petstore';
import type { DeleteUserRequest } from '@openapitools/typescript-fetch-petstore';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-petstore SDK...");
  const api = new UserApi();

  const body = {
    // string | The name that needs to be deleted
    username: username_example,
  } satisfies DeleteUserRequest;

  try {
    const data = await api.deleteUser(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **username** | `string` | The name that needs to be deleted | [Defaults to `undefined`] |

### Return type

`void` (Empty response body)

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

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## getUserByName

> User getUserByName(username)

Get user by user name

### Example

```ts
import {
  Configuration,
  UserApi,
} from '@openapitools/typescript-fetch-petstore';
import type { GetUserByNameRequest } from '@openapitools/typescript-fetch-petstore';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-petstore SDK...");
  const api = new UserApi();

  const body = {
    // string | The name that needs to be fetched. Use user1 for testing.
    username: username_example,
  } satisfies GetUserByNameRequest;

  try {
    const data = await api.getUserByName(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **username** | `string` | The name that needs to be fetched. Use user1 for testing. | [Defaults to `undefined`] |

### Return type

[**User**](User.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `application/xml`, `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid username supplied |  -  |
| **404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## loginUser

> string loginUser(username, password)

Logs user into the system

### Example

```ts
import {
  Configuration,
  UserApi,
} from '@openapitools/typescript-fetch-petstore';
import type { LoginUserRequest } from '@openapitools/typescript-fetch-petstore';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-petstore SDK...");
  const api = new UserApi();

  const body = {
    // string | The user name for login
    username: username_example,
    // string | The password for login in clear text
    password: password_example,
  } satisfies LoginUserRequest;

  try {
    const data = await api.loginUser(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **username** | `string` | The user name for login | [Defaults to `undefined`] |
| **password** | `string` | The password for login in clear text | [Defaults to `undefined`] |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `application/xml`, `application/json`


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  * X-Rate-Limit - calls per hour allowed by the user <br>  * X-Expires-After - date in UTC when token expires <br>  |
| **400** | Invalid username/password supplied |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## logoutUser

> logoutUser()

Logs out current logged in user session

### Example

```ts
import {
  Configuration,
  UserApi,
} from '@openapitools/typescript-fetch-petstore';
import type { LogoutUserRequest } from '@openapitools/typescript-fetch-petstore';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-petstore SDK...");
  const api = new UserApi();

  try {
    const data = await api.logoutUser();
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters

This endpoint does not need any parameter.

### Return type

`void` (Empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)


## updateUser

> updateUser(username, body)

Updated user

This can only be done by the logged in user.

### Example

```ts
import {
  Configuration,
  UserApi,
} from '@openapitools/typescript-fetch-petstore';
import type { UpdateUserRequest } from '@openapitools/typescript-fetch-petstore';

async function example() {
  console.log("🚀 Testing @openapitools/typescript-fetch-petstore SDK...");
  const api = new UserApi();

  const body = {
    // string | name that need to be deleted
    username: username_example,
    // User | Updated user object
    body: ...,
  } satisfies UpdateUserRequest;

  try {
    const data = await api.updateUser(body);
    console.log(data);
  } catch (error) {
    console.error(error);
  }
}

// Run the test
example().catch(console.error);
```

### Parameters


| Name | Type | Description  | Notes |
|------------- | ------------- | ------------- | -------------|
| **username** | `string` | name that need to be deleted | [Defaults to `undefined`] |
| **body** | [User](User.md) | Updated user object | |

### Return type

`void` (Empty response body)

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

[[Back to top]](#) [[Back to API list]](../README.md#api-endpoints) [[Back to Model list]](../README.md#models) [[Back to README]](../README.md)

