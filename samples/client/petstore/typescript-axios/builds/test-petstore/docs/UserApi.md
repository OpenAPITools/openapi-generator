# .UserApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|[**createUser**](UserApi.md#createUser) | **POST** /user | Create user|
|[**createUsersWithArrayInput**](UserApi.md#createUsersWithArrayInput) | **POST** /user/createWithArray | Creates list of users with given input array|
|[**createUsersWithListInput**](UserApi.md#createUsersWithListInput) | **POST** /user/createWithList | Creates list of users with given input array|
|[**deleteUser**](UserApi.md#deleteUser) | **DELETE** /user/{username} | Delete user|
|[**getUserByName**](UserApi.md#getUserByName) | **GET** /user/{username} | Get user by user name|
|[**loginUser**](UserApi.md#loginUser) | **GET** /user/login | Logs user into the system|
|[**logoutUser**](UserApi.md#logoutUser) | **GET** /user/logout | Logs out current logged in user session|
|[**updateUser**](UserApi.md#updateUser) | **PUT** /user/{username} | Updated user|

# **createUser**
> createUser(user)

This can only be done by the logged in user.

### Example

```typescript
import * as ApiModule from '';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.UserApi = new ApiModule.UserApi(configuration);

let user: ApiModule.User = ; // User | Created user object

apiInstance.createUser(user).then(() => {
  console.log('API called successfully.');
}).catch((error: any) => {
  console.error(error);
});
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **user** | **User**| Created user object | |


### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **createUsersWithArrayInput**
> createUsersWithArrayInput(user)



### Example

```typescript
import * as ApiModule from '';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.UserApi = new ApiModule.UserApi(configuration);

let user: ApiModule.Array<User> = [{"username":"foo"},{"username":"bar"}]; // Array<User> | List of user object

apiInstance.createUsersWithArrayInput(user).then(() => {
  console.log('API called successfully.');
}).catch((error: any) => {
  console.error(error);
});
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **user** | **Array<User>**| List of user object | |


### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **createUsersWithListInput**
> createUsersWithListInput(user)



### Example

```typescript
import * as ApiModule from '';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.UserApi = new ApiModule.UserApi(configuration);

let user: ApiModule.Array<User> = [{"username":"foo"},{"username":"bar"}]; // Array<User> | List of user object

apiInstance.createUsersWithListInput(user).then(() => {
  console.log('API called successfully.');
}).catch((error: any) => {
  console.error(error);
});
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **user** | **Array<User>**| List of user object | |


### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **deleteUser**
> deleteUser()

This can only be done by the logged in user.

### Example

```typescript
import * as ApiModule from '';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.UserApi = new ApiModule.UserApi(configuration);

let username: string = username_example; // string | The name that needs to be deleted

apiInstance.deleteUser(username).then(() => {
  console.log('API called successfully.');
}).catch((error: any) => {
  console.error(error);
});
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

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **getUserByName**
> User getUserByName()



### Example

```typescript
import * as ApiModule from '';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.UserApi = new ApiModule.UserApi(configuration);

let username: string = username_example; // string | The name that needs to be fetched. Use user1 for testing.

apiInstance.getUserByName(username).then((data: any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error: any) => {
  console.error(error);
});
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

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **loginUser**
> string loginUser()



### Example

```typescript
import * as ApiModule from '';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.UserApi = new ApiModule.UserApi(configuration);

let username: string = username_example; // string | The user name for login
let password: string = password_example; // string | The password for login in clear text

apiInstance.loginUser(username, password).then((data: any) => {
  console.log('API called successfully. Returned data: ', data);
}).catch((error: any) => {
  console.error(error);
});
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

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **logoutUser**
> logoutUser()



### Example

```typescript
import * as ApiModule from '';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.UserApi = new ApiModule.UserApi(configuration);

//let body: ApiModule. = {};

apiInstance.logoutUser().then(() => {
  console.log('API called successfully.');
}).catch((error: any) => {
  console.error(error);
});
```

### Parameters
This endpoint does not need any parameter.


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

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

# **updateUser**
> updateUser(user)

This can only be done by the logged in user.

### Example

```typescript
import * as ApiModule from '';

const configuration: ApiModule.Configuration = new ApiModule.Configuration();
const apiInstance: ApiModule.UserApi = new ApiModule.UserApi(configuration);

let username: string = username_example; // string | name that need to be deleted
let user: ApiModule.User = ; // User | Updated user object

apiInstance.updateUser(username, user).then(() => {
  console.log('API called successfully.');
}).catch((error: any) => {
  console.error(error);
});
```

### Parameters

|Name | Type | Description  | Notes|
|------------- | ------------- | ------------- | -------------|
| **user** | **User**| Updated user object | |
| **username** | [**string**] | name that need to be deleted | defaults to undefined|


### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
|**400** | Invalid user supplied |  -  |
|**404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


