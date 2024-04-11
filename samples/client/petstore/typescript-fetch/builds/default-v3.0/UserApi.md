# .UserApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createUser**](UserApi.md#createuser) | **POST** /user | Create user
[**createUsersWithArrayInput**](UserApi.md#createuserswitharrayinput) | **POST** /user/createWithArray | Creates list of users with given input array
[**createUsersWithListInput**](UserApi.md#createuserswithlistinput) | **POST** /user/createWithList | Creates list of users with given input array
[**deleteUser**](UserApi.md#deleteuser) | **DELETE** /user/{username} | Delete user
[**getUserByName**](UserApi.md#getuserbyname) | **GET** /user/{username} | Get user by user name
[**loginUser**](UserApi.md#loginuser) | **GET** /user/login | Logs user into the system
[**logoutUser**](UserApi.md#logoutuser) | **GET** /user/logout | Logs out current logged in user session
[**updateUser**](UserApi.md#updateuser) | **PUT** /user/{username} | Updated user


## **createUser**
> createUser(user)

This can only be done by the logged in user.

### Example


```typescript
import { UserApi } from '';

const apiInstance = new .UserApi();

let body:.UserApiCreateUserRequest = {
    // User | Created user object
    user: ,
};

apiInstance.createUser(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | **User**| Created user object |


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
**0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

## **createUsersWithArrayInput**
> createUsersWithArrayInput(user)



### Example


```typescript
import { UserApi } from '';

const apiInstance = new .UserApi();

let body:.UserApiCreateUsersWithArrayInputRequest = {
    // Array<User> | List of user object
    user: ,
};

apiInstance.createUsersWithArrayInput(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | **Array<User>**| List of user object |


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
**0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

## **createUsersWithListInput**
> createUsersWithListInput(user)



### Example


```typescript
import { UserApi } from '';

const apiInstance = new .UserApi();

let body:.UserApiCreateUsersWithListInputRequest = {
    // Array<User> | List of user object
    user: ,
};

apiInstance.createUsersWithListInput(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | **Array<User>**| List of user object |


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
**0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

## **deleteUser**
> deleteUser()

This can only be done by the logged in user.

### Example


```typescript
import { UserApi } from '';

const apiInstance = new .UserApi();

let body:.UserApiDeleteUserRequest = {
    // string | The name that needs to be deleted
    username: username_example,
};

apiInstance.deleteUser(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | [**string**] | The name that needs to be deleted | defaults to undefined


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
**400** | Invalid username supplied |  -  |
**404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

## **getUserByName**
> User getUserByName()



### Example


```typescript
import { UserApi } from '';

const apiInstance = new .UserApi();

let body:.UserApiGetUserByNameRequest = {
    // string | The name that needs to be fetched. Use user1 for testing.
    username: username_example,
};

apiInstance.getUserByName(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | [**string**] | The name that needs to be fetched. Use user1 for testing. | defaults to undefined


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
**200** | successful operation |  -  |
**400** | Invalid username supplied |  -  |
**404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

## **loginUser**
> string loginUser()



### Example


```typescript
import { UserApi } from '';

const apiInstance = new .UserApi();

let body:.UserApiLoginUserRequest = {
    // string | The user name for login
    username: username_example,
    // string | The password for login in clear text
    password: password_example,
};

apiInstance.loginUser(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
**200** | successful operation |  * X-Rate-Limit - calls per hour allowed by the user <br>  * X-Expires-After - date in UTC when token expires <br>  |
**400** | Invalid username/password supplied |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

## **logoutUser**
> logoutUser()



### Example


```typescript
import { UserApi } from '';

const apiInstance = new .UserApi();

let body:any = {};

apiInstance.logoutUser(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
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
**0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)

## **updateUser**
> updateUser(user)

This can only be done by the logged in user.

### Example


```typescript
import { UserApi } from '';

const apiInstance = new .UserApi();

let body:.UserApiUpdateUserRequest = {
    // string | name that need to be deleted
    username: username_example,
    // User | Updated user object
    user: ,
};

apiInstance.updateUser(body).then((data:any) => {
    console.log('API called successfully. Returned data: ' + data);
}).catch((error:any) => console.error(error));
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | **User**| Updated user object |
 **username** | [**string**] | name that need to be deleted | defaults to undefined


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
**400** | Invalid user supplied |  -  |
**404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](README.md#documentation-for-api-endpoints) [[Back to Model list]](README.md#documentation-for-models) [[Back to README]](README.md)


