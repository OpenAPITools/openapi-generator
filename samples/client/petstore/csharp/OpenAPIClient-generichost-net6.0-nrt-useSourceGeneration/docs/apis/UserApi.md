# UseSourceGeneration.Api.UserApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**CreateUser**](UserApi.md#createuser) | **POST** /user | Create user |
| [**CreateUsersWithArrayInput**](UserApi.md#createuserswitharrayinput) | **POST** /user/createWithArray | Creates list of users with given input array |
| [**CreateUsersWithListInput**](UserApi.md#createuserswithlistinput) | **POST** /user/createWithList | Creates list of users with given input array |
| [**DeleteUser**](UserApi.md#deleteuser) | **DELETE** /user/{username} | Delete user |
| [**GetUserByName**](UserApi.md#getuserbyname) | **GET** /user/{username} | Get user by user name |
| [**LoginUser**](UserApi.md#loginuser) | **GET** /user/login | Logs user into the system |
| [**LogoutUser**](UserApi.md#logoutuser) | **GET** /user/logout | Logs out current logged in user session |
| [**UpdateUser**](UserApi.md#updateuser) | **PUT** /user/{username} | Updated user |

<a id="createuser"></a>
# **CreateUser**
> void CreateUser (User user)

Create user

This can only be done by the logged in user.

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using UseSourceGeneration.Api;
using UseSourceGeneration.Client;
using UseSourceGeneration.Model;

namespace Example
{
    public class CreateUserExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new UserApi(config);
            var user = new User(); // User | Created user object

            try
            {
                // Create user
                apiInstance.CreateUser(user);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling UserApi.CreateUser: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the CreateUserWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Create user
    apiInstance.CreateUserWithHttpInfo(user);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling UserApi.CreateUserWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **user** | [**User**](User.md) | Created user object |  |

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
| **0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="createuserswitharrayinput"></a>
# **CreateUsersWithArrayInput**
> void CreateUsersWithArrayInput (List<User> user)

Creates list of users with given input array

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using UseSourceGeneration.Api;
using UseSourceGeneration.Client;
using UseSourceGeneration.Model;

namespace Example
{
    public class CreateUsersWithArrayInputExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new UserApi(config);
            var user = new List<User>(); // List<User> | List of user object

            try
            {
                // Creates list of users with given input array
                apiInstance.CreateUsersWithArrayInput(user);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling UserApi.CreateUsersWithArrayInput: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the CreateUsersWithArrayInputWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Creates list of users with given input array
    apiInstance.CreateUsersWithArrayInputWithHttpInfo(user);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling UserApi.CreateUsersWithArrayInputWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **user** | [**List&lt;User&gt;**](User.md) | List of user object |  |

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
| **0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="createuserswithlistinput"></a>
# **CreateUsersWithListInput**
> void CreateUsersWithListInput (List<User> user)

Creates list of users with given input array

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using UseSourceGeneration.Api;
using UseSourceGeneration.Client;
using UseSourceGeneration.Model;

namespace Example
{
    public class CreateUsersWithListInputExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new UserApi(config);
            var user = new List<User>(); // List<User> | List of user object

            try
            {
                // Creates list of users with given input array
                apiInstance.CreateUsersWithListInput(user);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling UserApi.CreateUsersWithListInput: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the CreateUsersWithListInputWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Creates list of users with given input array
    apiInstance.CreateUsersWithListInputWithHttpInfo(user);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling UserApi.CreateUsersWithListInputWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **user** | [**List&lt;User&gt;**](User.md) | List of user object |  |

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
| **0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="deleteuser"></a>
# **DeleteUser**
> void DeleteUser (string username)

Delete user

This can only be done by the logged in user.

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using UseSourceGeneration.Api;
using UseSourceGeneration.Client;
using UseSourceGeneration.Model;

namespace Example
{
    public class DeleteUserExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new UserApi(config);
            var username = "username_example";  // string | The name that needs to be deleted

            try
            {
                // Delete user
                apiInstance.DeleteUser(username);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling UserApi.DeleteUser: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the DeleteUserWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Delete user
    apiInstance.DeleteUserWithHttpInfo(username);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling UserApi.DeleteUserWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **username** | **string** | The name that needs to be deleted |  |

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
| **400** | Invalid username supplied |  -  |
| **404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="getuserbyname"></a>
# **GetUserByName**
> User GetUserByName (string username)

Get user by user name

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using UseSourceGeneration.Api;
using UseSourceGeneration.Client;
using UseSourceGeneration.Model;

namespace Example
{
    public class GetUserByNameExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new UserApi(config);
            var username = "username_example";  // string | The name that needs to be fetched. Use user1 for testing.

            try
            {
                // Get user by user name
                User result = apiInstance.GetUserByName(username);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling UserApi.GetUserByName: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the GetUserByNameWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Get user by user name
    ApiResponse<User> response = apiInstance.GetUserByNameWithHttpInfo(username);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling UserApi.GetUserByNameWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **username** | **string** | The name that needs to be fetched. Use user1 for testing. |  |

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
| **200** | successful operation |  -  |
| **400** | Invalid username supplied |  -  |
| **404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="loginuser"></a>
# **LoginUser**
> string LoginUser (string username, string password)

Logs user into the system

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using UseSourceGeneration.Api;
using UseSourceGeneration.Client;
using UseSourceGeneration.Model;

namespace Example
{
    public class LoginUserExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new UserApi(config);
            var username = "username_example";  // string | The user name for login
            var password = "password_example";  // string | The password for login in clear text

            try
            {
                // Logs user into the system
                string result = apiInstance.LoginUser(username, password);
                Debug.WriteLine(result);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling UserApi.LoginUser: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the LoginUserWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Logs user into the system
    ApiResponse<string> response = apiInstance.LoginUserWithHttpInfo(username, password);
    Debug.Write("Status Code: " + response.StatusCode);
    Debug.Write("Response Headers: " + response.Headers);
    Debug.Write("Response Body: " + response.Data);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling UserApi.LoginUserWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **username** | **string** | The user name for login |  |
| **password** | **string** | The password for login in clear text |  |

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
| **200** | successful operation |  * X-Rate-Limit - calls per hour allowed by the user <br>  * X-Expires-After - date in UTC when token expires <br>  |
| **400** | Invalid username/password supplied |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="logoutuser"></a>
# **LogoutUser**
> void LogoutUser ()

Logs out current logged in user session

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using UseSourceGeneration.Api;
using UseSourceGeneration.Client;
using UseSourceGeneration.Model;

namespace Example
{
    public class LogoutUserExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new UserApi(config);

            try
            {
                // Logs out current logged in user session
                apiInstance.LogoutUser();
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling UserApi.LogoutUser: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the LogoutUserWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Logs out current logged in user session
    apiInstance.LogoutUserWithHttpInfo();
}
catch (ApiException e)
{
    Debug.Print("Exception when calling UserApi.LogoutUserWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
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
| **0** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="updateuser"></a>
# **UpdateUser**
> void UpdateUser (User user, string username)

Updated user

This can only be done by the logged in user.

### Example
```csharp
using System.Collections.Generic;
using System.Diagnostics;
using UseSourceGeneration.Api;
using UseSourceGeneration.Client;
using UseSourceGeneration.Model;

namespace Example
{
    public class UpdateUserExample
    {
        public static void Main()
        {
            Configuration config = new Configuration();
            config.BasePath = "http://petstore.swagger.io:80/v2";
            var apiInstance = new UserApi(config);
            var user = new User(); // User | Updated user object
            var username = "username_example";  // string | name that need to be deleted

            try
            {
                // Updated user
                apiInstance.UpdateUser(user, username);
            }
            catch (ApiException  e)
            {
                Debug.Print("Exception when calling UserApi.UpdateUser: " + e.Message);
                Debug.Print("Status Code: " + e.ErrorCode);
                Debug.Print(e.StackTrace);
            }
        }
    }
}
```

#### Using the UpdateUserWithHttpInfo variant
This returns an ApiResponse object which contains the response data, status code and headers.

```csharp
try
{
    // Updated user
    apiInstance.UpdateUserWithHttpInfo(user, username);
}
catch (ApiException e)
{
    Debug.Print("Exception when calling UserApi.UpdateUserWithHttpInfo: " + e.Message);
    Debug.Print("Status Code: " + e.ErrorCode);
    Debug.Print(e.StackTrace);
}
```

### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **user** | [**User**](User.md) | Updated user object |  |
| **username** | **string** | name that need to be deleted |  |

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
| **400** | Invalid user supplied |  -  |
| **404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

