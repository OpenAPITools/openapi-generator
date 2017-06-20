# IO.Swagger.IO.Swagger\API.UserApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**CreateUser**](UserApi.md#createuser) | **POST** /user | Create user
[**CreateUsersWithArrayInput**](UserApi.md#createuserswitharrayinput) | **POST** /user/createWithArray | Creates list of users with given input array
[**CreateUsersWithListInput**](UserApi.md#createuserswithlistinput) | **POST** /user/createWithList | Creates list of users with given input array
[**DeleteUser**](UserApi.md#deleteuser) | **DELETE** /user/{username} | Delete user
[**GetUserByName**](UserApi.md#getuserbyname) | **GET** /user/{username} | Get user by user name
[**LoginUser**](UserApi.md#loginuser) | **GET** /user/login | Logs user into the system
[**LogoutUser**](UserApi.md#logoutuser) | **GET** /user/logout | Logs out current logged in user session
[**UpdateUser**](UserApi.md#updateuser) | **PUT** /user/{username} | Updated user


<a name="createuser"></a>
# **CreateUser**
> void CreateUser (User body)

Create user

This can only be done by the logged in user.

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger\API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger\Model;

namespace Example
{
    public class CreateUserExample
    {
        public void main()
        {
            var apiInstance = new UserApi();
            var body = new User(); // User | Created user object

            try
            {
                // Create user
                apiInstance.CreateUser(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling UserApi.CreateUser: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**User**](User.md)| Created user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="createuserswitharrayinput"></a>
# **CreateUsersWithArrayInput**
> void CreateUsersWithArrayInput (IO.Swagger.Model.User body)

Creates list of users with given input array



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger\API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger\Model;

namespace Example
{
    public class CreateUsersWithArrayInputExample
    {
        public void main()
        {
            var apiInstance = new UserApi();
            var body = new IO.Swagger.Model.User(); // IO.Swagger.Model.User | List of user object

            try
            {
                // Creates list of users with given input array
                apiInstance.CreateUsersWithArrayInput(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling UserApi.CreateUsersWithArrayInput: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**IO.Swagger.Model.User**](User.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="createuserswithlistinput"></a>
# **CreateUsersWithListInput**
> void CreateUsersWithListInput (IO.Swagger.Model.User body)

Creates list of users with given input array



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger\API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger\Model;

namespace Example
{
    public class CreateUsersWithListInputExample
    {
        public void main()
        {
            var apiInstance = new UserApi();
            var body = new IO.Swagger.Model.User(); // IO.Swagger.Model.User | List of user object

            try
            {
                // Creates list of users with given input array
                apiInstance.CreateUsersWithListInput(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling UserApi.CreateUsersWithListInput: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**IO.Swagger.Model.User**](User.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="deleteuser"></a>
# **DeleteUser**
> void DeleteUser (String username)

Delete user

This can only be done by the logged in user.

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger\API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger\Model;

namespace Example
{
    public class DeleteUserExample
    {
        public void main()
        {
            var apiInstance = new UserApi();
            var username = username_example;  // String | The name that needs to be deleted

            try
            {
                // Delete user
                apiInstance.DeleteUser(username);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling UserApi.DeleteUser: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The name that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="getuserbyname"></a>
# **GetUserByName**
> IO.Swagger.Model.User GetUserByName (String username)

Get user by user name



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger\API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger\Model;

namespace Example
{
    public class GetUserByNameExample
    {
        public void main()
        {
            var apiInstance = new UserApi();
            var username = username_example;  // String | The name that needs to be fetched. Use user1 for testing. 

            try
            {
                // Get user by user name
                IO.Swagger.Model.User result = apiInstance.GetUserByName(username);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling UserApi.GetUserByName: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The name that needs to be fetched. Use user1 for testing.  | 

### Return type

[**IO.Swagger.Model.User**](User.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="loginuser"></a>
# **LoginUser**
> String LoginUser (String username, String password)

Logs user into the system



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger\API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger\Model;

namespace Example
{
    public class LoginUserExample
    {
        public void main()
        {
            var apiInstance = new UserApi();
            var username = username_example;  // String | The user name for login
            var password = password_example;  // String | The password for login in clear text

            try
            {
                // Logs user into the system
                String result = apiInstance.LoginUser(username, password);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling UserApi.LoginUser: " + e.Message );
            }
        }
    }
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

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="logoutuser"></a>
# **LogoutUser**
> void LogoutUser ()

Logs out current logged in user session



### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger\API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger\Model;

namespace Example
{
    public class LogoutUserExample
    {
        public void main()
        {
            var apiInstance = new UserApi();

            try
            {
                // Logs out current logged in user session
                apiInstance.LogoutUser();
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling UserApi.LogoutUser: " + e.Message );
            }
        }
    }
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
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="updateuser"></a>
# **UpdateUser**
> void UpdateUser (String username, User body)

Updated user

This can only be done by the logged in user.

### Example
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.IO.Swagger\API;
using IO.Swagger.Client;
using IO.Swagger.IO.Swagger\Model;

namespace Example
{
    public class UpdateUserExample
    {
        public void main()
        {
            var apiInstance = new UserApi();
            var username = username_example;  // String | name that need to be deleted
            var body = new User(); // User | Updated user object

            try
            {
                // Updated user
                apiInstance.UpdateUser(username, body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling UserApi.UpdateUser: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| name that need to be deleted | 
 **body** | [**User**](User.md)| Updated user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

