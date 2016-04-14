# IO.Swagger.Api.UserApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**CreateUser**](UserApi.md#CreateUser) | **POST** /user | Create user
[**CreateUsersWithArrayInput**](UserApi.md#CreateUsersWithArrayInput) | **POST** /user/createWithArray | Creates list of users with given input array
[**CreateUsersWithListInput**](UserApi.md#CreateUsersWithListInput) | **POST** /user/createWithList | Creates list of users with given input array
[**DeleteUser**](UserApi.md#DeleteUser) | **DELETE** /user/{username} | Delete user
[**GetUserByName**](UserApi.md#GetUserByName) | **GET** /user/{username} | Get user by user name
[**LoginUser**](UserApi.md#LoginUser) | **GET** /user/login | Logs user into the system
[**LogoutUser**](UserApi.md#LogoutUser) | **GET** /user/logout | Logs out current logged in user session
[**UpdateUser**](UserApi.md#UpdateUser) | **PUT** /user/{username} | Updated user


# **CreateUser**
> CreateUser(body)

Create user

This can only be done by the logged in user.

### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class CreateUserExample
    {
        public void main(){
            
            var apiInstance = new UserApi();
            var body = new User(); // User | Created user object

            try {
                apiInstance.CreateUser(body);
            } catch (Exception e) {
                Debug.Print("Exception when calling UserApi.CreateUser: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**User**](User.md)| Created user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

# **CreateUsersWithArrayInput**
> CreateUsersWithArrayInput(body)

Creates list of users with given input array



### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class CreateUsersWithArrayInputExample
    {
        public void main(){
            
            var apiInstance = new UserApi();
            var body = new List<User>(); // List<User> | List of user object

            try {
                apiInstance.CreateUsersWithArrayInput(body);
            } catch (Exception e) {
                Debug.Print("Exception when calling UserApi.CreateUsersWithArrayInput: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**List&lt;User&gt;**](User.md)| List of user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

# **CreateUsersWithListInput**
> CreateUsersWithListInput(body)

Creates list of users with given input array



### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class CreateUsersWithListInputExample
    {
        public void main(){
            
            var apiInstance = new UserApi();
            var body = new List<User>(); // List<User> | List of user object

            try {
                apiInstance.CreateUsersWithListInput(body);
            } catch (Exception e) {
                Debug.Print("Exception when calling UserApi.CreateUsersWithListInput: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**List&lt;User&gt;**](User.md)| List of user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

# **DeleteUser**
> DeleteUser(username)

Delete user

This can only be done by the logged in user.

### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class DeleteUserExample
    {
        public void main(){
            
            // Configure HTTP basic authorization: test_http_basic
            Configuration.Default.Username = 'YOUR_USERNAME';
            Configuration.Default.Password = 'YOUR_PASSWORD';

            var apiInstance = new UserApi();
            var username = username_example;  // string | The name that needs to be deleted

            try {
                apiInstance.DeleteUser(username);
            } catch (Exception e) {
                Debug.Print("Exception when calling UserApi.DeleteUser: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| The name that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

[test_http_basic](../README.md#test_http_basic)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

# **GetUserByName**
> User GetUserByName(username)

Get user by user name



### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class GetUserByNameExample
    {
        public void main(){
            
            var apiInstance = new UserApi();
            var username = username_example;  // string | The name that needs to be fetched. Use user1 for testing. 

            try {
                User result = apiInstance.GetUserByName(username);
                Debug.WriteLine(result);
            } catch (Exception e) {
                Debug.Print("Exception when calling UserApi.GetUserByName: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| The name that needs to be fetched. Use user1 for testing.  | 

### Return type

[**User**](User.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

# **LoginUser**
> string LoginUser(username, password)

Logs user into the system



### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class LoginUserExample
    {
        public void main(){
            
            var apiInstance = new UserApi();
            var username = username_example;  // string | The user name for login
            var password = password_example;  // string | The password for login in clear text

            try {
                string result = apiInstance.LoginUser(username, password);
                Debug.WriteLine(result);
            } catch (Exception e) {
                Debug.Print("Exception when calling UserApi.LoginUser: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| The user name for login | [optional] 
 **password** | **string**| The password for login in clear text | [optional] 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

# **LogoutUser**
> LogoutUser()

Logs out current logged in user session



### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class LogoutUserExample
    {
        public void main(){
            
            var apiInstance = new UserApi();

            try {
                apiInstance.LogoutUser();
            } catch (Exception e) {
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
 - **Accept**: application/json, application/xml

# **UpdateUser**
> UpdateUser(username, body)

Updated user

This can only be done by the logged in user.

### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class UpdateUserExample
    {
        public void main(){
            
            var apiInstance = new UserApi();
            var username = username_example;  // string | name that need to be deleted
            var body = new User(); // User | Updated user object

            try {
                apiInstance.UpdateUser(username, body);
            } catch (Exception e) {
                Debug.Print("Exception when calling UserApi.UpdateUser: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| name that need to be deleted | 
 **body** | [**User**](User.md)| Updated user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

