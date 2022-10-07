# UserAPI

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createUser**](UserAPI.md#createuser) | **POST** /user | Create user
[**createUsersWithArrayInput**](UserAPI.md#createuserswitharrayinput) | **POST** /user/createWithArray | Creates list of users with given input array
[**createUsersWithListInput**](UserAPI.md#createuserswithlistinput) | **POST** /user/createWithList | Creates list of users with given input array
[**deleteUser**](UserAPI.md#deleteuser) | **DELETE** /user/{username} | Delete user
[**getUserByName**](UserAPI.md#getuserbyname) | **GET** /user/{username} | Get user by user name
[**loginUser**](UserAPI.md#loginuser) | **GET** /user/login | Logs user into the system
[**logoutUser**](UserAPI.md#logoutuser) | **GET** /user/logout | Logs out current logged in user session
[**updateUser**](UserAPI.md#updateuser) | **PUT** /user/{username} | Updated user


# **createUser**
```swift
    open class func createUser(user: User, completion: @escaping (_ data: Void?, _ error: Error?) -> Void)
```

Create user

This can only be done by the logged in user.

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let user = User(id: 123, username: "username_example", firstName: "firstName_example", lastName: "lastName_example", email: "email_example", password: "password_example", phone: "phone_example", userStatus: 123) // User | Created user object

// Create user
UserAPI.createUser(user: user) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | [**User**](User.md) | Created user object | 

### Return type

Void (empty response body)

### Authorization

[auth_cookie](../README.md#auth_cookie)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **createUsersWithArrayInput**
```swift
    open class func createUsersWithArrayInput(user: [User], completion: @escaping (_ data: Void?, _ error: Error?) -> Void)
```

Creates list of users with given input array



### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let user = [User(id: 123, username: "username_example", firstName: "firstName_example", lastName: "lastName_example", email: "email_example", password: "password_example", phone: "phone_example", userStatus: 123)] // [User] | List of user object

// Creates list of users with given input array
UserAPI.createUsersWithArrayInput(user: user) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | [**[User]**](User.md) | List of user object | 

### Return type

Void (empty response body)

### Authorization

[auth_cookie](../README.md#auth_cookie)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **createUsersWithListInput**
```swift
    open class func createUsersWithListInput(user: [User], completion: @escaping (_ data: Void?, _ error: Error?) -> Void)
```

Creates list of users with given input array



### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let user = [User(id: 123, username: "username_example", firstName: "firstName_example", lastName: "lastName_example", email: "email_example", password: "password_example", phone: "phone_example", userStatus: 123)] // [User] | List of user object

// Creates list of users with given input array
UserAPI.createUsersWithListInput(user: user) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | [**[User]**](User.md) | List of user object | 

### Return type

Void (empty response body)

### Authorization

[auth_cookie](../README.md#auth_cookie)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **deleteUser**
```swift
    open class func deleteUser(username: String, completion: @escaping (_ data: Void?, _ error: Error?) -> Void)
```

Delete user

This can only be done by the logged in user.

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let username = "username_example" // String | The name that needs to be deleted

// Delete user
UserAPI.deleteUser(username: username) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String** | The name that needs to be deleted | 

### Return type

Void (empty response body)

### Authorization

[auth_cookie](../README.md#auth_cookie)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getUserByName**
```swift
    open class func getUserByName(username: String, completion: @escaping (_ data: User?, _ error: Error?) -> Void)
```

Get user by user name



### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let username = "username_example" // String | The name that needs to be fetched. Use user1 for testing.

// Get user by user name
UserAPI.getUserByName(username: username) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String** | The name that needs to be fetched. Use user1 for testing. | 

### Return type

[**User**](User.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **loginUser**
```swift
    open class func loginUser(username: String, password: String, completion: @escaping (_ data: String?, _ error: Error?) -> Void)
```

Logs user into the system



### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let username = "username_example" // String | The user name for login
let password = "password_example" // String | The password for login in clear text

// Logs user into the system
UserAPI.loginUser(username: username, password: password) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String** | The user name for login | 
 **password** | **String** | The password for login in clear text | 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **logoutUser**
```swift
    open class func logoutUser(completion: @escaping (_ data: Void?, _ error: Error?) -> Void)
```

Logs out current logged in user session



### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient


// Logs out current logged in user session
UserAPI.logoutUser() { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

Void (empty response body)

### Authorization

[auth_cookie](../README.md#auth_cookie)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updateUser**
```swift
    open class func updateUser(username: String, user: User, completion: @escaping (_ data: Void?, _ error: Error?) -> Void)
```

Updated user

This can only be done by the logged in user.

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let username = "username_example" // String | name that need to be deleted
let user = User(id: 123, username: "username_example", firstName: "firstName_example", lastName: "lastName_example", email: "email_example", password: "password_example", phone: "phone_example", userStatus: 123) // User | Updated user object

// Updated user
UserAPI.updateUser(username: username, user: user) { (response, error) in
    guard error == nil else {
        print(error)
        return
    }

    if (response) {
        dump(response)
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String** | name that need to be deleted | 
 **user** | [**User**](User.md) | Updated user object | 

### Return type

Void (empty response body)

### Authorization

[auth_cookie](../README.md#auth_cookie)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

