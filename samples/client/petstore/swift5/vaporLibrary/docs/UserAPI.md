# UserAPI

All URIs are relative to *http://petstore.swagger.io:80/v2*

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
    open class func createUser(body: User, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<CreateUser>
```

Create user

This can only be done by the logged in user.

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let body = User(id: 123, username: "username_example", firstName: "firstName_example", lastName: "lastName_example", email: "email_example", password: "password_example", phone: "phone_example", userStatus: 123) // User | Created user object

// Create user
UserAPI.createUser(body: body).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**User**](User.md) | Created user object | 

### Return type

#### CreateUser

```swift
public enum CreateUser {
    case http0(value: Void?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **createUsersWithArrayInput**
```swift
    open class func createUsersWithArrayInput(body: [User], headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<CreateUsersWithArrayInput>
```

Creates list of users with given input array

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let body = [User(id: 123, username: "username_example", firstName: "firstName_example", lastName: "lastName_example", email: "email_example", password: "password_example", phone: "phone_example", userStatus: 123)] // [User] | List of user object

// Creates list of users with given input array
UserAPI.createUsersWithArrayInput(body: body).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**[User]**](User.md) | List of user object | 

### Return type

#### CreateUsersWithArrayInput

```swift
public enum CreateUsersWithArrayInput {
    case http0(value: Void?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **createUsersWithListInput**
```swift
    open class func createUsersWithListInput(body: [User], headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<CreateUsersWithListInput>
```

Creates list of users with given input array

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let body = [User(id: 123, username: "username_example", firstName: "firstName_example", lastName: "lastName_example", email: "email_example", password: "password_example", phone: "phone_example", userStatus: 123)] // [User] | List of user object

// Creates list of users with given input array
UserAPI.createUsersWithListInput(body: body).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**[User]**](User.md) | List of user object | 

### Return type

#### CreateUsersWithListInput

```swift
public enum CreateUsersWithListInput {
    case http0(value: Void?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **deleteUser**
```swift
    open class func deleteUser(username: String, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<DeleteUser>
```

Delete user

This can only be done by the logged in user.

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let username = "username_example" // String | The name that needs to be deleted

// Delete user
UserAPI.deleteUser(username: username).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http400(let value, let raw):
        case .http404(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String** | The name that needs to be deleted | 

### Return type

#### DeleteUser

```swift
public enum DeleteUser {
    case http400(value: Void?, raw: ClientResponse)
    case http404(value: Void?, raw: ClientResponse)
    case http0(value: Void?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getUserByName**
```swift
    open class func getUserByName(username: String, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<GetUserByName>
```

Get user by user name

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let username = "username_example" // String | The name that needs to be fetched. Use user1 for testing.

// Get user by user name
UserAPI.getUserByName(username: username).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http200(let value, let raw):
        case .http400(let value, let raw):
        case .http404(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String** | The name that needs to be fetched. Use user1 for testing. | 

### Return type

#### GetUserByName

```swift
public enum GetUserByName {
    case http200(value: User?, raw: ClientResponse)
    case http400(value: Void?, raw: ClientResponse)
    case http404(value: Void?, raw: ClientResponse)
    case http0(value: User?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **loginUser**
```swift
    open class func loginUser(username: String, password: String, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<LoginUser>
```

Logs user into the system

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let username = "username_example" // String | The user name for login
let password = "password_example" // String | The password for login in clear text

// Logs user into the system
UserAPI.loginUser(username: username, password: password).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http200(let value, let raw):
        case .http400(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String** | The user name for login | 
 **password** | **String** | The password for login in clear text | 

### Return type

#### LoginUser

```swift
public enum LoginUser {
    case http200(value: String?, raw: ClientResponse)
    case http400(value: Void?, raw: ClientResponse)
    case http0(value: String?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **logoutUser**
```swift
    open class func logoutUser(headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<LogoutUser>
```

Logs out current logged in user session

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient


// Logs out current logged in user session
UserAPI.logoutUser().whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

#### LogoutUser

```swift
public enum LogoutUser {
    case http0(value: Void?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updateUser**
```swift
    open class func updateUser(username: String, body: User, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<UpdateUser>
```

Updated user

This can only be done by the logged in user.

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let username = "username_example" // String | name that need to be deleted
let body = User(id: 123, username: "username_example", firstName: "firstName_example", lastName: "lastName_example", email: "email_example", password: "password_example", phone: "phone_example", userStatus: 123) // User | Updated user object

// Updated user
UserAPI.updateUser(username: username, body: body).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http400(let value, let raw):
        case .http404(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String** | name that need to be deleted | 
 **body** | [**User**](User.md) | Updated user object | 

### Return type

#### UpdateUser

```swift
public enum UpdateUser {
    case http400(value: Void?, raw: ClientResponse)
    case http404(value: Void?, raw: ClientResponse)
    case http0(value: Void?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

