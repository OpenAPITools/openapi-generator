# UserApi

All URIs are by default relative to `http://petstore.swagger.io/v2`

Method | HTTP request | Description
------------- | ------------- | -------------
[`createUser`](#createUser) | `POST` /user | Create user
[`createUsersWithArrayInput`](#createUsersWithArrayInput) | `POST` /user/createWithArray | Creates list of users with given input array
[`createUsersWithListInput`](#createUsersWithListInput) | `POST` /user/createWithList | Creates list of users with given input array
[`deleteUser`](#deleteUser) | `DELETE` /user/{username} | Delete user
[`getUserByName`](#getUserByName) | `GET` /user/{username} | Get user by user name
[`loginUser`](#loginUser) | `GET` /user/login | Logs user into the system
[`logoutUser`](#logoutUser) | `GET` /user/logout | Logs out current logged in user session
[`updateUser`](#updateUser) | `PUT` /user/{username} | Updated user


# **createUser**
> createUser(body)

Create user

This can only be done by the logged in user.

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.UserApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val userApi = UserApi()
val body: User =  
try {
    userApi.createUser(
        body,
    )
} catch (e: ClientException) {
    println("4xx response calling UserApi#createUser")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling UserApi#createUser")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**User**](User.md)| Created user object |

### Return type

`Unit`

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

# **createUsersWithArrayInput**
> createUsersWithArrayInput(body)

Creates list of users with given input array

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.UserApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val userApi = UserApi()
val body: kotlin.collections.List<User> =  
try {
    userApi.createUsersWithArrayInput(
        body,
    )
} catch (e: ClientException) {
    println("4xx response calling UserApi#createUsersWithArrayInput")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling UserApi#createUsersWithArrayInput")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**kotlin.collections.List&lt;User&gt;**](User.md)| List of user object |

### Return type

`Unit`

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

# **createUsersWithListInput**
> createUsersWithListInput(body)

Creates list of users with given input array

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.UserApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val userApi = UserApi()
val body: kotlin.collections.List<User> =  
try {
    userApi.createUsersWithListInput(
        body,
    )
} catch (e: ClientException) {
    println("4xx response calling UserApi#createUsersWithListInput")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling UserApi#createUsersWithListInput")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**kotlin.collections.List&lt;User&gt;**](User.md)| List of user object |

### Return type

`Unit`

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

# **deleteUser**
> deleteUser(username)

Delete user

This can only be done by the logged in user.

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.UserApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val userApi = UserApi()
val username: kotlin.String = username_example 
try {
    userApi.deleteUser(
        username,
    )
} catch (e: ClientException) {
    println("4xx response calling UserApi#deleteUser")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling UserApi#deleteUser")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **kotlin.String**| The name that needs to be deleted |

### Return type

`Unit`

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

# **getUserByName**
> User getUserByName(username)

Get user by user name

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.UserApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val userApi = UserApi()
val username: kotlin.String = username_example 
try {
    val result: User = userApi.getUserByName(
        username,
    )
    println(result)
} catch (e: ClientException) {
    println("4xx response calling UserApi#getUserByName")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling UserApi#getUserByName")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **kotlin.String**| The name that needs to be fetched. Use user1 for testing. |

### Return type

[`User`](User.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xmlapplication/json

# **loginUser**
> kotlin.String loginUser(username, password)

Logs user into the system

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.UserApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val userApi = UserApi()
val username: kotlin.String = username_example 
val password: kotlin.String = password_example 
try {
    val result: kotlin.String = userApi.loginUser(
        username,
        password,
    )
    println(result)
} catch (e: ClientException) {
    println("4xx response calling UserApi#loginUser")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling UserApi#loginUser")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **kotlin.String**| The user name for login |
 **password** | **kotlin.String**| The password for login in clear text |

### Return type

`kotlin.String`

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xmlapplication/json

# **logoutUser**
> logoutUser()

Logs out current logged in user session

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.UserApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val userApi = UserApi()
try {
    userApi.logoutUser(
    )
} catch (e: ClientException) {
    println("4xx response calling UserApi#logoutUser")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling UserApi#logoutUser")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

`Unit`

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

# **updateUser**
> updateUser(username, body)

Updated user

This can only be done by the logged in user.

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.UserApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val userApi = UserApi()
val username: kotlin.String = username_example 
val body: User =  
try {
    userApi.updateUser(
        username,
        body,
    )
} catch (e: ClientException) {
    println("4xx response calling UserApi#updateUser")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling UserApi#updateUser")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **kotlin.String**| name that need to be deleted |
 **body** | [**User**](User.md)| Updated user object |

### Return type

`Unit`

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

