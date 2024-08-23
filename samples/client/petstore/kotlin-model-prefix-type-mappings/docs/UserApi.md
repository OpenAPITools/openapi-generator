# UserApi

All URIs are relative to *http://petstore.swagger.io/v2*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**createUser**](UserApi.md#createUser) | **POST** user | Create user |
| [**createUsersWithArrayInput**](UserApi.md#createUsersWithArrayInput) | **POST** user/createWithArray | Creates list of users with given input array |
| [**createUsersWithListInput**](UserApi.md#createUsersWithListInput) | **POST** user/createWithList | Creates list of users with given input array |
| [**deleteUser**](UserApi.md#deleteUser) | **DELETE** user/{username} | Delete user |
| [**getUserByName**](UserApi.md#getUserByName) | **GET** user/{username} | Get user by user name |
| [**loginUser**](UserApi.md#loginUser) | **GET** user/login | Logs user into the system |
| [**logoutUser**](UserApi.md#logoutUser) | **GET** user/logout | Logs out current logged in user session |
| [**updateUser**](UserApi.md#updateUser) | **PUT** user/{username} | Updated user |



Create user

This can only be done by the logged in user.

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(UserApi::class.java)
val apiUser : ApiUser =  // ApiUser | Created user object

launch(Dispatchers.IO) {
    webService.createUser(apiUser)
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **apiUser** | [**ApiUser**](ApiUser.md)| Created user object | |

### Return type

null (empty response body)

### Authorization



### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


Creates list of users with given input array



### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(UserApi::class.java)
val apiUser : kotlin.collections.List<ApiUser> =  // kotlin.collections.List<ApiUser> | List of user object

launch(Dispatchers.IO) {
    webService.createUsersWithArrayInput(apiUser)
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **apiUser** | [**kotlin.collections.List&lt;ApiUser&gt;**](ApiUser.md)| List of user object | |

### Return type

null (empty response body)

### Authorization



### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


Creates list of users with given input array



### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(UserApi::class.java)
val apiUser : kotlin.collections.List<ApiUser> =  // kotlin.collections.List<ApiUser> | List of user object

launch(Dispatchers.IO) {
    webService.createUsersWithListInput(apiUser)
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **apiUser** | [**kotlin.collections.List&lt;ApiUser&gt;**](ApiUser.md)| List of user object | |

### Return type

null (empty response body)

### Authorization



### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined


Delete user

This can only be done by the logged in user.

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(UserApi::class.java)
val username : kotlin.String = username_example // kotlin.String | The name that needs to be deleted

launch(Dispatchers.IO) {
    webService.deleteUser(username)
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **username** | **kotlin.String**| The name that needs to be deleted | |

### Return type

null (empty response body)

### Authorization



### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


Get user by user name



### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(UserApi::class.java)
val username : kotlin.String = username_example // kotlin.String | The name that needs to be fetched. Use user1 for testing.

launch(Dispatchers.IO) {
    val result : ApiUser = webService.getUserByName(username)
}
```

### Parameters
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **username** | **kotlin.String**| The name that needs to be fetched. Use user1 for testing. | |

### Return type

[**ApiUser**](ApiUser.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


Logs user into the system



### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(UserApi::class.java)
val username : kotlin.String = username_example // kotlin.String | The user name for login
val password : kotlin.String = password_example // kotlin.String | The password for login in clear text

launch(Dispatchers.IO) {
    val result : kotlin.String = webService.loginUser(username, password)
}
```

### Parameters
| **username** | **kotlin.String**| The user name for login | |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **password** | **kotlin.String**| The password for login in clear text | |

### Return type

**kotlin.String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json


Logs out current logged in user session



### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(UserApi::class.java)

launch(Dispatchers.IO) {
    webService.logoutUser()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

null (empty response body)

### Authorization



### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


Updated user

This can only be done by the logged in user.

### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(UserApi::class.java)
val username : kotlin.String = username_example // kotlin.String | name that need to be deleted
val apiUser : ApiUser =  // ApiUser | Updated user object

launch(Dispatchers.IO) {
    webService.updateUser(username, apiUser)
}
```

### Parameters
| **username** | **kotlin.String**| name that need to be deleted | |
| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **apiUser** | [**ApiUser**](ApiUser.md)| Updated user object | |

### Return type

null (empty response body)

### Authorization



### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

