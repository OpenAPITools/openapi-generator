# Swagger\Server\Api\UserApiInterface

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createUser**](UserApiInterface.md#createUser) | **POST** /user | Create user
[**createUsersWithArrayInput**](UserApiInterface.md#createUsersWithArrayInput) | **POST** /user/createWithArray | Creates list of users with given input array
[**createUsersWithListInput**](UserApiInterface.md#createUsersWithListInput) | **POST** /user/createWithList | Creates list of users with given input array
[**deleteUser**](UserApiInterface.md#deleteUser) | **DELETE** /user/{username} | Delete user
[**getUserByName**](UserApiInterface.md#getUserByName) | **GET** /user/{username} | Get user by user name
[**loginUser**](UserApiInterface.md#loginUser) | **GET** /user/login | Logs user into the system
[**logoutUser**](UserApiInterface.md#logoutUser) | **GET** /user/logout | Logs out current logged in user session
[**updateUser**](UserApiInterface.md#updateUser) | **PUT** /user/{username} | Updated user


## Service Declaration
```yaml
# src/Acme/MyBundle/Resources/services.yml
services:
    # ...
    acme.my_bundle.api.user:
        class: Acme\MyBundle\Api\UserApi
        tags:
            - { name: "swagger_server.api", api: "user" }
    # ...
```

## **createUser**
> createUser($body)

Create user

This can only be done by the logged in user.

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/UserApiInterface.php

namespace Acme\MyBundle\Api;

use Swagger\Server\Api\UserApiInterface;

class UserApi implements UserApiInterface
{

    // ...

    /**
     * Implementation of UserApiInterface#createUser
     */
    public function createUser(User $body)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Swagger\Server\Model\User**](../Model/User.md)| Created user object |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **createUsersWithArrayInput**
> createUsersWithArrayInput($body)

Creates list of users with given input array



### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/UserApiInterface.php

namespace Acme\MyBundle\Api;

use Swagger\Server\Api\UserApiInterface;

class UserApi implements UserApiInterface
{

    // ...

    /**
     * Implementation of UserApiInterface#createUsersWithArrayInput
     */
    public function createUsersWithArrayInput(array $body)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Swagger\Server\Model\User**](../Model/User.md)| List of user object |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **createUsersWithListInput**
> createUsersWithListInput($body)

Creates list of users with given input array



### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/UserApiInterface.php

namespace Acme\MyBundle\Api;

use Swagger\Server\Api\UserApiInterface;

class UserApi implements UserApiInterface
{

    // ...

    /**
     * Implementation of UserApiInterface#createUsersWithListInput
     */
    public function createUsersWithListInput(array $body)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Swagger\Server\Model\User**](../Model/User.md)| List of user object |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **deleteUser**
> deleteUser($username)

Delete user

This can only be done by the logged in user.

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/UserApiInterface.php

namespace Acme\MyBundle\Api;

use Swagger\Server\Api\UserApiInterface;

class UserApi implements UserApiInterface
{

    // ...

    /**
     * Implementation of UserApiInterface#deleteUser
     */
    public function deleteUser($username)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| The name that needs to be deleted |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **getUserByName**
> Swagger\Server\Model\User getUserByName($username)

Get user by user name



### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/UserApiInterface.php

namespace Acme\MyBundle\Api;

use Swagger\Server\Api\UserApiInterface;

class UserApi implements UserApiInterface
{

    // ...

    /**
     * Implementation of UserApiInterface#getUserByName
     */
    public function getUserByName($username)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| The name that needs to be fetched. Use user1 for testing. |

### Return type

[**Swagger\Server\Model\User**](../Model/User.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **loginUser**
> string loginUser($username, $password)

Logs user into the system



### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/UserApiInterface.php

namespace Acme\MyBundle\Api;

use Swagger\Server\Api\UserApiInterface;

class UserApi implements UserApiInterface
{

    // ...

    /**
     * Implementation of UserApiInterface#loginUser
     */
    public function loginUser($username, $password)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| The user name for login |
 **password** | **string**| The password for login in clear text |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **logoutUser**
> logoutUser()

Logs out current logged in user session



### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/UserApiInterface.php

namespace Acme\MyBundle\Api;

use Swagger\Server\Api\UserApiInterface;

class UserApi implements UserApiInterface
{

    // ...

    /**
     * Implementation of UserApiInterface#logoutUser
     */
    public function logoutUser()
    {
        // Implement the operation ...
    }

    // ...
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

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **updateUser**
> updateUser($username, $body)

Updated user

This can only be done by the logged in user.

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/UserApiInterface.php

namespace Acme\MyBundle\Api;

use Swagger\Server\Api\UserApiInterface;

class UserApi implements UserApiInterface
{

    // ...

    /**
     * Implementation of UserApiInterface#updateUser
     */
    public function updateUser($username, User $body)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| name that need to be deleted |
 **body** | [**Swagger\Server\Model\User**](../Model/User.md)| Updated user object |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

