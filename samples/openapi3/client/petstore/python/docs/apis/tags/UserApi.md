<a name="__pageTop"></a>
# petstore_api.apis.tags.user_api.UserApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**create_user**](#create_user) | **post** /user | Create user
[**create_users_with_array_input**](#create_users_with_array_input) | **post** /user/createWithArray | Creates list of users with given input array
[**create_users_with_list_input**](#create_users_with_list_input) | **post** /user/createWithList | Creates list of users with given input array
[**delete_user**](#delete_user) | **delete** /user/{username} | Delete user
[**get_user_by_name**](#get_user_by_name) | **get** /user/{username} | Get user by user name
[**login_user**](#login_user) | **get** /user/login | Logs user into the system
[**logout_user**](#logout_user) | **get** /user/logout | Logs out current logged in user session
[**update_user**](#update_user) | **put** /user/{username} | Updated user

# **create_user**
<a name="create_user"></a>
> create_user(user)

Create user

This can only be done by the logged in user.

### Example

```python
import petstore_api
from petstore_api.apis.tags import user_api
from petstore_api.model.user import User
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = user_api.UserApi(api_client)

    # example passing only required values which don't have defaults set
    body = User(
        id=1,
        username="username_example",
        first_name="first_name_example",
        last_name="last_name_example",
        email="email_example",
        password="password_example",
        phone="phone_example",
        user_status=1,
        object_with_no_declared_props=dict(),
        object_with_no_declared_props_nullable=dict(),
        any_type_prop=None,
        any_type_except_null_prop=None,
        any_type_prop_nullable=None,
    )
    try:
        # Create user
        api_response = api_instance.create_user(
            body=body,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling UserApi->create_user: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson] | required |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

# SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**User**](../../models/User.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
default | [ApiResponseForDefault](#create_user.ApiResponseForDefault) | successful operation

#### create_user.ApiResponseForDefault
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **create_users_with_array_input**
<a name="create_users_with_array_input"></a>
> create_users_with_array_input(user)

Creates list of users with given input array

### Example

```python
import petstore_api
from petstore_api.apis.tags import user_api
from petstore_api.model.user import User
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = user_api.UserApi(api_client)

    # example passing only required values which don't have defaults set
    body = [
        User(
            id=1,
            username="username_example",
            first_name="first_name_example",
            last_name="last_name_example",
            email="email_example",
            password="password_example",
            phone="phone_example",
            user_status=1,
            object_with_no_declared_props=dict(),
            object_with_no_declared_props_nullable=dict(),
            any_type_prop=None,
            any_type_except_null_prop=None,
            any_type_prop_nullable=None,
        )
    ]
    try:
        # Creates list of users with given input array
        api_response = api_instance.create_users_with_array_input(
            body=body,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling UserApi->create_users_with_array_input: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson] | required |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

# SchemaForRequestBodyApplicationJson

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
list, tuple,  | tuple,  |  | 

### Tuple Items
Class Name | Input Type | Accessed Type | Description | Notes
------------- | ------------- | ------------- | ------------- | -------------
[**User**]({{complexTypePrefix}}User.md) | [**User**]({{complexTypePrefix}}User.md) | [**User**]({{complexTypePrefix}}User.md) |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
default | [ApiResponseForDefault](#create_users_with_array_input.ApiResponseForDefault) | successful operation

#### create_users_with_array_input.ApiResponseForDefault
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **create_users_with_list_input**
<a name="create_users_with_list_input"></a>
> create_users_with_list_input(user)

Creates list of users with given input array

### Example

```python
import petstore_api
from petstore_api.apis.tags import user_api
from petstore_api.model.user import User
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = user_api.UserApi(api_client)

    # example passing only required values which don't have defaults set
    body = [
        User(
            id=1,
            username="username_example",
            first_name="first_name_example",
            last_name="last_name_example",
            email="email_example",
            password="password_example",
            phone="phone_example",
            user_status=1,
            object_with_no_declared_props=dict(),
            object_with_no_declared_props_nullable=dict(),
            any_type_prop=None,
            any_type_except_null_prop=None,
            any_type_prop_nullable=None,
        )
    ]
    try:
        # Creates list of users with given input array
        api_response = api_instance.create_users_with_list_input(
            body=body,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling UserApi->create_users_with_list_input: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson] | required |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

# SchemaForRequestBodyApplicationJson

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
list, tuple,  | tuple,  |  | 

### Tuple Items
Class Name | Input Type | Accessed Type | Description | Notes
------------- | ------------- | ------------- | ------------- | -------------
[**User**]({{complexTypePrefix}}User.md) | [**User**]({{complexTypePrefix}}User.md) | [**User**]({{complexTypePrefix}}User.md) |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
default | [ApiResponseForDefault](#create_users_with_list_input.ApiResponseForDefault) | successful operation

#### create_users_with_list_input.ApiResponseForDefault
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **delete_user**
<a name="delete_user"></a>
> delete_user(username)

Delete user

This can only be done by the logged in user.

### Example

```python
import petstore_api
from petstore_api.apis.tags import user_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = user_api.UserApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'username': "username_example",
    }
    try:
        # Delete user
        api_response = api_instance.delete_user(
            path_params=path_params,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling UserApi->delete_user: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
path_params | RequestPathParams | |
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
username | UsernameSchema | | 

# UsernameSchema

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
str,  | str,  |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#delete_user.ApiResponseFor200) | Success
404 | [ApiResponseFor404](#delete_user.ApiResponseFor404) | User not found

#### delete_user.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

#### delete_user.ApiResponseFor404
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **get_user_by_name**
<a name="get_user_by_name"></a>
> User get_user_by_name(username)

Get user by user name

### Example

```python
import petstore_api
from petstore_api.apis.tags import user_api
from petstore_api.model.user import User
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = user_api.UserApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'username': "username_example",
    }
    try:
        # Get user by user name
        api_response = api_instance.get_user_by_name(
            path_params=path_params,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling UserApi->get_user_by_name: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('application/xml', 'application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
username | UsernameSchema | | 

# UsernameSchema

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
str,  | str,  |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#get_user_by_name.ApiResponseFor200) | successful operation
400 | [ApiResponseFor400](#get_user_by_name.ApiResponseFor400) | Invalid username supplied
404 | [ApiResponseFor404](#get_user_by_name.ApiResponseFor404) | User not found

#### get_user_by_name.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationXml, SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationXml
Type | Description  | Notes
------------- | ------------- | -------------
[**User**](../../models/User.md) |  | 


# SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**User**](../../models/User.md) |  | 


#### get_user_by_name.ApiResponseFor400
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

#### get_user_by_name.ApiResponseFor404
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **login_user**
<a name="login_user"></a>
> str login_user(usernamepassword)

Logs user into the system

### Example

```python
import petstore_api
from petstore_api.apis.tags import user_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = user_api.UserApi(api_client)

    # example passing only required values which don't have defaults set
    query_params = {
        'username': "username_example",
        'password': "password_example",
    }
    try:
        # Logs user into the system
        api_response = api_instance.login_user(
            query_params=query_params,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling UserApi->login_user: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query_params | RequestQueryParams | |
accept_content_types | typing.Tuple[str] | default is ('application/xml', 'application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
username | UsernameSchema | | 
password | PasswordSchema | | 


# UsernameSchema

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
str,  | str,  |  | 

# PasswordSchema

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
str,  | str,  |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#login_user.ApiResponseFor200) | successful operation
400 | [ApiResponseFor400](#login_user.ApiResponseFor400) | Invalid username/password supplied

#### login_user.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationXml, SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | ResponseHeadersFor200 |  |

# SchemaFor200ResponseBodyApplicationXml

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
str,  | str,  |  | 

# SchemaFor200ResponseBodyApplicationJson

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
str,  | str,  |  | 
#### ResponseHeadersFor200

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
X-Rate-Limit | XRateLimitSchema | | optional
X-Expires-After | XExpiresAfterSchema | | optional

# XRateLimitSchema

calls per hour allowed by the user

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
decimal.Decimal, int,  | decimal.Decimal,  | calls per hour allowed by the user | value must be a 32 bit integer

# XExpiresAfterSchema

date in UTC when token expires

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
str, datetime,  | str,  | date in UTC when token expires | value must conform to RFC-3339 date-time


#### login_user.ApiResponseFor400
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **logout_user**
<a name="logout_user"></a>
> logout_user()

Logs out current logged in user session

### Example

```python
import petstore_api
from petstore_api.apis.tags import user_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = user_api.UserApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        # Logs out current logged in user session
        api_response = api_instance.logout_user()
    except petstore_api.ApiException as e:
        print("Exception when calling UserApi->logout_user: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
default | [ApiResponseForDefault](#logout_user.ApiResponseForDefault) | successful operation

#### logout_user.ApiResponseForDefault
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **update_user**
<a name="update_user"></a>
> update_user(usernameuser)

Updated user

This can only be done by the logged in user.

### Example

```python
import petstore_api
from petstore_api.apis.tags import user_api
from petstore_api.model.user import User
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = user_api.UserApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'username': "username_example",
    }
    body = User(
        id=1,
        username="username_example",
        first_name="first_name_example",
        last_name="last_name_example",
        email="email_example",
        password="password_example",
        phone="phone_example",
        user_status=1,
        object_with_no_declared_props=dict(),
        object_with_no_declared_props_nullable=dict(),
        any_type_prop=None,
        any_type_except_null_prop=None,
        any_type_prop_nullable=None,
    )
    try:
        # Updated user
        api_response = api_instance.update_user(
            path_params=path_params,
            body=body,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling UserApi->update_user: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson] | required |
path_params | RequestPathParams | |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

# SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**User**](../../models/User.md) |  | 


### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
username | UsernameSchema | | 

# UsernameSchema

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
str,  | str,  |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
400 | [ApiResponseFor400](#update_user.ApiResponseFor400) | Invalid user supplied
404 | [ApiResponseFor404](#update_user.ApiResponseFor404) | User not found

#### update_user.ApiResponseFor400
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

#### update_user.ApiResponseFor404
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

