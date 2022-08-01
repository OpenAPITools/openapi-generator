# edu_sharing_client_api.AUTHENTICATION v1Api

All URIs are relative to *http://localhost/edu-sharing/rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**authenticate**](AUTHENTICATION v1Api.md#authenticate) | **post** /authentication/v1/appauth/{userId} | authenticate user of an registered application.
[**has_access_to_scope**](AUTHENTICATION v1Api.md#has_access_to_scope) | **get** /authentication/v1/hasAccessToScope | Returns true if the current user has access to the given scope
[**login**](AUTHENTICATION v1Api.md#login) | **get** /authentication/v1/validateSession | Validates the Basic Auth Credentials and check if the session is a logged in user
[**login_to_scope**](AUTHENTICATION v1Api.md#login_to_scope) | **post** /authentication/v1/loginToScope | Validates the Basic Auth Credentials and check if the session is a logged in user
[**logout**](AUTHENTICATION v1Api.md#logout) | **get** /authentication/v1/destroySession | Destroys the current session and logout the user

# **authenticate**
> AuthenticationToken authenticate(user_id)

authenticate user of an registered application.

headers must be set: X-Edu-App-Id, X-Edu-App-Sig, X-Edu-App-Signed, X-Edu-App-Ts

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import authentication_v1_api
from edu_sharing_client_api.model.authentication_token import AuthenticationToken
from edu_sharing_client_api.model.user_profile_app_auth import UserProfileAppAuth
from edu_sharing_client_api.model.error_response import ErrorResponse
from pprint import pprint
# Defining the host is optional and defaults to http://localhost/edu-sharing/rest
# See configuration.py for a list of all supported configuration parameters.
configuration = edu_sharing_client_api.Configuration(
    host = "http://localhost/edu-sharing/rest"
)

# Enter a context with an instance of the API client
with edu_sharing_client_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = authentication_v1_api.AUTHENTICATION v1Api(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'userId': "userId_example",
    }
    try:
        # authenticate user of an registered application.
        api_response = api_instance.authenticate(
            path_params=path_params,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling AUTHENTICATION v1Api->authenticate: %s\n" % e)

    # example passing only optional values
    path_params = {
        'userId': "userId_example",
    }
    body = UserProfileAppAuth(
        primary_affiliation="primary_affiliation_example",
        skills=[
            "skills_example"
        ],
        types=[],
        extended_attributes=dict(
            "key": [],
        ),
        vcard="vcard_example",
        type=[
            "type_example"
        ],
        first_name="first_name_example",
        last_name="last_name_example",
        email="email_example",
        avatar="avatar_example",
        about="about_example",
    )
    try:
        # authenticate user of an registered application.
        api_response = api_instance.authenticate(
            path_params=path_params,
            body=body,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling AUTHENTICATION v1Api->authenticate: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
path_params | RequestPathParams | |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**UserProfileAppAuth**](UserProfileAppAuth.md) |  | 


### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
userId | UserIdSchema | | 

#### UserIdSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | OK.
400 | ApiResponseFor400 | Preconditions are not present.
401 | ApiResponseFor401 | Authorization failed.
403 | ApiResponseFor403 | Session user has insufficient rights to perform this operation.
404 | ApiResponseFor404 | Ressources are not found.
500 | ApiResponseFor500 | Fatal error occured.

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AuthenticationToken**](AuthenticationToken.md) |  | 


#### ApiResponseFor400
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor400ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor400ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ErrorResponse**](ErrorResponse.md) |  | 


#### ApiResponseFor401
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor401ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor401ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ErrorResponse**](ErrorResponse.md) |  | 


#### ApiResponseFor403
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor403ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor403ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ErrorResponse**](ErrorResponse.md) |  | 


#### ApiResponseFor404
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor404ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor404ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ErrorResponse**](ErrorResponse.md) |  | 


#### ApiResponseFor500
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor500ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor500ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ErrorResponse**](ErrorResponse.md) |  | 



[**AuthenticationToken**](AuthenticationToken.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **has_access_to_scope**
> has_access_to_scope(scope)

Returns true if the current user has access to the given scope

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import authentication_v1_api
from pprint import pprint
# Defining the host is optional and defaults to http://localhost/edu-sharing/rest
# See configuration.py for a list of all supported configuration parameters.
configuration = edu_sharing_client_api.Configuration(
    host = "http://localhost/edu-sharing/rest"
)

# Enter a context with an instance of the API client
with edu_sharing_client_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = authentication_v1_api.AUTHENTICATION v1Api(api_client)

    # example passing only required values which don't have defaults set
    query_params = {
        'scope': "scope_example",
    }
    try:
        # Returns true if the current user has access to the given scope
        api_response = api_instance.has_access_to_scope(
            query_params=query_params,
        )
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling AUTHENTICATION v1Api->has_access_to_scope: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query_params | RequestQueryParams | |
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
scope | ScopeSchema | | 


#### ScopeSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | OK.
500 | ApiResponseFor500 | Fatal error occured.

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[Unset, ] |  |
headers | Unset | headers were not defined |

#### ApiResponseFor500
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[Unset, ] |  |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **login**
> Login login()

Validates the Basic Auth Credentials and check if the session is a logged in user

Use the Basic auth header field to transfer the credentials

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import authentication_v1_api
from edu_sharing_client_api.model.login import Login
from pprint import pprint
# Defining the host is optional and defaults to http://localhost/edu-sharing/rest
# See configuration.py for a list of all supported configuration parameters.
configuration = edu_sharing_client_api.Configuration(
    host = "http://localhost/edu-sharing/rest"
)

# Enter a context with an instance of the API client
with edu_sharing_client_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = authentication_v1_api.AUTHENTICATION v1Api(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        # Validates the Basic Auth Credentials and check if the session is a logged in user
        api_response = api_instance.login()
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling AUTHENTICATION v1Api->login: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | OK.

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**Login**](Login.md) |  | 



[**Login**](Login.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **login_to_scope**
> Login login_to_scope(login_credentials)

Validates the Basic Auth Credentials and check if the session is a logged in user

Use the Basic auth header field to transfer the credentials

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import authentication_v1_api
from edu_sharing_client_api.model.login import Login
from edu_sharing_client_api.model.login_credentials import LoginCredentials
from pprint import pprint
# Defining the host is optional and defaults to http://localhost/edu-sharing/rest
# See configuration.py for a list of all supported configuration parameters.
configuration = edu_sharing_client_api.Configuration(
    host = "http://localhost/edu-sharing/rest"
)

# Enter a context with an instance of the API client
with edu_sharing_client_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = authentication_v1_api.AUTHENTICATION v1Api(api_client)

    # example passing only required values which don't have defaults set
    body = LoginCredentials(
        user_name="user_name_example",
        password="password_example",
        scope="scope_example",
    )
    try:
        # Validates the Basic Auth Credentials and check if the session is a logged in user
        api_response = api_instance.login_to_scope(
            body=body,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling AUTHENTICATION v1Api->login_to_scope: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson] | required |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**LoginCredentials**](LoginCredentials.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | OK.

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**Login**](Login.md) |  | 



[**Login**](Login.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **logout**
> logout()

Destroys the current session and logout the user

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import authentication_v1_api
from pprint import pprint
# Defining the host is optional and defaults to http://localhost/edu-sharing/rest
# See configuration.py for a list of all supported configuration parameters.
configuration = edu_sharing_client_api.Configuration(
    host = "http://localhost/edu-sharing/rest"
)

# Enter a context with an instance of the API client
with edu_sharing_client_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = authentication_v1_api.AUTHENTICATION v1Api(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        # Destroys the current session and logout the user
        api_response = api_instance.logout()
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling AUTHENTICATION v1Api->logout: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | OK.
500 | ApiResponseFor500 | Fatal error occured.

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[Unset, ] |  |
headers | Unset | headers were not defined |

#### ApiResponseFor500
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[Unset, ] |  |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

