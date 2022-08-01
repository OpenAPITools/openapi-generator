# edu_sharing_client_api.LTI v13Api

All URIs are relative to *http://localhost/edu-sharing/rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**generate_deep_linking_response**](LTI v13Api.md#generate_deep_linking_response) | **get** /lti/v13/generateDeepLinkingResponse | generate DeepLinkingResponse
[**jwks_uri**](LTI v13Api.md#jwks_uri) | **get** /lti/v13/jwks | LTI - returns repository JSON Web Key Sets
[**login_initiations**](LTI v13Api.md#login_initiations) | **post** /lti/v13/oidc/login_initiations | lti authentication process preparation.
[**lti**](LTI v13Api.md#lti) | **post** /lti/v13/lti13 | lti tool redirect.
[**lti_registration_dynamic**](LTI v13Api.md#lti_registration_dynamic) | **get** /lti/v13/registration/dynamic/{token} | LTI Dynamic Registration - Initiate registration
[**lti_registration_url**](LTI v13Api.md#lti_registration_url) | **get** /lti/v13/registration/url | LTI Dynamic Registration - generates url for platform
[**lti_target**](LTI v13Api.md#lti_target) | **post** /lti/v13/lti13/{nodeId} | lti tool resource link target.
[**register_by_type**](LTI v13Api.md#register_by_type) | **post** /lti/v13/registration/{type} | register LTI platform
[**register_test**](LTI v13Api.md#register_test) | **post** /lti/v13/registration/static | register LTI platform
[**remove_lti_registration_url**](LTI v13Api.md#remove_lti_registration_url) | **delete** /lti/v13/registration/url/{token} | LTI Dynamic Regitration - delete url

# **generate_deep_linking_response**
> NodeLTIDeepLink generate_deep_linking_response(node_ids)

generate DeepLinkingResponse

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import lti_v13_api
from edu_sharing_client_api.model.node_lti_deep_link import NodeLTIDeepLink
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
    api_instance = lti_v13_api.LTI v13Api(api_client)

    # example passing only required values which don't have defaults set
    query_params = {
        'nodeIds': [
        "nodeIds_example"
    ],
    }
    try:
        # generate DeepLinkingResponse
        api_response = api_instance.generate_deep_linking_response(
            query_params=query_params,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling LTI v13Api->generate_deep_linking_response: %s\n" % e)
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
nodeIds | NodeIdsSchema | | 


#### NodeIdsSchema

Type | Description | Notes
------------- | ------------- | -------------
**[str]** |  | 

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
[**NodeLTIDeepLink**](NodeLTIDeepLink.md) |  | 


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



[**NodeLTIDeepLink**](NodeLTIDeepLink.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **jwks_uri**
> RegistrationUrl jwks_uri()

LTI - returns repository JSON Web Key Sets

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import lti_v13_api
from edu_sharing_client_api.model.registration_url import RegistrationUrl
from pprint import pprint
# Defining the host is optional and defaults to http://localhost/edu-sharing/rest
# See configuration.py for a list of all supported configuration parameters.
configuration = edu_sharing_client_api.Configuration(
    host = "http://localhost/edu-sharing/rest"
)

# Enter a context with an instance of the API client
with edu_sharing_client_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = lti_v13_api.LTI v13Api(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        # LTI - returns repository JSON Web Key Sets
        api_response = api_instance.jwks_uri()
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling LTI v13Api->jwks_uri: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

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
[**RegistrationUrl**](RegistrationUrl.md) |  | 


#### ApiResponseFor400
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor400ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor400ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor401
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor401ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor401ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor403
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor403ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor403ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor404
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor404ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor404ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor500
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor500ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor500ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 


[**RegistrationUrl**](RegistrationUrl.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **login_initiations**
> str login_initiations()

lti authentication process preparation.

preflight phase. prepares lti authentication process. checks it issuer is valid

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import lti_v13_api
from pprint import pprint
# Defining the host is optional and defaults to http://localhost/edu-sharing/rest
# See configuration.py for a list of all supported configuration parameters.
configuration = edu_sharing_client_api.Configuration(
    host = "http://localhost/edu-sharing/rest"
)

# Enter a context with an instance of the API client
with edu_sharing_client_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = lti_v13_api.LTI v13Api(api_client)

    # example passing only optional values
    body = dict(
        iss="iss_example",
        target_link_uri="target_link_uri_example",
        client_id="client_id_example",
        login_hint="login_hint_example",
        lti_message_hint="lti_message_hint_example",
        lti_deployment_id="lti_deployment_id_example",
    )
    try:
        # lti authentication process preparation.
        api_response = api_instance.login_initiations(
            body=body,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling LTI v13Api->login_initiations: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationXWwwFormUrlencoded, Unset] | optional, default is unset |
content_type | str | optional, default is 'application/x-www-form-urlencoded' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('text/html', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationXWwwFormUrlencoded

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**iss** | **str** | Issuer of the request, will be validated | 
**target_link_uri** | **str** | target url of platform at the end of the flow | 
**client_id** | **str** | Id of the issuer | [optional] 
**login_hint** | **str** | context information of the platform | [optional] 
**lti_message_hint** | **str** | additional context information of the platform | [optional] 
**lti_deployment_id** | **str** | A can have multiple deployments in a platform | [optional] 
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

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
body | typing.Union[SchemaFor200ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor400
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor400ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor400ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor401
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor401ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor401ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor403
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor403ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor403ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor404
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor404ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor404ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor500
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor500ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor500ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 


**str**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **lti**
> str lti()

lti tool redirect.

lti tool redirect

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import lti_v13_api
from pprint import pprint
# Defining the host is optional and defaults to http://localhost/edu-sharing/rest
# See configuration.py for a list of all supported configuration parameters.
configuration = edu_sharing_client_api.Configuration(
    host = "http://localhost/edu-sharing/rest"
)

# Enter a context with an instance of the API client
with edu_sharing_client_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = lti_v13_api.LTI v13Api(api_client)

    # example passing only optional values
    body = dict(
        id_token="id_token_example",
        state="state_example",
    )
    try:
        # lti tool redirect.
        api_response = api_instance.lti(
            body=body,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling LTI v13Api->lti: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationXWwwFormUrlencoded, Unset] | optional, default is unset |
content_type | str | optional, default is 'application/x-www-form-urlencoded' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('text/html', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationXWwwFormUrlencoded

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id_token** | **str** | Issuer of the request, will be validated | 
**state** | **str** | Issuer of the request, will be validated | 
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

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
body | typing.Union[SchemaFor200ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor400
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor400ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor400ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor401
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor401ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor401ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor403
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor403ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor403ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor404
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor404ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor404ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor500
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor500ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor500ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 


**str**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **lti_registration_dynamic**
> str lti_registration_dynamic(openid_configurationtoken)

LTI Dynamic Registration - Initiate registration

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import lti_v13_api
from pprint import pprint
# Defining the host is optional and defaults to http://localhost/edu-sharing/rest
# See configuration.py for a list of all supported configuration parameters.
configuration = edu_sharing_client_api.Configuration(
    host = "http://localhost/edu-sharing/rest"
)

# Enter a context with an instance of the API client
with edu_sharing_client_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = lti_v13_api.LTI v13Api(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'token': "token_example",
    }
    query_params = {
        'openid_configuration': "openid_configuration_example",
    }
    try:
        # LTI Dynamic Registration - Initiate registration
        api_response = api_instance.lti_registration_dynamic(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling LTI v13Api->lti_registration_dynamic: %s\n" % e)

    # example passing only optional values
    path_params = {
        'token': "token_example",
    }
    query_params = {
        'openid_configuration': "openid_configuration_example",
        'registration_token': "registration_token_example",
    }
    try:
        # LTI Dynamic Registration - Initiate registration
        api_response = api_instance.lti_registration_dynamic(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling LTI v13Api->lti_registration_dynamic: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query_params | RequestQueryParams | |
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('text/html', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
openid_configuration | OpenidConfigurationSchema | | 
registration_token | RegistrationTokenSchema | | optional


#### OpenidConfigurationSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### RegistrationTokenSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
token | TokenSchema | | 

#### TokenSchema

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
body | typing.Union[SchemaFor200ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor400
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor400ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor400ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor401
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor401ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor401ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor403
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor403ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor403ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor404
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor404ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor404ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor500
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor500ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor500ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 


**str**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **lti_registration_url**
> DynamicRegistrationTokens lti_registration_url()

LTI Dynamic Registration - generates url for platform

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import lti_v13_api
from edu_sharing_client_api.model.dynamic_registration_tokens import DynamicRegistrationTokens
from pprint import pprint
# Defining the host is optional and defaults to http://localhost/edu-sharing/rest
# See configuration.py for a list of all supported configuration parameters.
configuration = edu_sharing_client_api.Configuration(
    host = "http://localhost/edu-sharing/rest"
)

# Enter a context with an instance of the API client
with edu_sharing_client_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = lti_v13_api.LTI v13Api(api_client)

    # example passing only required values which don't have defaults set
    query_params = {
        'generate': False,
    }
    try:
        # LTI Dynamic Registration - generates url for platform
        api_response = api_instance.lti_registration_url(
            query_params=query_params,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling LTI v13Api->lti_registration_url: %s\n" % e)
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
generate | GenerateSchema | | 


#### GenerateSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | defaults to False

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
[**DynamicRegistrationTokens**](DynamicRegistrationTokens.md) |  | 


#### ApiResponseFor400
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor400ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor400ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor401
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor401ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor401ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor403
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor403ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor403ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor404
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor404ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor404ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor500
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor500ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor500ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 


[**DynamicRegistrationTokens**](DynamicRegistrationTokens.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **lti_target**
> str lti_target(node_id)

lti tool resource link target.

used by some platforms for direct (without oidc login_init) launch requests

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import lti_v13_api
from pprint import pprint
# Defining the host is optional and defaults to http://localhost/edu-sharing/rest
# See configuration.py for a list of all supported configuration parameters.
configuration = edu_sharing_client_api.Configuration(
    host = "http://localhost/edu-sharing/rest"
)

# Enter a context with an instance of the API client
with edu_sharing_client_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = lti_v13_api.LTI v13Api(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'nodeId': "nodeId_example",
    }
    try:
        # lti tool resource link target.
        api_response = api_instance.lti_target(
            path_params=path_params,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling LTI v13Api->lti_target: %s\n" % e)

    # example passing only optional values
    path_params = {
        'nodeId': "nodeId_example",
    }
    body = dict(
        id_token="id_token_example",
        state="state_example",
    )
    try:
        # lti tool resource link target.
        api_response = api_instance.lti_target(
            path_params=path_params,
            body=body,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling LTI v13Api->lti_target: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationXWwwFormUrlencoded, Unset] | optional, default is unset |
path_params | RequestPathParams | |
content_type | str | optional, default is 'application/x-www-form-urlencoded' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('text/html', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationXWwwFormUrlencoded

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id_token** | **str** | Issuer of the request, will be validated | 
**state** | **str** | Issuer of the request, will be validated | 
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
nodeId | NodeIdSchema | | 

#### NodeIdSchema

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
body | typing.Union[SchemaFor200ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor400
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor400ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor400ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor401
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor401ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor401ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor403
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor403ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor403ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor404
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor404ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor404ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor500
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor500ResponseBodyTextHtml, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor500ResponseBodyTextHtml

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 


**str**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **register_by_type**
> register_by_type(typebase_url)

register LTI platform

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import lti_v13_api
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
    api_instance = lti_v13_api.LTI v13Api(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'type': "moodle",
    }
    query_params = {
        'baseUrl': "baseUrl_example",
    }
    try:
        # register LTI platform
        api_response = api_instance.register_by_type(
            path_params=path_params,
            query_params=query_params,
        )
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling LTI v13Api->register_by_type: %s\n" % e)

    # example passing only optional values
    path_params = {
        'type': "moodle",
    }
    query_params = {
        'baseUrl': "baseUrl_example",
        'client_id': "client_id_example",
        'deployment_id': "deployment_id_example",
    }
    try:
        # register LTI platform
        api_response = api_instance.register_by_type(
            path_params=path_params,
            query_params=query_params,
        )
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling LTI v13Api->register_by_type: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query_params | RequestQueryParams | |
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
baseUrl | BaseUrlSchema | | 
client_id | ClientIdSchema | | optional
deployment_id | DeploymentIdSchema | | optional


#### BaseUrlSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ClientIdSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### DeploymentIdSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
type | TypeSchema | | 

#### TypeSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  |  must be one of ["moodle", ]

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
body | typing.Union[Unset, ] |  |
headers | Unset | headers were not defined |

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



void (empty response body)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **register_test**
> register_test(platform_idclient_iddeployment_idauthentication_request_urlkeyset_urlauth_token_url)

register LTI platform

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import lti_v13_api
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
    api_instance = lti_v13_api.LTI v13Api(api_client)

    # example passing only required values which don't have defaults set
    query_params = {
        'platformId': "platformId_example",
        'client_id': "client_id_example",
        'deployment_id': "deployment_id_example",
        'authentication_request_url': "authentication_request_url_example",
        'keyset_url': "keyset_url_example",
        'auth_token_url': "auth_token_url_example",
    }
    try:
        # register LTI platform
        api_response = api_instance.register_test(
            query_params=query_params,
        )
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling LTI v13Api->register_test: %s\n" % e)

    # example passing only optional values
    query_params = {
        'platformId': "platformId_example",
        'client_id': "client_id_example",
        'deployment_id': "deployment_id_example",
        'authentication_request_url': "authentication_request_url_example",
        'keyset_url': "keyset_url_example",
        'key_id': "key_id_example",
        'auth_token_url': "auth_token_url_example",
    }
    try:
        # register LTI platform
        api_response = api_instance.register_test(
            query_params=query_params,
        )
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling LTI v13Api->register_test: %s\n" % e)
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
platformId | PlatformIdSchema | | 
client_id | ClientIdSchema | | 
deployment_id | DeploymentIdSchema | | 
authentication_request_url | AuthenticationRequestUrlSchema | | 
keyset_url | KeysetUrlSchema | | 
key_id | KeyIdSchema | | optional
auth_token_url | AuthTokenUrlSchema | | 


#### PlatformIdSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ClientIdSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### DeploymentIdSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### AuthenticationRequestUrlSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### KeysetUrlSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### KeyIdSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### AuthTokenUrlSchema

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
body | typing.Union[Unset, ] |  |
headers | Unset | headers were not defined |

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



void (empty response body)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **remove_lti_registration_url**
> DynamicRegistrationTokens remove_lti_registration_url(token)

LTI Dynamic Regitration - delete url

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import lti_v13_api
from edu_sharing_client_api.model.dynamic_registration_tokens import DynamicRegistrationTokens
from pprint import pprint
# Defining the host is optional and defaults to http://localhost/edu-sharing/rest
# See configuration.py for a list of all supported configuration parameters.
configuration = edu_sharing_client_api.Configuration(
    host = "http://localhost/edu-sharing/rest"
)

# Enter a context with an instance of the API client
with edu_sharing_client_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = lti_v13_api.LTI v13Api(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'token': "token_example",
    }
    try:
        # LTI Dynamic Regitration - delete url
        api_response = api_instance.remove_lti_registration_url(
            path_params=path_params,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling LTI v13Api->remove_lti_registration_url: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
token | TokenSchema | | 

#### TokenSchema

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
[**DynamicRegistrationTokens**](DynamicRegistrationTokens.md) |  | 


#### ApiResponseFor400
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor400ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor400ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor401
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor401ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor401ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor403
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor403ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor403ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor404
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor404ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor404ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ApiResponseFor500
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor500ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor500ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 


[**DynamicRegistrationTokens**](DynamicRegistrationTokens.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

