# edu_sharing_client_api.TRACKING v1Api

All URIs are relative to *http://localhost/edu-sharing/rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**track_event**](TRACKING v1Api.md#track_event) | **put** /tracking/v1/tracking/{repository}/{event} | Track a user interaction

# **track_event**
> track_event(event)

Track a user interaction

Currently limited to video / audio play interactions

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import tracking_v1_api
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
    api_instance = tracking_v1_api.TRACKING v1Api(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'repository': "-home-",
        'event': "DOWNLOAD_MATERIAL",
    }
    query_params = {
    }
    try:
        # Track a user interaction
        api_response = api_instance.track_event(
            path_params=path_params,
            query_params=query_params,
        )
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling TRACKING v1Api->track_event: %s\n" % e)

    # example passing only optional values
    path_params = {
        'repository': "-home-",
        'event': "DOWNLOAD_MATERIAL",
    }
    query_params = {
        'node': "node_example",
    }
    try:
        # Track a user interaction
        api_response = api_instance.track_event(
            path_params=path_params,
            query_params=query_params,
        )
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling TRACKING v1Api->track_event: %s\n" % e)
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
node | NodeSchema | | optional


#### NodeSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
repository | RepositorySchema | | 
event | EventSchema | | 

#### RepositorySchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | defaults to "-home-"

#### EventSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  |  must be one of ["DOWNLOAD_MATERIAL", "VIEW_MATERIAL", "VIEW_MATERIAL_EMBEDDED", "VIEW_MATERIAL_PLAY_MEDIA", "LOGIN_USER_SESSION", "LOGIN_USER_OAUTH_PASSWORD", "LOGIN_USER_OAUTH_REFRESH_TOKEN", "LOGOUT_USER_TIMEOUT", "LOGOUT_USER_REGULAR", ]

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

