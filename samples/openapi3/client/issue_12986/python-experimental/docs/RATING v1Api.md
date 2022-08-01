# edu_sharing_client_api.RATING v1Api

All URIs are relative to *http://localhost/edu-sharing/rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**add_or_update_rating**](RATING v1Api.md#add_or_update_rating) | **put** /rating/v1/ratings/{repository}/{node} | create or update a rating
[**delete_rating**](RATING v1Api.md#delete_rating) | **delete** /rating/v1/ratings/{repository}/{node} | delete a comment

# **add_or_update_rating**
> add_or_update_rating(noderatingbody)

create or update a rating

Adds the rating. If the current user already rated that element, the rating will be altered

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import rating_v1_api
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
    api_instance = rating_v1_api.RATING v1Api(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'repository': "-home-",
        'node': "node_example",
    }
    query_params = {
        'rating': 3.14,
    }
    body = "body_example"
    try:
        # create or update a rating
        api_response = api_instance.add_or_update_rating(
            path_params=path_params,
            query_params=query_params,
            body=body,
        )
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling RATING v1Api->add_or_update_rating: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson] | required |
query_params | RequestQueryParams | |
path_params | RequestPathParams | |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
rating | RatingSchema | | 


#### RatingSchema

Type | Description | Notes
------------- | ------------- | -------------
**int, float** |  | 

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
repository | RepositorySchema | | 
node | NodeSchema | | 

#### RepositorySchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | defaults to "-home-"

#### NodeSchema

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

# **delete_rating**
> delete_rating(node)

delete a comment

Delete the comment with the given id

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import rating_v1_api
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
    api_instance = rating_v1_api.RATING v1Api(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'repository': "-home-",
        'node': "node_example",
    }
    try:
        # delete a comment
        api_response = api_instance.delete_rating(
            path_params=path_params,
        )
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling RATING v1Api->delete_rating: %s\n" % e)
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
repository | RepositorySchema | | 
node | NodeSchema | | 

#### RepositorySchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | defaults to "-home-"

#### NodeSchema

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

