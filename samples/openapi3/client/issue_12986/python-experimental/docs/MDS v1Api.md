# edu_sharing_client_api.MDS v1Api

All URIs are relative to *http://localhost/edu-sharing/rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**get_metadata_set**](MDS v1Api.md#get_metadata_set) | **get** /mds/v1/metadatasets/{repository}/{metadataset} | Get metadata set new.
[**get_metadata_sets**](MDS v1Api.md#get_metadata_sets) | **get** /mds/v1/metadatasets/{repository} | Get metadata sets V2 of repository.
[**get_values**](MDS v1Api.md#get_values) | **post** /mds/v1/metadatasets/{repository}/{metadataset}/values | Get values.
[**get_values4_keys**](MDS v1Api.md#get_values4_keys) | **post** /mds/v1/metadatasets/{repository}/{metadataset}/values_for_keys | Get values for keys.
[**suggest_value**](MDS v1Api.md#suggest_value) | **post** /mds/v1/metadatasets/{repository}/{metadataset}/values/{widget}/suggest | Suggest a value.

# **get_metadata_set**
> Mds get_metadata_set()

Get metadata set new.

Get metadata set new.

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import mds_v1_api
from edu_sharing_client_api.model.error_response import ErrorResponse
from edu_sharing_client_api.model.mds import Mds
from pprint import pprint
# Defining the host is optional and defaults to http://localhost/edu-sharing/rest
# See configuration.py for a list of all supported configuration parameters.
configuration = edu_sharing_client_api.Configuration(
    host = "http://localhost/edu-sharing/rest"
)

# Enter a context with an instance of the API client
with edu_sharing_client_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = mds_v1_api.MDS v1Api(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'repository': "-home-",
        'metadataset': "-default-",
    }
    try:
        # Get metadata set new.
        api_response = api_instance.get_metadata_set(
            path_params=path_params,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling MDS v1Api->get_metadata_set: %s\n" % e)
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
metadataset | MetadatasetSchema | | 

#### RepositorySchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | defaults to "-home-"

#### MetadatasetSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | defaults to "-default-"

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
[**Mds**](Mds.md) |  | 


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



[**Mds**](Mds.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_metadata_sets**
> MdsEntries get_metadata_sets()

Get metadata sets V2 of repository.

Get metadata sets V2 of repository.

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import mds_v1_api
from edu_sharing_client_api.model.mds_entries import MdsEntries
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
    api_instance = mds_v1_api.MDS v1Api(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'repository': "-home-",
    }
    try:
        # Get metadata sets V2 of repository.
        api_response = api_instance.get_metadata_sets(
            path_params=path_params,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling MDS v1Api->get_metadata_sets: %s\n" % e)
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

#### RepositorySchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | defaults to "-home-"

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
[**MdsEntries**](MdsEntries.md) |  | 


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



[**MdsEntries**](MdsEntries.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_values**
> Mds get_values()

Get values.

Get values.

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import mds_v1_api
from edu_sharing_client_api.model.error_response import ErrorResponse
from edu_sharing_client_api.model.suggestion_param import SuggestionParam
from edu_sharing_client_api.model.mds import Mds
from pprint import pprint
# Defining the host is optional and defaults to http://localhost/edu-sharing/rest
# See configuration.py for a list of all supported configuration parameters.
configuration = edu_sharing_client_api.Configuration(
    host = "http://localhost/edu-sharing/rest"
)

# Enter a context with an instance of the API client
with edu_sharing_client_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = mds_v1_api.MDS v1Api(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'repository': "-home-",
        'metadataset': "-default-",
    }
    try:
        # Get values.
        api_response = api_instance.get_values(
            path_params=path_params,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling MDS v1Api->get_values: %s\n" % e)

    # example passing only optional values
    path_params = {
        'repository': "-home-",
        'metadataset': "-default-",
    }
    body = SuggestionParam(
        value_parameters=ValueParameters(
            query="query_example",
            _property="_property_example",
            pattern="pattern_example",
        ),
        criteria=[
            MdsQueryCriteria(
                _property="_property_example",
                values=[
                    "values_example"
                ],
            )
        ],
    )
    try:
        # Get values.
        api_response = api_instance.get_values(
            path_params=path_params,
            body=body,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling MDS v1Api->get_values: %s\n" % e)
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
[**SuggestionParam**](SuggestionParam.md) |  | 


### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
repository | RepositorySchema | | 
metadataset | MetadatasetSchema | | 

#### RepositorySchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | defaults to "-home-"

#### MetadatasetSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | defaults to "-default-"

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
[**Mds**](Mds.md) |  | 


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



[**Mds**](Mds.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_values4_keys**
> Suggestions get_values4_keys()

Get values for keys.

Get values for keys.

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import mds_v1_api
from edu_sharing_client_api.model.suggestions import Suggestions
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
    api_instance = mds_v1_api.MDS v1Api(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'repository': "-home-",
        'metadataset': "-default-",
    }
    query_params = {
    }
    try:
        # Get values for keys.
        api_response = api_instance.get_values4_keys(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling MDS v1Api->get_values4_keys: %s\n" % e)

    # example passing only optional values
    path_params = {
        'repository': "-home-",
        'metadataset': "-default-",
    }
    query_params = {
        'query': "query_example",
        'property': "property_example",
    }
    body = [
        "request_body_example"
    ]
    try:
        # Get values for keys.
        api_response = api_instance.get_values4_keys(
            path_params=path_params,
            query_params=query_params,
            body=body,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling MDS v1Api->get_values4_keys: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
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
**[str]** |  | 

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query | QuerySchema | | optional
property | ModelPropertySchema | | optional


#### QuerySchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ModelPropertySchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
repository | RepositorySchema | | 
metadataset | MetadatasetSchema | | 

#### RepositorySchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | defaults to "-home-"

#### MetadatasetSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | defaults to "-default-"

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
[**Suggestions**](Suggestions.md) |  | 


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



[**Suggestions**](Suggestions.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **suggest_value**
> MdsValue suggest_value(widgetcaption)

Suggest a value.

Suggest a new value for a given metadataset and widget. The suggestion will be forwarded to the corresponding person in the metadataset file

### Example

```python
import edu_sharing_client_api
from edu_sharing_client_api.apis.tags import mds_v1_api
from edu_sharing_client_api.model.mds_value import MdsValue
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
    api_instance = mds_v1_api.MDS v1Api(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'repository': "-home-",
        'metadataset': "-default-",
        'widget': "widget_example",
    }
    query_params = {
        'caption': "caption_example",
    }
    try:
        # Suggest a value.
        api_response = api_instance.suggest_value(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling MDS v1Api->suggest_value: %s\n" % e)

    # example passing only optional values
    path_params = {
        'repository': "-home-",
        'metadataset': "-default-",
        'widget': "widget_example",
    }
    query_params = {
        'caption': "caption_example",
        'parent': "parent_example",
        'nodeId': [
        "nodeId_example"
    ],
    }
    try:
        # Suggest a value.
        api_response = api_instance.suggest_value(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except edu_sharing_client_api.ApiException as e:
        print("Exception when calling MDS v1Api->suggest_value: %s\n" % e)
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
caption | CaptionSchema | | 
parent | ParentSchema | | optional
nodeId | NodeIdSchema | | optional


#### CaptionSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ParentSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### NodeIdSchema

Type | Description | Notes
------------- | ------------- | -------------
**[str]** |  | 

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
repository | RepositorySchema | | 
metadataset | MetadatasetSchema | | 
widget | WidgetSchema | | 

#### RepositorySchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | defaults to "-home-"

#### MetadatasetSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | defaults to "-default-"

#### WidgetSchema

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
[**MdsValue**](MdsValue.md) |  | 


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



[**MdsValue**](MdsValue.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

