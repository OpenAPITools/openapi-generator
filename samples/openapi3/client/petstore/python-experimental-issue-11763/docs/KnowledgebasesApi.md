# petstore_api.KnowledgebasesApi

All URIs are relative to *http://}*

Method | HTTP request | Description
------------- | ------------- | -------------
[**kbs_get**](KnowledgebasesApi.md#kbs_get) | **GET** /kbs | List available knowledgebases
[**kbs_kb_get**](KnowledgebasesApi.md#kbs_kb_get) | **GET** /kbs/{kb} | Knowledgebase

# **kbs_get**
> [str] kbs_get()

List available knowledgebases

List names of available knowledgebases in this instance of Owlery

### Example

```python
import petstore_api
from petstore_api.api import knowledgebases_api
from pprint import pprint
# Defining the host is optional and defaults to http://}
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://}"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = knowledgebases_api.KnowledgebasesApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        # List available knowledgebases
        api_response = api_instance.kbs_get()
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling KnowledgebasesApi->kbs_get: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | knowledgebase names

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**[str]** |  | 


**[str]**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **kbs_kb_get**
> InlineResponse200 kbs_kb_get(kb)

Knowledgebase

Display Knowledgebase information and status

### Example

```python
import petstore_api
from petstore_api.api import knowledgebases_api
from petstore_api.model.inline_response200 import InlineResponse200
from pprint import pprint
# Defining the host is optional and defaults to http://}
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://}"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = knowledgebases_api.KnowledgebasesApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'kb': "uberon",
    }
    try:
        # Knowledgebase
        api_response = api_instance.kbs_kb_get(
            path_params=path_params,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling KnowledgebasesApi->kbs_kb_get: %s\n" % e)
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
kb | KbSchema | | 

#### KbSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | KB info

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**InlineResponse200**](InlineResponse200.md) |  | 



[**InlineResponse200**](InlineResponse200.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

