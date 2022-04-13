# petstore_api.SPARQLApi

All URIs are relative to *http://}*

Method | HTTP request | Description
------------- | ------------- | -------------
[**kbs_kb_expand_get**](SPARQLApi.md#kbs_kb_expand_get) | **GET** /kbs/{kb}/expand | Expand SPARQL query encoded in URL parameter
[**kbs_kb_expand_post**](SPARQLApi.md#kbs_kb_expand_post) | **POST** /kbs/{kb}/expand | Expand SPARQL query contained in request body
[**kbs_kb_sparql_get**](SPARQLApi.md#kbs_kb_sparql_get) | **GET** /kbs/{kb}/sparql | Perform SPARQL query encoded in URL parameter
[**kbs_kb_sparql_post**](SPARQLApi.md#kbs_kb_sparql_post) | **POST** /kbs/{kb}/sparql | Perform SPARQL query contained in request body

# **kbs_kb_expand_get**
> kbs_kb_expand_get(kbquery)

Expand SPARQL query encoded in URL parameter

Expand a SPARQL query, transforming Owlet-style embedded class expressions into `FILTER`s

### Example

```python
import petstore_api
from petstore_api.api import sparql_api
from pprint import pprint
# Defining the host is optional and defaults to http://}
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://}"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = sparql_api.SPARQLApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'kb': "kb_example",
    }
    query_params = {
        'query': "query_example",
    }
    try:
        # Expand SPARQL query encoded in URL parameter
        api_response = api_instance.kbs_kb_expand_get(
            path_params=path_params,
            query_params=query_params,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling SPARQLApi->kbs_kb_expand_get: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query_params | RequestQueryParams | |
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('application/sparql-query', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query | QuerySchema | | 


#### QuerySchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

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
200 | ApiResponseFor200 | Expanded SPARQL query

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[, ] |  |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **kbs_kb_expand_post**
> kbs_kb_expand_post(kbbody)

Expand SPARQL query contained in request body

Expand a SPARQL query, transforming Owlet-style embedded class expressions into `FILTER`s

### Example

```python
import petstore_api
from petstore_api.api import sparql_api
from pprint import pprint
# Defining the host is optional and defaults to http://}
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://}"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = sparql_api.SPARQLApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'kb': "kb_example",
    }
    body = "SELECT ?x WHERE { ?x a "blah"}"
    try:
        # Expand SPARQL query contained in request body
        api_response = api_instance.kbs_kb_expand_post(
            path_params=path_params,
            body=body,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling SPARQLApi->kbs_kb_expand_post: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationSparqlQuery, SchemaForRequestBodyApplicationXWwwFormUrlencoded] | required |
path_params | RequestPathParams | |
content_type | str | optional, default is 'application/sparql-query' | Selects the schema and serialization of the request body
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationSparqlQuery

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### SchemaForRequestBodyApplicationXWwwFormUrlencoded

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**query** | **str** |  | 
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

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
200 | ApiResponseFor200 | SPARQL results

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **kbs_kb_sparql_get**
> kbs_kb_sparql_get(kbquery)

Perform SPARQL query encoded in URL parameter

Perform SPARQL query using Owlet-style embedded class expression. This is not a complete SPARQL endpoint. It is for using Owlery as a federated query endpoint for a single Owlet triple pattern.

### Example

```python
import petstore_api
from petstore_api.api import sparql_api
from pprint import pprint
# Defining the host is optional and defaults to http://}
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://}"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = sparql_api.SPARQLApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'kb': "kb_example",
    }
    query_params = {
        'query': "query_example",
    }
    try:
        # Perform SPARQL query encoded in URL parameter
        api_response = api_instance.kbs_kb_sparql_get(
            path_params=path_params,
            query_params=query_params,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling SPARQLApi->kbs_kb_sparql_get: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query_params | RequestQueryParams | |
path_params | RequestPathParams | |
accept_content_types | typing.Tuple[str] | default is ('application/sparql-results+xml', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query | QuerySchema | | 


#### QuerySchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

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
200 | ApiResponseFor200 | SPARQL results

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[, ] |  |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **kbs_kb_sparql_post**
> kbs_kb_sparql_post(kbbody)

Perform SPARQL query contained in request body

Perform SPARQL query using Owlet-style embedded class expression. This is not a complete SPARQL endpoint. It is for using Owlery as a federated query endpoint for a single Owlet triple pattern.

### Example

```python
import petstore_api
from petstore_api.api import sparql_api
from pprint import pprint
# Defining the host is optional and defaults to http://}
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://}"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = sparql_api.SPARQLApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'kb': "kb_example",
    }
    body = "SELECT ?x WHERE { ?x a "blah"}"
    try:
        # Perform SPARQL query contained in request body
        api_response = api_instance.kbs_kb_sparql_post(
            path_params=path_params,
            body=body,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling SPARQLApi->kbs_kb_sparql_post: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationSparqlQuery, SchemaForRequestBodyApplicationXWwwFormUrlencoded] | required |
path_params | RequestPathParams | |
content_type | str | optional, default is 'application/sparql-query' | Selects the schema and serialization of the request body
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationSparqlQuery

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### SchemaForRequestBodyApplicationXWwwFormUrlencoded

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**query** | **str** |  | 
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

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
200 | ApiResponseFor200 | SPARQL results

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

