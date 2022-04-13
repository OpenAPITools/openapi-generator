# petstore_api.DLQueriesApi

All URIs are relative to *http://}*

Method | HTTP request | Description
------------- | ------------- | -------------
[**kbs_kb_equivalent_get**](DLQueriesApi.md#kbs_kb_equivalent_get) | **GET** /kbs/{kb}/equivalent | Equivalent classes
[**kbs_kb_instances_get**](DLQueriesApi.md#kbs_kb_instances_get) | **GET** /kbs/{kb}/instances | Instances
[**kbs_kb_satisfiable_get**](DLQueriesApi.md#kbs_kb_satisfiable_get) | **GET** /kbs/{kb}/satisfiable | Satisfiability
[**kbs_kb_subclasses_get**](DLQueriesApi.md#kbs_kb_subclasses_get) | **GET** /kbs/{kb}/subclasses | Subclasses
[**kbs_kb_superclasses_get**](DLQueriesApi.md#kbs_kb_superclasses_get) | **GET** /kbs/{kb}/superclasses | Superclasses
[**kbs_kb_types_get**](DLQueriesApi.md#kbs_kb_types_get) | **GET** /kbs/{kb}/types | Types

# **kbs_kb_equivalent_get**
> {str: (bool, date, datetime, dict, float, int, list, str, none_type)} kbs_kb_equivalent_get(kbobject)

Equivalent classes

Get equivalent classes of a named class or class expression

### Example

```python
import petstore_api
from petstore_api.api import dl_queries_api
from pprint import pprint
# Defining the host is optional and defaults to http://}
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://}"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = dl_queries_api.DLQueriesApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'kb': "uberon",
    }
    query_params = {
        'object': "<http://purl.obolibrary.org/obo/UBERON_0002101>",
    }
    try:
        # Equivalent classes
        api_response = api_instance.kbs_kb_equivalent_get(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling DLQueriesApi->kbs_kb_equivalent_get: %s\n" % e)

    # example passing only optional values
    path_params = {
        'kb': "uberon",
    }
    query_params = {
        'object': "<http://purl.obolibrary.org/obo/UBERON_0002101>",
        'prefixes': "{"obo": "http://purl.obolibrary.org/obo/", "part_of": "http://purl.obolibrary.org/obo/BFO_0000050"}",
        'direct': True,
        'includeDeprecated': True,
    }
    try:
        # Equivalent classes
        api_response = api_instance.kbs_kb_equivalent_get(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling DLQueriesApi->kbs_kb_equivalent_get: %s\n" % e)
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
object | ObjectSchema | | 
prefixes | PrefixesSchema | | optional
direct | DirectSchema | | optional
includeDeprecated | IncludeDeprecatedSchema | | optional


#### ObjectSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### PrefixesSchema

JSON format prefix map, used to expand prefixes in the 'object' expression.  A JSON format prefix map is a JSON object whose keys are prefixes without the colon separator (e.g., \\\"owl\\\"), and whose values are IRI prefixes (typically WITH the hash-mark, \\\"#\\\" suffix).  Because these are passed as parameters, they may need to be URL-encoded to pass over HTTP. 

Type | Description | Notes
------------- | ------------- | -------------
**str** | JSON format prefix map, used to expand prefixes in the &#x27;object&#x27; expression.  A JSON format prefix map is a JSON object whose keys are prefixes without the colon separator (e.g., \\\&quot;owl\\\&quot;), and whose values are IRI prefixes (typically WITH the hash-mark, \\\&quot;#\\\&quot; suffix).  Because these are passed as parameters, they may need to be URL-encoded to pass over HTTP.  | 

#### DirectSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | defaults to True

#### IncludeDeprecatedSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | defaults to True

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
200 | ApiResponseFor200 | equivalent classes

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]


**{str: (bool, date, datetime, dict, float, int, list, str, none_type)}**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **kbs_kb_instances_get**
> {str: (bool, date, datetime, dict, float, int, list, str, none_type)} kbs_kb_instances_get(kbobject)

Instances

Get instances of a named class or class expression

### Example

```python
import petstore_api
from petstore_api.api import dl_queries_api
from pprint import pprint
# Defining the host is optional and defaults to http://}
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://}"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = dl_queries_api.DLQueriesApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'kb': "uberon",
    }
    query_params = {
        'object': "object_example",
    }
    try:
        # Instances
        api_response = api_instance.kbs_kb_instances_get(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling DLQueriesApi->kbs_kb_instances_get: %s\n" % e)

    # example passing only optional values
    path_params = {
        'kb': "uberon",
    }
    query_params = {
        'object': "object_example",
        'prefixes': "{"obo": "http://purl.obolibrary.org/obo/", "part_of": "http://purl.obolibrary.org/obo/BFO_0000050"}",
        'direct': True,
        'includeDeprecated': True,
    }
    try:
        # Instances
        api_response = api_instance.kbs_kb_instances_get(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling DLQueriesApi->kbs_kb_instances_get: %s\n" % e)
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
object | ObjectSchema | | 
prefixes | PrefixesSchema | | optional
direct | DirectSchema | | optional
includeDeprecated | IncludeDeprecatedSchema | | optional


#### ObjectSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### PrefixesSchema

JSON format prefix map, used to expand prefixes in the 'object' expression.  A JSON format prefix map is a JSON object whose keys are prefixes without the colon separator (e.g., \\\"owl\\\"), and whose values are IRI prefixes (typically WITH the hash-mark, \\\"#\\\" suffix).  Because these are passed as parameters, they may need to be URL-encoded to pass over HTTP. 

Type | Description | Notes
------------- | ------------- | -------------
**str** | JSON format prefix map, used to expand prefixes in the &#x27;object&#x27; expression.  A JSON format prefix map is a JSON object whose keys are prefixes without the colon separator (e.g., \\\&quot;owl\\\&quot;), and whose values are IRI prefixes (typically WITH the hash-mark, \\\&quot;#\\\&quot; suffix).  Because these are passed as parameters, they may need to be URL-encoded to pass over HTTP.  | 

#### DirectSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | defaults to True

#### IncludeDeprecatedSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | defaults to True

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
200 | ApiResponseFor200 | instances

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]


**{str: (bool, date, datetime, dict, float, int, list, str, none_type)}**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **kbs_kb_satisfiable_get**
> {str: (bool, date, datetime, dict, float, int, list, str, none_type)} kbs_kb_satisfiable_get(kbobject)

Satisfiability

Returns whether the given named class or expression is satisfiable

### Example

```python
import petstore_api
from petstore_api.api import dl_queries_api
from pprint import pprint
# Defining the host is optional and defaults to http://}
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://}"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = dl_queries_api.DLQueriesApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'kb': "uberon",
    }
    query_params = {
        'object': "<http://purl.obolibrary.org/obo/UBERON_0002101>",
    }
    try:
        # Satisfiability
        api_response = api_instance.kbs_kb_satisfiable_get(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling DLQueriesApi->kbs_kb_satisfiable_get: %s\n" % e)

    # example passing only optional values
    path_params = {
        'kb': "uberon",
    }
    query_params = {
        'object': "<http://purl.obolibrary.org/obo/UBERON_0002101>",
        'prefixes': "{"obo": "http://purl.obolibrary.org/obo/", "part_of": "http://purl.obolibrary.org/obo/BFO_0000050"}",
    }
    try:
        # Satisfiability
        api_response = api_instance.kbs_kb_satisfiable_get(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling DLQueriesApi->kbs_kb_satisfiable_get: %s\n" % e)
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
object | ObjectSchema | | 
prefixes | PrefixesSchema | | optional


#### ObjectSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### PrefixesSchema

JSON format prefix map, used to expand prefixes in the 'object' expression.  A JSON format prefix map is a JSON object whose keys are prefixes without the colon separator (e.g., \\\"owl\\\"), and whose values are IRI prefixes (typically WITH the hash-mark, \\\"#\\\" suffix).  Because these are passed as parameters, they may need to be URL-encoded to pass over HTTP. 

Type | Description | Notes
------------- | ------------- | -------------
**str** | JSON format prefix map, used to expand prefixes in the &#x27;object&#x27; expression.  A JSON format prefix map is a JSON object whose keys are prefixes without the colon separator (e.g., \\\&quot;owl\\\&quot;), and whose values are IRI prefixes (typically WITH the hash-mark, \\\&quot;#\\\&quot; suffix).  Because these are passed as parameters, they may need to be URL-encoded to pass over HTTP.  | 

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
200 | ApiResponseFor200 | satisfiability

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]


**{str: (bool, date, datetime, dict, float, int, list, str, none_type)}**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **kbs_kb_subclasses_get**
> {str: (bool, date, datetime, dict, float, int, list, str, none_type)} kbs_kb_subclasses_get(kbobject)

Subclasses

Get subclasses of a named class or class expression

### Example

```python
import petstore_api
from petstore_api.api import dl_queries_api
from pprint import pprint
# Defining the host is optional and defaults to http://}
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://}"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = dl_queries_api.DLQueriesApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'kb': "uberon",
    }
    query_params = {
        'object': "<http://purl.obolibrary.org/obo/UBERON_0002101>",
    }
    try:
        # Subclasses
        api_response = api_instance.kbs_kb_subclasses_get(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling DLQueriesApi->kbs_kb_subclasses_get: %s\n" % e)

    # example passing only optional values
    path_params = {
        'kb': "uberon",
    }
    query_params = {
        'object': "<http://purl.obolibrary.org/obo/UBERON_0002101>",
        'prefixes': "{"obo": "http://purl.obolibrary.org/obo/", "part_of": "http://purl.obolibrary.org/obo/BFO_0000050"}",
        'direct': True,
        'includeEquivalent': False,
        'includeNothing': False,
        'includeDeprecated': True,
    }
    try:
        # Subclasses
        api_response = api_instance.kbs_kb_subclasses_get(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling DLQueriesApi->kbs_kb_subclasses_get: %s\n" % e)
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
object | ObjectSchema | | 
prefixes | PrefixesSchema | | optional
direct | DirectSchema | | optional
includeEquivalent | IncludeEquivalentSchema | | optional
includeNothing | IncludeNothingSchema | | optional
includeDeprecated | IncludeDeprecatedSchema | | optional


#### ObjectSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### PrefixesSchema

JSON format prefix map, used to expand prefixes in the 'object' expression.  A JSON format prefix map is a JSON object whose keys are prefixes without the colon separator (e.g., \\\"owl\\\"), and whose values are IRI prefixes (typically WITH the hash-mark, \\\"#\\\" suffix).  Because these are passed as parameters, they may need to be URL-encoded to pass over HTTP. 

Type | Description | Notes
------------- | ------------- | -------------
**str** | JSON format prefix map, used to expand prefixes in the &#x27;object&#x27; expression.  A JSON format prefix map is a JSON object whose keys are prefixes without the colon separator (e.g., \\\&quot;owl\\\&quot;), and whose values are IRI prefixes (typically WITH the hash-mark, \\\&quot;#\\\&quot; suffix).  Because these are passed as parameters, they may need to be URL-encoded to pass over HTTP.  | 

#### DirectSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | defaults to True

#### IncludeEquivalentSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | defaults to False

#### IncludeNothingSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | defaults to False

#### IncludeDeprecatedSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | defaults to True

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
200 | ApiResponseFor200 | subclasses

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]


**{str: (bool, date, datetime, dict, float, int, list, str, none_type)}**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **kbs_kb_superclasses_get**
> {str: (bool, date, datetime, dict, float, int, list, str, none_type)} kbs_kb_superclasses_get(kbobject)

Superclasses

Get superclasses of a named class or class expression

### Example

```python
import petstore_api
from petstore_api.api import dl_queries_api
from pprint import pprint
# Defining the host is optional and defaults to http://}
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://}"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = dl_queries_api.DLQueriesApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'kb': "uberon",
    }
    query_params = {
        'object': "<http://purl.obolibrary.org/obo/UBERON_0002101>",
    }
    try:
        # Superclasses
        api_response = api_instance.kbs_kb_superclasses_get(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling DLQueriesApi->kbs_kb_superclasses_get: %s\n" % e)

    # example passing only optional values
    path_params = {
        'kb': "uberon",
    }
    query_params = {
        'object': "<http://purl.obolibrary.org/obo/UBERON_0002101>",
        'prefixes': "{"obo": "http://purl.obolibrary.org/obo/", "part_of": "http://purl.obolibrary.org/obo/BFO_0000050"}",
        'direct': True,
        'includeEquivalent': False,
        'includeThing': False,
        'includeDeprecated': True,
    }
    try:
        # Superclasses
        api_response = api_instance.kbs_kb_superclasses_get(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling DLQueriesApi->kbs_kb_superclasses_get: %s\n" % e)
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
object | ObjectSchema | | 
prefixes | PrefixesSchema | | optional
direct | DirectSchema | | optional
includeEquivalent | IncludeEquivalentSchema | | optional
includeThing | IncludeThingSchema | | optional
includeDeprecated | IncludeDeprecatedSchema | | optional


#### ObjectSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### PrefixesSchema

JSON format prefix map, used to expand prefixes in the 'object' expression.  A JSON format prefix map is a JSON object whose keys are prefixes without the colon separator (e.g., \\\"owl\\\"), and whose values are IRI prefixes (typically WITH the hash-mark, \\\"#\\\" suffix).  Because these are passed as parameters, they may need to be URL-encoded to pass over HTTP. 

Type | Description | Notes
------------- | ------------- | -------------
**str** | JSON format prefix map, used to expand prefixes in the &#x27;object&#x27; expression.  A JSON format prefix map is a JSON object whose keys are prefixes without the colon separator (e.g., \\\&quot;owl\\\&quot;), and whose values are IRI prefixes (typically WITH the hash-mark, \\\&quot;#\\\&quot; suffix).  Because these are passed as parameters, they may need to be URL-encoded to pass over HTTP.  | 

#### DirectSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | defaults to True

#### IncludeEquivalentSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | defaults to False

#### IncludeThingSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | defaults to False

#### IncludeDeprecatedSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | defaults to True

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
200 | ApiResponseFor200 | superclasses

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]


**{str: (bool, date, datetime, dict, float, int, list, str, none_type)}**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **kbs_kb_types_get**
> {str: (bool, date, datetime, dict, float, int, list, str, none_type)} kbs_kb_types_get(kbobject)

Types

Get types of a named individual

### Example

```python
import petstore_api
from petstore_api.api import dl_queries_api
from pprint import pprint
# Defining the host is optional and defaults to http://}
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://}"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = dl_queries_api.DLQueriesApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        'kb': "kb_example",
    }
    query_params = {
        'object': "<http://example.org/person/1234>",
    }
    try:
        # Types
        api_response = api_instance.kbs_kb_types_get(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling DLQueriesApi->kbs_kb_types_get: %s\n" % e)

    # example passing only optional values
    path_params = {
        'kb': "kb_example",
    }
    query_params = {
        'object': "<http://example.org/person/1234>",
        'prefixes': "{"obo": "http://purl.obolibrary.org/obo/", "part_of": "http://purl.obolibrary.org/obo/BFO_0000050"}",
        'direct': True,
        'includeThing': False,
        'includeDeprecated': True,
    }
    try:
        # Types
        api_response = api_instance.kbs_kb_types_get(
            path_params=path_params,
            query_params=query_params,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling DLQueriesApi->kbs_kb_types_get: %s\n" % e)
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
object | ObjectSchema | | 
prefixes | PrefixesSchema | | optional
direct | DirectSchema | | optional
includeThing | IncludeThingSchema | | optional
includeDeprecated | IncludeDeprecatedSchema | | optional


#### ObjectSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### PrefixesSchema

JSON format prefix map, used to expand prefixes in the 'object' expression.  A JSON format prefix map is a JSON object whose keys are prefixes without the colon separator (e.g., \\\"owl\\\"), and whose values are IRI prefixes (typically WITH the hash-mark, \\\"#\\\" suffix).  Because these are passed as parameters, they may need to be URL-encoded to pass over HTTP. 

Type | Description | Notes
------------- | ------------- | -------------
**str** | JSON format prefix map, used to expand prefixes in the &#x27;object&#x27; expression.  A JSON format prefix map is a JSON object whose keys are prefixes without the colon separator (e.g., \\\&quot;owl\\\&quot;), and whose values are IRI prefixes (typically WITH the hash-mark, \\\&quot;#\\\&quot; suffix).  Because these are passed as parameters, they may need to be URL-encoded to pass over HTTP.  | 

#### DirectSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | defaults to True

#### IncludeThingSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | defaults to False

#### IncludeDeprecatedSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | defaults to True

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
200 | ApiResponseFor200 | types

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]


**{str: (bool, date, datetime, dict, float, int, list, str, none_type)}**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

