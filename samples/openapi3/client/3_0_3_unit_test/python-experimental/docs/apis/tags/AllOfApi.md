<a name="__pageTop"></a>
# unit_test_api.apis.tags.all_of_api.AllOfApi

All URIs are relative to *https://someserver.com/v1*

Method | HTTP request | Description
------------- | ------------- | -------------
[**post_allof_combined_with_anyof_oneof_request_body**](#post_allof_combined_with_anyof_oneof_request_body) | **post** /requestBody/postAllofCombinedWithAnyofOneofRequestBody | 
[**post_allof_combined_with_anyof_oneof_response_body_for_content_types**](#post_allof_combined_with_anyof_oneof_response_body_for_content_types) | **post** /responseBody/postAllofCombinedWithAnyofOneofResponseBodyForContentTypes | 
[**post_allof_request_body**](#post_allof_request_body) | **post** /requestBody/postAllofRequestBody | 
[**post_allof_response_body_for_content_types**](#post_allof_response_body_for_content_types) | **post** /responseBody/postAllofResponseBodyForContentTypes | 
[**post_allof_simple_types_request_body**](#post_allof_simple_types_request_body) | **post** /requestBody/postAllofSimpleTypesRequestBody | 
[**post_allof_simple_types_response_body_for_content_types**](#post_allof_simple_types_response_body_for_content_types) | **post** /responseBody/postAllofSimpleTypesResponseBodyForContentTypes | 
[**post_allof_with_base_schema_request_body**](#post_allof_with_base_schema_request_body) | **post** /requestBody/postAllofWithBaseSchemaRequestBody | 
[**post_allof_with_base_schema_response_body_for_content_types**](#post_allof_with_base_schema_response_body_for_content_types) | **post** /responseBody/postAllofWithBaseSchemaResponseBodyForContentTypes | 
[**post_allof_with_one_empty_schema_request_body**](#post_allof_with_one_empty_schema_request_body) | **post** /requestBody/postAllofWithOneEmptySchemaRequestBody | 
[**post_allof_with_one_empty_schema_response_body_for_content_types**](#post_allof_with_one_empty_schema_response_body_for_content_types) | **post** /responseBody/postAllofWithOneEmptySchemaResponseBodyForContentTypes | 
[**post_allof_with_the_first_empty_schema_request_body**](#post_allof_with_the_first_empty_schema_request_body) | **post** /requestBody/postAllofWithTheFirstEmptySchemaRequestBody | 
[**post_allof_with_the_first_empty_schema_response_body_for_content_types**](#post_allof_with_the_first_empty_schema_response_body_for_content_types) | **post** /responseBody/postAllofWithTheFirstEmptySchemaResponseBodyForContentTypes | 
[**post_allof_with_the_last_empty_schema_request_body**](#post_allof_with_the_last_empty_schema_request_body) | **post** /requestBody/postAllofWithTheLastEmptySchemaRequestBody | 
[**post_allof_with_the_last_empty_schema_response_body_for_content_types**](#post_allof_with_the_last_empty_schema_response_body_for_content_types) | **post** /responseBody/postAllofWithTheLastEmptySchemaResponseBodyForContentTypes | 
[**post_allof_with_two_empty_schemas_request_body**](#post_allof_with_two_empty_schemas_request_body) | **post** /requestBody/postAllofWithTwoEmptySchemasRequestBody | 
[**post_allof_with_two_empty_schemas_response_body_for_content_types**](#post_allof_with_two_empty_schemas_response_body_for_content_types) | **post** /responseBody/postAllofWithTwoEmptySchemasResponseBodyForContentTypes | 
[**post_nested_allof_to_check_validation_semantics_request_body**](#post_nested_allof_to_check_validation_semantics_request_body) | **post** /requestBody/postNestedAllofToCheckValidationSemanticsRequestBody | 
[**post_nested_allof_to_check_validation_semantics_response_body_for_content_types**](#post_nested_allof_to_check_validation_semantics_response_body_for_content_types) | **post** /responseBody/postNestedAllofToCheckValidationSemanticsResponseBodyForContentTypes | 

# **post_allof_combined_with_anyof_oneof_request_body**
<a name="post_allof_combined_with_anyof_oneof_request_body"></a>
> post_allof_combined_with_anyof_oneof_request_body(allof_combined_with_anyof_oneof)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.allof_combined_with_anyof_oneof import AllofCombinedWithAnyofOneof
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example passing only required values which don't have defaults set
    body = AllofCombinedWithAnyofOneof(None)
    try:
        api_response = api_instance.post_allof_combined_with_anyof_oneof_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_allof_combined_with_anyof_oneof_request_body: %s\n" % e)
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

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AllofCombinedWithAnyofOneof**](AllofCombinedWithAnyofOneof.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_allof_combined_with_anyof_oneof_response_body_for_content_types**
<a name="post_allof_combined_with_anyof_oneof_response_body_for_content_types"></a>
> AllofCombinedWithAnyofOneof post_allof_combined_with_anyof_oneof_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.allof_combined_with_anyof_oneof import AllofCombinedWithAnyofOneof
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_allof_combined_with_anyof_oneof_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_allof_combined_with_anyof_oneof_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AllofCombinedWithAnyofOneof**](AllofCombinedWithAnyofOneof.md) |  | 



[**AllofCombinedWithAnyofOneof**](AllofCombinedWithAnyofOneof.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_allof_request_body**
<a name="post_allof_request_body"></a>
> post_allof_request_body(allof)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.allof import Allof
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example passing only required values which don't have defaults set
    body = Allof(None)
    try:
        api_response = api_instance.post_allof_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_allof_request_body: %s\n" % e)
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

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**Allof**](Allof.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_allof_response_body_for_content_types**
<a name="post_allof_response_body_for_content_types"></a>
> Allof post_allof_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.allof import Allof
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_allof_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_allof_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**Allof**](Allof.md) |  | 



[**Allof**](Allof.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_allof_simple_types_request_body**
<a name="post_allof_simple_types_request_body"></a>
> post_allof_simple_types_request_body(allof_simple_types)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.allof_simple_types import AllofSimpleTypes
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example passing only required values which don't have defaults set
    body = AllofSimpleTypes(None)
    try:
        api_response = api_instance.post_allof_simple_types_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_allof_simple_types_request_body: %s\n" % e)
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

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AllofSimpleTypes**](AllofSimpleTypes.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_allof_simple_types_response_body_for_content_types**
<a name="post_allof_simple_types_response_body_for_content_types"></a>
> AllofSimpleTypes post_allof_simple_types_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.allof_simple_types import AllofSimpleTypes
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_allof_simple_types_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_allof_simple_types_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AllofSimpleTypes**](AllofSimpleTypes.md) |  | 



[**AllofSimpleTypes**](AllofSimpleTypes.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_allof_with_base_schema_request_body**
<a name="post_allof_with_base_schema_request_body"></a>
> post_allof_with_base_schema_request_body(allof_with_base_schema)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.allof_with_base_schema import AllofWithBaseSchema
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example passing only required values which don't have defaults set
    body = AllofWithBaseSchema({})
    try:
        api_response = api_instance.post_allof_with_base_schema_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_allof_with_base_schema_request_body: %s\n" % e)
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

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AllofWithBaseSchema**](AllofWithBaseSchema.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_allof_with_base_schema_response_body_for_content_types**
<a name="post_allof_with_base_schema_response_body_for_content_types"></a>
> AllofWithBaseSchema post_allof_with_base_schema_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.allof_with_base_schema import AllofWithBaseSchema
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_allof_with_base_schema_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_allof_with_base_schema_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AllofWithBaseSchema**](AllofWithBaseSchema.md) |  | 



[**AllofWithBaseSchema**](AllofWithBaseSchema.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_allof_with_one_empty_schema_request_body**
<a name="post_allof_with_one_empty_schema_request_body"></a>
> post_allof_with_one_empty_schema_request_body(allof_with_one_empty_schema)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.allof_with_one_empty_schema import AllofWithOneEmptySchema
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example passing only required values which don't have defaults set
    body = AllofWithOneEmptySchema(None)
    try:
        api_response = api_instance.post_allof_with_one_empty_schema_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_allof_with_one_empty_schema_request_body: %s\n" % e)
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

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AllofWithOneEmptySchema**](AllofWithOneEmptySchema.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_allof_with_one_empty_schema_response_body_for_content_types**
<a name="post_allof_with_one_empty_schema_response_body_for_content_types"></a>
> AllofWithOneEmptySchema post_allof_with_one_empty_schema_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.allof_with_one_empty_schema import AllofWithOneEmptySchema
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_allof_with_one_empty_schema_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_allof_with_one_empty_schema_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AllofWithOneEmptySchema**](AllofWithOneEmptySchema.md) |  | 



[**AllofWithOneEmptySchema**](AllofWithOneEmptySchema.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_allof_with_the_first_empty_schema_request_body**
<a name="post_allof_with_the_first_empty_schema_request_body"></a>
> post_allof_with_the_first_empty_schema_request_body(allof_with_the_first_empty_schema)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.allof_with_the_first_empty_schema import AllofWithTheFirstEmptySchema
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example passing only required values which don't have defaults set
    body = AllofWithTheFirstEmptySchema(None)
    try:
        api_response = api_instance.post_allof_with_the_first_empty_schema_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_allof_with_the_first_empty_schema_request_body: %s\n" % e)
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

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AllofWithTheFirstEmptySchema**](AllofWithTheFirstEmptySchema.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_allof_with_the_first_empty_schema_response_body_for_content_types**
<a name="post_allof_with_the_first_empty_schema_response_body_for_content_types"></a>
> AllofWithTheFirstEmptySchema post_allof_with_the_first_empty_schema_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.allof_with_the_first_empty_schema import AllofWithTheFirstEmptySchema
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_allof_with_the_first_empty_schema_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_allof_with_the_first_empty_schema_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AllofWithTheFirstEmptySchema**](AllofWithTheFirstEmptySchema.md) |  | 



[**AllofWithTheFirstEmptySchema**](AllofWithTheFirstEmptySchema.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_allof_with_the_last_empty_schema_request_body**
<a name="post_allof_with_the_last_empty_schema_request_body"></a>
> post_allof_with_the_last_empty_schema_request_body(allof_with_the_last_empty_schema)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.allof_with_the_last_empty_schema import AllofWithTheLastEmptySchema
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example passing only required values which don't have defaults set
    body = AllofWithTheLastEmptySchema(None)
    try:
        api_response = api_instance.post_allof_with_the_last_empty_schema_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_allof_with_the_last_empty_schema_request_body: %s\n" % e)
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

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AllofWithTheLastEmptySchema**](AllofWithTheLastEmptySchema.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_allof_with_the_last_empty_schema_response_body_for_content_types**
<a name="post_allof_with_the_last_empty_schema_response_body_for_content_types"></a>
> AllofWithTheLastEmptySchema post_allof_with_the_last_empty_schema_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.allof_with_the_last_empty_schema import AllofWithTheLastEmptySchema
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_allof_with_the_last_empty_schema_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_allof_with_the_last_empty_schema_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AllofWithTheLastEmptySchema**](AllofWithTheLastEmptySchema.md) |  | 



[**AllofWithTheLastEmptySchema**](AllofWithTheLastEmptySchema.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_allof_with_two_empty_schemas_request_body**
<a name="post_allof_with_two_empty_schemas_request_body"></a>
> post_allof_with_two_empty_schemas_request_body(allof_with_two_empty_schemas)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.allof_with_two_empty_schemas import AllofWithTwoEmptySchemas
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example passing only required values which don't have defaults set
    body = AllofWithTwoEmptySchemas(None)
    try:
        api_response = api_instance.post_allof_with_two_empty_schemas_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_allof_with_two_empty_schemas_request_body: %s\n" % e)
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

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AllofWithTwoEmptySchemas**](AllofWithTwoEmptySchemas.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_allof_with_two_empty_schemas_response_body_for_content_types**
<a name="post_allof_with_two_empty_schemas_response_body_for_content_types"></a>
> AllofWithTwoEmptySchemas post_allof_with_two_empty_schemas_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.allof_with_two_empty_schemas import AllofWithTwoEmptySchemas
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_allof_with_two_empty_schemas_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_allof_with_two_empty_schemas_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AllofWithTwoEmptySchemas**](AllofWithTwoEmptySchemas.md) |  | 



[**AllofWithTwoEmptySchemas**](AllofWithTwoEmptySchemas.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_nested_allof_to_check_validation_semantics_request_body**
<a name="post_nested_allof_to_check_validation_semantics_request_body"></a>
> post_nested_allof_to_check_validation_semantics_request_body(nested_allof_to_check_validation_semantics)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.nested_allof_to_check_validation_semantics import NestedAllofToCheckValidationSemantics
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example passing only required values which don't have defaults set
    body = NestedAllofToCheckValidationSemantics(None)
    try:
        api_response = api_instance.post_nested_allof_to_check_validation_semantics_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_nested_allof_to_check_validation_semantics_request_body: %s\n" % e)
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

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**NestedAllofToCheckValidationSemantics**](NestedAllofToCheckValidationSemantics.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_nested_allof_to_check_validation_semantics_response_body_for_content_types**
<a name="post_nested_allof_to_check_validation_semantics_response_body_for_content_types"></a>
> NestedAllofToCheckValidationSemantics post_nested_allof_to_check_validation_semantics_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import all_of_api
from unit_test_api.model.nested_allof_to_check_validation_semantics import NestedAllofToCheckValidationSemantics
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = all_of_api.AllOfApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_nested_allof_to_check_validation_semantics_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling AllOfApi->post_nested_allof_to_check_validation_semantics_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**NestedAllofToCheckValidationSemantics**](NestedAllofToCheckValidationSemantics.md) |  | 



[**NestedAllofToCheckValidationSemantics**](NestedAllofToCheckValidationSemantics.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

