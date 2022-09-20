<a name="__pageTop"></a>
# unit_test_api.apis.tags.type_api.TypeApi

All URIs are relative to *https://someserver.com/v1*

Method | HTTP request | Description
------------- | ------------- | -------------
[**post_array_type_matches_arrays_request_body**](#post_array_type_matches_arrays_request_body) | **post** /requestBody/postArrayTypeMatchesArraysRequestBody | 
[**post_array_type_matches_arrays_response_body_for_content_types**](#post_array_type_matches_arrays_response_body_for_content_types) | **post** /responseBody/postArrayTypeMatchesArraysResponseBodyForContentTypes | 
[**post_boolean_type_matches_booleans_request_body**](#post_boolean_type_matches_booleans_request_body) | **post** /requestBody/postBooleanTypeMatchesBooleansRequestBody | 
[**post_boolean_type_matches_booleans_response_body_for_content_types**](#post_boolean_type_matches_booleans_response_body_for_content_types) | **post** /responseBody/postBooleanTypeMatchesBooleansResponseBodyForContentTypes | 
[**post_integer_type_matches_integers_request_body**](#post_integer_type_matches_integers_request_body) | **post** /requestBody/postIntegerTypeMatchesIntegersRequestBody | 
[**post_integer_type_matches_integers_response_body_for_content_types**](#post_integer_type_matches_integers_response_body_for_content_types) | **post** /responseBody/postIntegerTypeMatchesIntegersResponseBodyForContentTypes | 
[**post_null_type_matches_only_the_null_object_request_body**](#post_null_type_matches_only_the_null_object_request_body) | **post** /requestBody/postNullTypeMatchesOnlyTheNullObjectRequestBody | 
[**post_null_type_matches_only_the_null_object_response_body_for_content_types**](#post_null_type_matches_only_the_null_object_response_body_for_content_types) | **post** /responseBody/postNullTypeMatchesOnlyTheNullObjectResponseBodyForContentTypes | 
[**post_number_type_matches_numbers_request_body**](#post_number_type_matches_numbers_request_body) | **post** /requestBody/postNumberTypeMatchesNumbersRequestBody | 
[**post_number_type_matches_numbers_response_body_for_content_types**](#post_number_type_matches_numbers_response_body_for_content_types) | **post** /responseBody/postNumberTypeMatchesNumbersResponseBodyForContentTypes | 
[**post_object_type_matches_objects_request_body**](#post_object_type_matches_objects_request_body) | **post** /requestBody/postObjectTypeMatchesObjectsRequestBody | 
[**post_object_type_matches_objects_response_body_for_content_types**](#post_object_type_matches_objects_response_body_for_content_types) | **post** /responseBody/postObjectTypeMatchesObjectsResponseBodyForContentTypes | 
[**post_string_type_matches_strings_request_body**](#post_string_type_matches_strings_request_body) | **post** /requestBody/postStringTypeMatchesStringsRequestBody | 
[**post_string_type_matches_strings_response_body_for_content_types**](#post_string_type_matches_strings_response_body_for_content_types) | **post** /responseBody/postStringTypeMatchesStringsResponseBodyForContentTypes | 

# **post_array_type_matches_arrays_request_body**
<a name="post_array_type_matches_arrays_request_body"></a>
> post_array_type_matches_arrays_request_body(array_type_matches_arrays)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import type_api
from unit_test_api.model.array_type_matches_arrays import ArrayTypeMatchesArrays
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = type_api.TypeApi(api_client)

    # example passing only required values which don't have defaults set
    body = ArrayTypeMatchesArrays([
        None
    ])
    try:
        api_response = api_instance.post_array_type_matches_arrays_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling TypeApi->post_array_type_matches_arrays_request_body: %s\n" % e)
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
[**ArrayTypeMatchesArrays**](../../models/ArrayTypeMatchesArrays.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_array_type_matches_arrays_request_body.ApiResponseFor200) | success

#### post_array_type_matches_arrays_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_array_type_matches_arrays_response_body_for_content_types**
<a name="post_array_type_matches_arrays_response_body_for_content_types"></a>
> ArrayTypeMatchesArrays post_array_type_matches_arrays_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import type_api
from unit_test_api.model.array_type_matches_arrays import ArrayTypeMatchesArrays
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = type_api.TypeApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_array_type_matches_arrays_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling TypeApi->post_array_type_matches_arrays_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_array_type_matches_arrays_response_body_for_content_types.ApiResponseFor200) | success

#### post_array_type_matches_arrays_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ArrayTypeMatchesArrays**](../../models/ArrayTypeMatchesArrays.md) |  | 


### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_boolean_type_matches_booleans_request_body**
<a name="post_boolean_type_matches_booleans_request_body"></a>
> post_boolean_type_matches_booleans_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import type_api
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = type_api.TypeApi(api_client)

    # example passing only required values which don't have defaults set
    body = True
    try:
        api_response = api_instance.post_boolean_type_matches_booleans_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling TypeApi->post_boolean_type_matches_booleans_request_body: %s\n" % e)
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
bool,  | BoolClass,  |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_boolean_type_matches_booleans_request_body.ApiResponseFor200) | success

#### post_boolean_type_matches_booleans_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_boolean_type_matches_booleans_response_body_for_content_types**
<a name="post_boolean_type_matches_booleans_response_body_for_content_types"></a>
> bool post_boolean_type_matches_booleans_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import type_api
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = type_api.TypeApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_boolean_type_matches_booleans_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling TypeApi->post_boolean_type_matches_booleans_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_boolean_type_matches_booleans_response_body_for_content_types.ApiResponseFor200) | success

#### post_boolean_type_matches_booleans_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
bool,  | BoolClass,  |  | 

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_integer_type_matches_integers_request_body**
<a name="post_integer_type_matches_integers_request_body"></a>
> post_integer_type_matches_integers_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import type_api
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = type_api.TypeApi(api_client)

    # example passing only required values which don't have defaults set
    body = 1
    try:
        api_response = api_instance.post_integer_type_matches_integers_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling TypeApi->post_integer_type_matches_integers_request_body: %s\n" % e)
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
decimal.Decimal, int,  | decimal.Decimal,  |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_integer_type_matches_integers_request_body.ApiResponseFor200) | success

#### post_integer_type_matches_integers_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_integer_type_matches_integers_response_body_for_content_types**
<a name="post_integer_type_matches_integers_response_body_for_content_types"></a>
> int post_integer_type_matches_integers_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import type_api
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = type_api.TypeApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_integer_type_matches_integers_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling TypeApi->post_integer_type_matches_integers_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_integer_type_matches_integers_response_body_for_content_types.ApiResponseFor200) | success

#### post_integer_type_matches_integers_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
decimal.Decimal, int,  | decimal.Decimal,  |  | 

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_null_type_matches_only_the_null_object_request_body**
<a name="post_null_type_matches_only_the_null_object_request_body"></a>
> post_null_type_matches_only_the_null_object_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import type_api
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = type_api.TypeApi(api_client)

    # example passing only required values which don't have defaults set
    body = None
    try:
        api_response = api_instance.post_null_type_matches_only_the_null_object_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling TypeApi->post_null_type_matches_only_the_null_object_request_body: %s\n" % e)
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
None,  | NoneClass,  |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_null_type_matches_only_the_null_object_request_body.ApiResponseFor200) | success

#### post_null_type_matches_only_the_null_object_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_null_type_matches_only_the_null_object_response_body_for_content_types**
<a name="post_null_type_matches_only_the_null_object_response_body_for_content_types"></a>
> none_type post_null_type_matches_only_the_null_object_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import type_api
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = type_api.TypeApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_null_type_matches_only_the_null_object_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling TypeApi->post_null_type_matches_only_the_null_object_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_null_type_matches_only_the_null_object_response_body_for_content_types.ApiResponseFor200) | success

#### post_null_type_matches_only_the_null_object_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
None,  | NoneClass,  |  | 

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_number_type_matches_numbers_request_body**
<a name="post_number_type_matches_numbers_request_body"></a>
> post_number_type_matches_numbers_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import type_api
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = type_api.TypeApi(api_client)

    # example passing only required values which don't have defaults set
    body = 3.14
    try:
        api_response = api_instance.post_number_type_matches_numbers_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling TypeApi->post_number_type_matches_numbers_request_body: %s\n" % e)
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
decimal.Decimal, int, float,  | decimal.Decimal,  |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_number_type_matches_numbers_request_body.ApiResponseFor200) | success

#### post_number_type_matches_numbers_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_number_type_matches_numbers_response_body_for_content_types**
<a name="post_number_type_matches_numbers_response_body_for_content_types"></a>
> int, float post_number_type_matches_numbers_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import type_api
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = type_api.TypeApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_number_type_matches_numbers_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling TypeApi->post_number_type_matches_numbers_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_number_type_matches_numbers_response_body_for_content_types.ApiResponseFor200) | success

#### post_number_type_matches_numbers_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
decimal.Decimal, int, float,  | decimal.Decimal,  |  | 

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_object_type_matches_objects_request_body**
<a name="post_object_type_matches_objects_request_body"></a>
> post_object_type_matches_objects_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import type_api
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = type_api.TypeApi(api_client)

    # example passing only required values which don't have defaults set
    body = dict()
    try:
        api_response = api_instance.post_object_type_matches_objects_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling TypeApi->post_object_type_matches_objects_request_body: %s\n" % e)
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
dict, frozendict.frozendict,  | frozendict.frozendict,  |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_object_type_matches_objects_request_body.ApiResponseFor200) | success

#### post_object_type_matches_objects_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_object_type_matches_objects_response_body_for_content_types**
<a name="post_object_type_matches_objects_response_body_for_content_types"></a>
> {str: (bool, date, datetime, dict, float, int, list, str, none_type)} post_object_type_matches_objects_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import type_api
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = type_api.TypeApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_object_type_matches_objects_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling TypeApi->post_object_type_matches_objects_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_object_type_matches_objects_response_body_for_content_types.ApiResponseFor200) | success

#### post_object_type_matches_objects_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
dict, frozendict.frozendict,  | frozendict.frozendict,  |  | 

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_string_type_matches_strings_request_body**
<a name="post_string_type_matches_strings_request_body"></a>
> post_string_type_matches_strings_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import type_api
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = type_api.TypeApi(api_client)

    # example passing only required values which don't have defaults set
    body = "body_example"
    try:
        api_response = api_instance.post_string_type_matches_strings_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling TypeApi->post_string_type_matches_strings_request_body: %s\n" % e)
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
str,  | str,  |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_string_type_matches_strings_request_body.ApiResponseFor200) | success

#### post_string_type_matches_strings_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_string_type_matches_strings_response_body_for_content_types**
<a name="post_string_type_matches_strings_response_body_for_content_types"></a>
> str post_string_type_matches_strings_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import type_api
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = type_api.TypeApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_string_type_matches_strings_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling TypeApi->post_string_type_matches_strings_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_string_type_matches_strings_response_body_for_content_types.ApiResponseFor200) | success

#### post_string_type_matches_strings_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
str,  | str,  |  | 

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

