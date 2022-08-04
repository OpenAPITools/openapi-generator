<a name="__pageTop"></a>
# unit_test_api.apis.tags.enum_api.EnumApi

All URIs are relative to *https://someserver.com/v1*

Method | HTTP request | Description
------------- | ------------- | -------------
[**post_enum_with0_does_not_match_false_request_body**](#post_enum_with0_does_not_match_false_request_body) | **post** /requestBody/postEnumWith0DoesNotMatchFalseRequestBody | 
[**post_enum_with0_does_not_match_false_response_body_for_content_types**](#post_enum_with0_does_not_match_false_response_body_for_content_types) | **post** /responseBody/postEnumWith0DoesNotMatchFalseResponseBodyForContentTypes | 
[**post_enum_with1_does_not_match_true_request_body**](#post_enum_with1_does_not_match_true_request_body) | **post** /requestBody/postEnumWith1DoesNotMatchTrueRequestBody | 
[**post_enum_with1_does_not_match_true_response_body_for_content_types**](#post_enum_with1_does_not_match_true_response_body_for_content_types) | **post** /responseBody/postEnumWith1DoesNotMatchTrueResponseBodyForContentTypes | 
[**post_enum_with_escaped_characters_request_body**](#post_enum_with_escaped_characters_request_body) | **post** /requestBody/postEnumWithEscapedCharactersRequestBody | 
[**post_enum_with_escaped_characters_response_body_for_content_types**](#post_enum_with_escaped_characters_response_body_for_content_types) | **post** /responseBody/postEnumWithEscapedCharactersResponseBodyForContentTypes | 
[**post_enum_with_false_does_not_match0_request_body**](#post_enum_with_false_does_not_match0_request_body) | **post** /requestBody/postEnumWithFalseDoesNotMatch0RequestBody | 
[**post_enum_with_false_does_not_match0_response_body_for_content_types**](#post_enum_with_false_does_not_match0_response_body_for_content_types) | **post** /responseBody/postEnumWithFalseDoesNotMatch0ResponseBodyForContentTypes | 
[**post_enum_with_true_does_not_match1_request_body**](#post_enum_with_true_does_not_match1_request_body) | **post** /requestBody/postEnumWithTrueDoesNotMatch1RequestBody | 
[**post_enum_with_true_does_not_match1_response_body_for_content_types**](#post_enum_with_true_does_not_match1_response_body_for_content_types) | **post** /responseBody/postEnumWithTrueDoesNotMatch1ResponseBodyForContentTypes | 
[**post_enums_in_properties_request_body**](#post_enums_in_properties_request_body) | **post** /requestBody/postEnumsInPropertiesRequestBody | 
[**post_enums_in_properties_response_body_for_content_types**](#post_enums_in_properties_response_body_for_content_types) | **post** /responseBody/postEnumsInPropertiesResponseBodyForContentTypes | 
[**post_nul_characters_in_strings_request_body**](#post_nul_characters_in_strings_request_body) | **post** /requestBody/postNulCharactersInStringsRequestBody | 
[**post_nul_characters_in_strings_response_body_for_content_types**](#post_nul_characters_in_strings_response_body_for_content_types) | **post** /responseBody/postNulCharactersInStringsResponseBodyForContentTypes | 
[**post_simple_enum_validation_request_body**](#post_simple_enum_validation_request_body) | **post** /requestBody/postSimpleEnumValidationRequestBody | 
[**post_simple_enum_validation_response_body_for_content_types**](#post_simple_enum_validation_response_body_for_content_types) | **post** /responseBody/postSimpleEnumValidationResponseBodyForContentTypes | 

# **post_enum_with0_does_not_match_false_request_body**
<a name="post_enum_with0_does_not_match_false_request_body"></a>
> post_enum_with0_does_not_match_false_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import enum_api
from unit_test_api.model.enum_with0_does_not_match_false import EnumWith0DoesNotMatchFalse
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = enum_api.EnumApi(api_client)

    # example passing only required values which don't have defaults set
    body = EnumWith0DoesNotMatchFalse(0)
    try:
        api_response = api_instance.post_enum_with0_does_not_match_false_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling EnumApi->post_enum_with0_does_not_match_false_request_body: %s\n" % e)
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
[**EnumWith0DoesNotMatchFalse**](EnumWith0DoesNotMatchFalse.md) |  | 


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

# **post_enum_with0_does_not_match_false_response_body_for_content_types**
<a name="post_enum_with0_does_not_match_false_response_body_for_content_types"></a>
> EnumWith0DoesNotMatchFalse post_enum_with0_does_not_match_false_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import enum_api
from unit_test_api.model.enum_with0_does_not_match_false import EnumWith0DoesNotMatchFalse
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = enum_api.EnumApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_enum_with0_does_not_match_false_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling EnumApi->post_enum_with0_does_not_match_false_response_body_for_content_types: %s\n" % e)
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
[**EnumWith0DoesNotMatchFalse**](EnumWith0DoesNotMatchFalse.md) |  | 



[**EnumWith0DoesNotMatchFalse**](EnumWith0DoesNotMatchFalse.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_enum_with1_does_not_match_true_request_body**
<a name="post_enum_with1_does_not_match_true_request_body"></a>
> post_enum_with1_does_not_match_true_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import enum_api
from unit_test_api.model.enum_with1_does_not_match_true import EnumWith1DoesNotMatchTrue
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = enum_api.EnumApi(api_client)

    # example passing only required values which don't have defaults set
    body = EnumWith1DoesNotMatchTrue(1)
    try:
        api_response = api_instance.post_enum_with1_does_not_match_true_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling EnumApi->post_enum_with1_does_not_match_true_request_body: %s\n" % e)
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
[**EnumWith1DoesNotMatchTrue**](EnumWith1DoesNotMatchTrue.md) |  | 


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

# **post_enum_with1_does_not_match_true_response_body_for_content_types**
<a name="post_enum_with1_does_not_match_true_response_body_for_content_types"></a>
> EnumWith1DoesNotMatchTrue post_enum_with1_does_not_match_true_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import enum_api
from unit_test_api.model.enum_with1_does_not_match_true import EnumWith1DoesNotMatchTrue
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = enum_api.EnumApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_enum_with1_does_not_match_true_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling EnumApi->post_enum_with1_does_not_match_true_response_body_for_content_types: %s\n" % e)
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
[**EnumWith1DoesNotMatchTrue**](EnumWith1DoesNotMatchTrue.md) |  | 



[**EnumWith1DoesNotMatchTrue**](EnumWith1DoesNotMatchTrue.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_enum_with_escaped_characters_request_body**
<a name="post_enum_with_escaped_characters_request_body"></a>
> post_enum_with_escaped_characters_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import enum_api
from unit_test_api.model.enum_with_escaped_characters import EnumWithEscapedCharacters
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = enum_api.EnumApi(api_client)

    # example passing only required values which don't have defaults set
    body = EnumWithEscapedCharacters("foo\nbar")
    try:
        api_response = api_instance.post_enum_with_escaped_characters_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling EnumApi->post_enum_with_escaped_characters_request_body: %s\n" % e)
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
[**EnumWithEscapedCharacters**](EnumWithEscapedCharacters.md) |  | 


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

# **post_enum_with_escaped_characters_response_body_for_content_types**
<a name="post_enum_with_escaped_characters_response_body_for_content_types"></a>
> EnumWithEscapedCharacters post_enum_with_escaped_characters_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import enum_api
from unit_test_api.model.enum_with_escaped_characters import EnumWithEscapedCharacters
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = enum_api.EnumApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_enum_with_escaped_characters_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling EnumApi->post_enum_with_escaped_characters_response_body_for_content_types: %s\n" % e)
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
[**EnumWithEscapedCharacters**](EnumWithEscapedCharacters.md) |  | 



[**EnumWithEscapedCharacters**](EnumWithEscapedCharacters.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_enum_with_false_does_not_match0_request_body**
<a name="post_enum_with_false_does_not_match0_request_body"></a>
> post_enum_with_false_does_not_match0_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import enum_api
from unit_test_api.model.enum_with_false_does_not_match0 import EnumWithFalseDoesNotMatch0
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = enum_api.EnumApi(api_client)

    # example passing only required values which don't have defaults set
    body = EnumWithFalseDoesNotMatch0(False)
    try:
        api_response = api_instance.post_enum_with_false_does_not_match0_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling EnumApi->post_enum_with_false_does_not_match0_request_body: %s\n" % e)
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
[**EnumWithFalseDoesNotMatch0**](EnumWithFalseDoesNotMatch0.md) |  | 


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

# **post_enum_with_false_does_not_match0_response_body_for_content_types**
<a name="post_enum_with_false_does_not_match0_response_body_for_content_types"></a>
> EnumWithFalseDoesNotMatch0 post_enum_with_false_does_not_match0_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import enum_api
from unit_test_api.model.enum_with_false_does_not_match0 import EnumWithFalseDoesNotMatch0
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = enum_api.EnumApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_enum_with_false_does_not_match0_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling EnumApi->post_enum_with_false_does_not_match0_response_body_for_content_types: %s\n" % e)
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
[**EnumWithFalseDoesNotMatch0**](EnumWithFalseDoesNotMatch0.md) |  | 



[**EnumWithFalseDoesNotMatch0**](EnumWithFalseDoesNotMatch0.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_enum_with_true_does_not_match1_request_body**
<a name="post_enum_with_true_does_not_match1_request_body"></a>
> post_enum_with_true_does_not_match1_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import enum_api
from unit_test_api.model.enum_with_true_does_not_match1 import EnumWithTrueDoesNotMatch1
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = enum_api.EnumApi(api_client)

    # example passing only required values which don't have defaults set
    body = EnumWithTrueDoesNotMatch1(True)
    try:
        api_response = api_instance.post_enum_with_true_does_not_match1_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling EnumApi->post_enum_with_true_does_not_match1_request_body: %s\n" % e)
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
[**EnumWithTrueDoesNotMatch1**](EnumWithTrueDoesNotMatch1.md) |  | 


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

# **post_enum_with_true_does_not_match1_response_body_for_content_types**
<a name="post_enum_with_true_does_not_match1_response_body_for_content_types"></a>
> EnumWithTrueDoesNotMatch1 post_enum_with_true_does_not_match1_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import enum_api
from unit_test_api.model.enum_with_true_does_not_match1 import EnumWithTrueDoesNotMatch1
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = enum_api.EnumApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_enum_with_true_does_not_match1_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling EnumApi->post_enum_with_true_does_not_match1_response_body_for_content_types: %s\n" % e)
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
[**EnumWithTrueDoesNotMatch1**](EnumWithTrueDoesNotMatch1.md) |  | 



[**EnumWithTrueDoesNotMatch1**](EnumWithTrueDoesNotMatch1.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_enums_in_properties_request_body**
<a name="post_enums_in_properties_request_body"></a>
> post_enums_in_properties_request_body(enums_in_properties)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import enum_api
from unit_test_api.model.enums_in_properties import EnumsInProperties
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = enum_api.EnumApi(api_client)

    # example passing only required values which don't have defaults set
    body = EnumsInProperties(
        foo="foo",
        bar="bar",
    )
    try:
        api_response = api_instance.post_enums_in_properties_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling EnumApi->post_enums_in_properties_request_body: %s\n" % e)
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
[**EnumsInProperties**](EnumsInProperties.md) |  | 


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

# **post_enums_in_properties_response_body_for_content_types**
<a name="post_enums_in_properties_response_body_for_content_types"></a>
> EnumsInProperties post_enums_in_properties_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import enum_api
from unit_test_api.model.enums_in_properties import EnumsInProperties
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = enum_api.EnumApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_enums_in_properties_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling EnumApi->post_enums_in_properties_response_body_for_content_types: %s\n" % e)
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
[**EnumsInProperties**](EnumsInProperties.md) |  | 



[**EnumsInProperties**](EnumsInProperties.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_nul_characters_in_strings_request_body**
<a name="post_nul_characters_in_strings_request_body"></a>
> post_nul_characters_in_strings_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import enum_api
from unit_test_api.model.nul_characters_in_strings import NulCharactersInStrings
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = enum_api.EnumApi(api_client)

    # example passing only required values which don't have defaults set
    body = NulCharactersInStrings("hello\x00there")
    try:
        api_response = api_instance.post_nul_characters_in_strings_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling EnumApi->post_nul_characters_in_strings_request_body: %s\n" % e)
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
[**NulCharactersInStrings**](NulCharactersInStrings.md) |  | 


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

# **post_nul_characters_in_strings_response_body_for_content_types**
<a name="post_nul_characters_in_strings_response_body_for_content_types"></a>
> NulCharactersInStrings post_nul_characters_in_strings_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import enum_api
from unit_test_api.model.nul_characters_in_strings import NulCharactersInStrings
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = enum_api.EnumApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_nul_characters_in_strings_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling EnumApi->post_nul_characters_in_strings_response_body_for_content_types: %s\n" % e)
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
[**NulCharactersInStrings**](NulCharactersInStrings.md) |  | 



[**NulCharactersInStrings**](NulCharactersInStrings.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_simple_enum_validation_request_body**
<a name="post_simple_enum_validation_request_body"></a>
> post_simple_enum_validation_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import enum_api
from unit_test_api.model.simple_enum_validation import SimpleEnumValidation
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = enum_api.EnumApi(api_client)

    # example passing only required values which don't have defaults set
    body = SimpleEnumValidation(1)
    try:
        api_response = api_instance.post_simple_enum_validation_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling EnumApi->post_simple_enum_validation_request_body: %s\n" % e)
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
[**SimpleEnumValidation**](SimpleEnumValidation.md) |  | 


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

# **post_simple_enum_validation_response_body_for_content_types**
<a name="post_simple_enum_validation_response_body_for_content_types"></a>
> SimpleEnumValidation post_simple_enum_validation_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import enum_api
from unit_test_api.model.simple_enum_validation import SimpleEnumValidation
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = enum_api.EnumApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_simple_enum_validation_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling EnumApi->post_simple_enum_validation_response_body_for_content_types: %s\n" % e)
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
[**SimpleEnumValidation**](SimpleEnumValidation.md) |  | 



[**SimpleEnumValidation**](SimpleEnumValidation.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

