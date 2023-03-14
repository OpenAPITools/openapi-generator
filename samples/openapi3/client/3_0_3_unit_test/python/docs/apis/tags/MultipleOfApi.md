<a name="__pageTop"></a>
# unit_test_api.apis.tags.multiple_of_api.MultipleOfApi

All URIs are relative to *https://someserver.com/v1*

Method | HTTP request | Description
------------- | ------------- | -------------
[**post_by_int_request_body**](#post_by_int_request_body) | **post** /requestBody/postByIntRequestBody | 
[**post_by_int_response_body_for_content_types**](#post_by_int_response_body_for_content_types) | **post** /responseBody/postByIntResponseBodyForContentTypes | 
[**post_by_number_request_body**](#post_by_number_request_body) | **post** /requestBody/postByNumberRequestBody | 
[**post_by_number_response_body_for_content_types**](#post_by_number_response_body_for_content_types) | **post** /responseBody/postByNumberResponseBodyForContentTypes | 
[**post_by_small_number_request_body**](#post_by_small_number_request_body) | **post** /requestBody/postBySmallNumberRequestBody | 
[**post_by_small_number_response_body_for_content_types**](#post_by_small_number_response_body_for_content_types) | **post** /responseBody/postBySmallNumberResponseBodyForContentTypes | 
[**post_invalid_instance_should_not_raise_error_when_float_division_inf_request_body**](#post_invalid_instance_should_not_raise_error_when_float_division_inf_request_body) | **post** /requestBody/postInvalidInstanceShouldNotRaiseErrorWhenFloatDivisionInfRequestBody | 
[**post_invalid_instance_should_not_raise_error_when_float_division_inf_response_body_for_content_types**](#post_invalid_instance_should_not_raise_error_when_float_division_inf_response_body_for_content_types) | **post** /responseBody/postInvalidInstanceShouldNotRaiseErrorWhenFloatDivisionInfResponseBodyForContentTypes | 

# **post_by_int_request_body**
<a name="post_by_int_request_body"></a>
> post_by_int_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import multiple_of_api
from unit_test_api.model.by_int import ByInt
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = multiple_of_api.MultipleOfApi(api_client)

    # example passing only required values which don't have defaults set
    body = ByInt(None)
    try:
        api_response = api_instance.post_by_int_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling MultipleOfApi->post_by_int_request_body: %s\n" % e)
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
[**ByInt**](../../models/ByInt.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_by_int_request_body.ApiResponseFor200) | success

#### post_by_int_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_by_int_response_body_for_content_types**
<a name="post_by_int_response_body_for_content_types"></a>
> ByInt post_by_int_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import multiple_of_api
from unit_test_api.model.by_int import ByInt
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = multiple_of_api.MultipleOfApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_by_int_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling MultipleOfApi->post_by_int_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_by_int_response_body_for_content_types.ApiResponseFor200) | success

#### post_by_int_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ByInt**](../../models/ByInt.md) |  | 


### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_by_number_request_body**
<a name="post_by_number_request_body"></a>
> post_by_number_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import multiple_of_api
from unit_test_api.model.by_number import ByNumber
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = multiple_of_api.MultipleOfApi(api_client)

    # example passing only required values which don't have defaults set
    body = ByNumber(None)
    try:
        api_response = api_instance.post_by_number_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling MultipleOfApi->post_by_number_request_body: %s\n" % e)
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
[**ByNumber**](../../models/ByNumber.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_by_number_request_body.ApiResponseFor200) | success

#### post_by_number_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_by_number_response_body_for_content_types**
<a name="post_by_number_response_body_for_content_types"></a>
> ByNumber post_by_number_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import multiple_of_api
from unit_test_api.model.by_number import ByNumber
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = multiple_of_api.MultipleOfApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_by_number_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling MultipleOfApi->post_by_number_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_by_number_response_body_for_content_types.ApiResponseFor200) | success

#### post_by_number_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ByNumber**](../../models/ByNumber.md) |  | 


### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_by_small_number_request_body**
<a name="post_by_small_number_request_body"></a>
> post_by_small_number_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import multiple_of_api
from unit_test_api.model.by_small_number import BySmallNumber
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = multiple_of_api.MultipleOfApi(api_client)

    # example passing only required values which don't have defaults set
    body = BySmallNumber(None)
    try:
        api_response = api_instance.post_by_small_number_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling MultipleOfApi->post_by_small_number_request_body: %s\n" % e)
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
[**BySmallNumber**](../../models/BySmallNumber.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_by_small_number_request_body.ApiResponseFor200) | success

#### post_by_small_number_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_by_small_number_response_body_for_content_types**
<a name="post_by_small_number_response_body_for_content_types"></a>
> BySmallNumber post_by_small_number_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import multiple_of_api
from unit_test_api.model.by_small_number import BySmallNumber
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = multiple_of_api.MultipleOfApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_by_small_number_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling MultipleOfApi->post_by_small_number_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_by_small_number_response_body_for_content_types.ApiResponseFor200) | success

#### post_by_small_number_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**BySmallNumber**](../../models/BySmallNumber.md) |  | 


### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_invalid_instance_should_not_raise_error_when_float_division_inf_request_body**
<a name="post_invalid_instance_should_not_raise_error_when_float_division_inf_request_body"></a>
> post_invalid_instance_should_not_raise_error_when_float_division_inf_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import multiple_of_api
from unit_test_api.model.invalid_instance_should_not_raise_error_when_float_division_inf import InvalidInstanceShouldNotRaiseErrorWhenFloatDivisionInf
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = multiple_of_api.MultipleOfApi(api_client)

    # example passing only required values which don't have defaults set
    body = InvalidInstanceShouldNotRaiseErrorWhenFloatDivisionInf(1)
    try:
        api_response = api_instance.post_invalid_instance_should_not_raise_error_when_float_division_inf_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling MultipleOfApi->post_invalid_instance_should_not_raise_error_when_float_division_inf_request_body: %s\n" % e)
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
[**InvalidInstanceShouldNotRaiseErrorWhenFloatDivisionInf**](../../models/InvalidInstanceShouldNotRaiseErrorWhenFloatDivisionInf.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_invalid_instance_should_not_raise_error_when_float_division_inf_request_body.ApiResponseFor200) | success

#### post_invalid_instance_should_not_raise_error_when_float_division_inf_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_invalid_instance_should_not_raise_error_when_float_division_inf_response_body_for_content_types**
<a name="post_invalid_instance_should_not_raise_error_when_float_division_inf_response_body_for_content_types"></a>
> InvalidInstanceShouldNotRaiseErrorWhenFloatDivisionInf post_invalid_instance_should_not_raise_error_when_float_division_inf_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import multiple_of_api
from unit_test_api.model.invalid_instance_should_not_raise_error_when_float_division_inf import InvalidInstanceShouldNotRaiseErrorWhenFloatDivisionInf
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = multiple_of_api.MultipleOfApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_invalid_instance_should_not_raise_error_when_float_division_inf_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling MultipleOfApi->post_invalid_instance_should_not_raise_error_when_float_division_inf_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_invalid_instance_should_not_raise_error_when_float_division_inf_response_body_for_content_types.ApiResponseFor200) | success

#### post_invalid_instance_should_not_raise_error_when_float_division_inf_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**InvalidInstanceShouldNotRaiseErrorWhenFloatDivisionInf**](../../models/InvalidInstanceShouldNotRaiseErrorWhenFloatDivisionInf.md) |  | 


### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

