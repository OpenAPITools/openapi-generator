<a name="__pageTop"></a>
# unit_test_api.apis.tags.properties_api.PropertiesApi

All URIs are relative to *https://someserver.com/v1*

Method | HTTP request | Description
------------- | ------------- | -------------
[**post_object_properties_validation_request_body**](#post_object_properties_validation_request_body) | **post** /requestBody/postObjectPropertiesValidationRequestBody | 
[**post_object_properties_validation_response_body_for_content_types**](#post_object_properties_validation_response_body_for_content_types) | **post** /responseBody/postObjectPropertiesValidationResponseBodyForContentTypes | 
[**post_properties_with_escaped_characters_request_body**](#post_properties_with_escaped_characters_request_body) | **post** /requestBody/postPropertiesWithEscapedCharactersRequestBody | 
[**post_properties_with_escaped_characters_response_body_for_content_types**](#post_properties_with_escaped_characters_response_body_for_content_types) | **post** /responseBody/postPropertiesWithEscapedCharactersResponseBodyForContentTypes | 

# **post_object_properties_validation_request_body**
<a name="post_object_properties_validation_request_body"></a>
> post_object_properties_validation_request_body(object_properties_validation)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import properties_api
from unit_test_api.model.object_properties_validation import ObjectPropertiesValidation
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = properties_api.PropertiesApi(api_client)

    # example passing only required values which don't have defaults set
    body = ObjectPropertiesValidation(None)
    try:
        api_response = api_instance.post_object_properties_validation_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling PropertiesApi->post_object_properties_validation_request_body: %s\n" % e)
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
[**ObjectPropertiesValidation**](../../models/ObjectPropertiesValidation.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_object_properties_validation_request_body.ApiResponseFor200) | success

#### post_object_properties_validation_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_object_properties_validation_response_body_for_content_types**
<a name="post_object_properties_validation_response_body_for_content_types"></a>
> ObjectPropertiesValidation post_object_properties_validation_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import properties_api
from unit_test_api.model.object_properties_validation import ObjectPropertiesValidation
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = properties_api.PropertiesApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_object_properties_validation_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling PropertiesApi->post_object_properties_validation_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_object_properties_validation_response_body_for_content_types.ApiResponseFor200) | success

#### post_object_properties_validation_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ObjectPropertiesValidation**](../../models/ObjectPropertiesValidation.md) |  | 


### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_properties_with_escaped_characters_request_body**
<a name="post_properties_with_escaped_characters_request_body"></a>
> post_properties_with_escaped_characters_request_body(properties_with_escaped_characters)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import properties_api
from unit_test_api.model.properties_with_escaped_characters import PropertiesWithEscapedCharacters
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = properties_api.PropertiesApi(api_client)

    # example passing only required values which don't have defaults set
    body = PropertiesWithEscapedCharacters(None)
    try:
        api_response = api_instance.post_properties_with_escaped_characters_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling PropertiesApi->post_properties_with_escaped_characters_request_body: %s\n" % e)
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
[**PropertiesWithEscapedCharacters**](../../models/PropertiesWithEscapedCharacters.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_properties_with_escaped_characters_request_body.ApiResponseFor200) | success

#### post_properties_with_escaped_characters_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_properties_with_escaped_characters_response_body_for_content_types**
<a name="post_properties_with_escaped_characters_response_body_for_content_types"></a>
> PropertiesWithEscapedCharacters post_properties_with_escaped_characters_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import properties_api
from unit_test_api.model.properties_with_escaped_characters import PropertiesWithEscapedCharacters
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = properties_api.PropertiesApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_properties_with_escaped_characters_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling PropertiesApi->post_properties_with_escaped_characters_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_properties_with_escaped_characters_response_body_for_content_types.ApiResponseFor200) | success

#### post_properties_with_escaped_characters_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**PropertiesWithEscapedCharacters**](../../models/PropertiesWithEscapedCharacters.md) |  | 


### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

