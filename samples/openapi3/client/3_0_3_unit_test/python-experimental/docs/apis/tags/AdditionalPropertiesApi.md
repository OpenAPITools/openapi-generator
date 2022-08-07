<a name="__pageTop"></a>
# unit_test_api.apis.tags.additional_properties_api.AdditionalPropertiesApi

All URIs are relative to *https://someserver.com/v1*

Method | HTTP request | Description
------------- | ------------- | -------------
[**post_additionalproperties_allows_a_schema_which_should_validate_request_body**](#post_additionalproperties_allows_a_schema_which_should_validate_request_body) | **post** /requestBody/postAdditionalpropertiesAllowsASchemaWhichShouldValidateRequestBody | 
[**post_additionalproperties_allows_a_schema_which_should_validate_response_body_for_content_types**](#post_additionalproperties_allows_a_schema_which_should_validate_response_body_for_content_types) | **post** /responseBody/postAdditionalpropertiesAllowsASchemaWhichShouldValidateResponseBodyForContentTypes | 
[**post_additionalproperties_are_allowed_by_default_request_body**](#post_additionalproperties_are_allowed_by_default_request_body) | **post** /requestBody/postAdditionalpropertiesAreAllowedByDefaultRequestBody | 
[**post_additionalproperties_are_allowed_by_default_response_body_for_content_types**](#post_additionalproperties_are_allowed_by_default_response_body_for_content_types) | **post** /responseBody/postAdditionalpropertiesAreAllowedByDefaultResponseBodyForContentTypes | 
[**post_additionalproperties_can_exist_by_itself_request_body**](#post_additionalproperties_can_exist_by_itself_request_body) | **post** /requestBody/postAdditionalpropertiesCanExistByItselfRequestBody | 
[**post_additionalproperties_can_exist_by_itself_response_body_for_content_types**](#post_additionalproperties_can_exist_by_itself_response_body_for_content_types) | **post** /responseBody/postAdditionalpropertiesCanExistByItselfResponseBodyForContentTypes | 
[**post_additionalproperties_should_not_look_in_applicators_request_body**](#post_additionalproperties_should_not_look_in_applicators_request_body) | **post** /requestBody/postAdditionalpropertiesShouldNotLookInApplicatorsRequestBody | 
[**post_additionalproperties_should_not_look_in_applicators_response_body_for_content_types**](#post_additionalproperties_should_not_look_in_applicators_response_body_for_content_types) | **post** /responseBody/postAdditionalpropertiesShouldNotLookInApplicatorsResponseBodyForContentTypes | 

# **post_additionalproperties_allows_a_schema_which_should_validate_request_body**
<a name="post_additionalproperties_allows_a_schema_which_should_validate_request_body"></a>
> post_additionalproperties_allows_a_schema_which_should_validate_request_body(additionalproperties_allows_a_schema_which_should_validate)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import additional_properties_api
from unit_test_api.model.additionalproperties_allows_a_schema_which_should_validate import AdditionalpropertiesAllowsASchemaWhichShouldValidate
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = additional_properties_api.AdditionalPropertiesApi(api_client)

    # example passing only required values which don't have defaults set
    body = AdditionalpropertiesAllowsASchemaWhichShouldValidate(
        foo=None,
        bar=None,
    )
    try:
        api_response = api_instance.post_additionalproperties_allows_a_schema_which_should_validate_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling AdditionalPropertiesApi->post_additionalproperties_allows_a_schema_which_should_validate_request_body: %s\n" % e)
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
[**AdditionalpropertiesAllowsASchemaWhichShouldValidate**](AdditionalpropertiesAllowsASchemaWhichShouldValidate.md) |  | 


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

# **post_additionalproperties_allows_a_schema_which_should_validate_response_body_for_content_types**
<a name="post_additionalproperties_allows_a_schema_which_should_validate_response_body_for_content_types"></a>
> AdditionalpropertiesAllowsASchemaWhichShouldValidate post_additionalproperties_allows_a_schema_which_should_validate_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import additional_properties_api
from unit_test_api.model.additionalproperties_allows_a_schema_which_should_validate import AdditionalpropertiesAllowsASchemaWhichShouldValidate
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = additional_properties_api.AdditionalPropertiesApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_additionalproperties_allows_a_schema_which_should_validate_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling AdditionalPropertiesApi->post_additionalproperties_allows_a_schema_which_should_validate_response_body_for_content_types: %s\n" % e)
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
[**AdditionalpropertiesAllowsASchemaWhichShouldValidate**](AdditionalpropertiesAllowsASchemaWhichShouldValidate.md) |  | 



[**AdditionalpropertiesAllowsASchemaWhichShouldValidate**](AdditionalpropertiesAllowsASchemaWhichShouldValidate.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_additionalproperties_are_allowed_by_default_request_body**
<a name="post_additionalproperties_are_allowed_by_default_request_body"></a>
> post_additionalproperties_are_allowed_by_default_request_body(additionalproperties_are_allowed_by_default)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import additional_properties_api
from unit_test_api.model.additionalproperties_are_allowed_by_default import AdditionalpropertiesAreAllowedByDefault
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = additional_properties_api.AdditionalPropertiesApi(api_client)

    # example passing only required values which don't have defaults set
    body = AdditionalpropertiesAreAllowedByDefault(None)
    try:
        api_response = api_instance.post_additionalproperties_are_allowed_by_default_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling AdditionalPropertiesApi->post_additionalproperties_are_allowed_by_default_request_body: %s\n" % e)
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
[**AdditionalpropertiesAreAllowedByDefault**](AdditionalpropertiesAreAllowedByDefault.md) |  | 


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

# **post_additionalproperties_are_allowed_by_default_response_body_for_content_types**
<a name="post_additionalproperties_are_allowed_by_default_response_body_for_content_types"></a>
> AdditionalpropertiesAreAllowedByDefault post_additionalproperties_are_allowed_by_default_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import additional_properties_api
from unit_test_api.model.additionalproperties_are_allowed_by_default import AdditionalpropertiesAreAllowedByDefault
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = additional_properties_api.AdditionalPropertiesApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_additionalproperties_are_allowed_by_default_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling AdditionalPropertiesApi->post_additionalproperties_are_allowed_by_default_response_body_for_content_types: %s\n" % e)
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
[**AdditionalpropertiesAreAllowedByDefault**](AdditionalpropertiesAreAllowedByDefault.md) |  | 



[**AdditionalpropertiesAreAllowedByDefault**](AdditionalpropertiesAreAllowedByDefault.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_additionalproperties_can_exist_by_itself_request_body**
<a name="post_additionalproperties_can_exist_by_itself_request_body"></a>
> post_additionalproperties_can_exist_by_itself_request_body(additionalproperties_can_exist_by_itself)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import additional_properties_api
from unit_test_api.model.additionalproperties_can_exist_by_itself import AdditionalpropertiesCanExistByItself
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = additional_properties_api.AdditionalPropertiesApi(api_client)

    # example passing only required values which don't have defaults set
    body = AdditionalpropertiesCanExistByItself(
        key=True,
    )
    try:
        api_response = api_instance.post_additionalproperties_can_exist_by_itself_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling AdditionalPropertiesApi->post_additionalproperties_can_exist_by_itself_request_body: %s\n" % e)
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
[**AdditionalpropertiesCanExistByItself**](AdditionalpropertiesCanExistByItself.md) |  | 


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

# **post_additionalproperties_can_exist_by_itself_response_body_for_content_types**
<a name="post_additionalproperties_can_exist_by_itself_response_body_for_content_types"></a>
> AdditionalpropertiesCanExistByItself post_additionalproperties_can_exist_by_itself_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import additional_properties_api
from unit_test_api.model.additionalproperties_can_exist_by_itself import AdditionalpropertiesCanExistByItself
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = additional_properties_api.AdditionalPropertiesApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_additionalproperties_can_exist_by_itself_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling AdditionalPropertiesApi->post_additionalproperties_can_exist_by_itself_response_body_for_content_types: %s\n" % e)
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
[**AdditionalpropertiesCanExistByItself**](AdditionalpropertiesCanExistByItself.md) |  | 



[**AdditionalpropertiesCanExistByItself**](AdditionalpropertiesCanExistByItself.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_additionalproperties_should_not_look_in_applicators_request_body**
<a name="post_additionalproperties_should_not_look_in_applicators_request_body"></a>
> post_additionalproperties_should_not_look_in_applicators_request_body(additionalproperties_should_not_look_in_applicators)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import additional_properties_api
from unit_test_api.model.additionalproperties_should_not_look_in_applicators import AdditionalpropertiesShouldNotLookInApplicators
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = additional_properties_api.AdditionalPropertiesApi(api_client)

    # example passing only required values which don't have defaults set
    body = AdditionalpropertiesShouldNotLookInApplicators(None)
    try:
        api_response = api_instance.post_additionalproperties_should_not_look_in_applicators_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling AdditionalPropertiesApi->post_additionalproperties_should_not_look_in_applicators_request_body: %s\n" % e)
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
[**AdditionalpropertiesShouldNotLookInApplicators**](AdditionalpropertiesShouldNotLookInApplicators.md) |  | 


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

# **post_additionalproperties_should_not_look_in_applicators_response_body_for_content_types**
<a name="post_additionalproperties_should_not_look_in_applicators_response_body_for_content_types"></a>
> AdditionalpropertiesShouldNotLookInApplicators post_additionalproperties_should_not_look_in_applicators_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import additional_properties_api
from unit_test_api.model.additionalproperties_should_not_look_in_applicators import AdditionalpropertiesShouldNotLookInApplicators
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = additional_properties_api.AdditionalPropertiesApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_additionalproperties_should_not_look_in_applicators_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling AdditionalPropertiesApi->post_additionalproperties_should_not_look_in_applicators_response_body_for_content_types: %s\n" % e)
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
[**AdditionalpropertiesShouldNotLookInApplicators**](AdditionalpropertiesShouldNotLookInApplicators.md) |  | 



[**AdditionalpropertiesShouldNotLookInApplicators**](AdditionalpropertiesShouldNotLookInApplicators.md)

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

