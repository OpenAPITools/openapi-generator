<a name="__pageTop"></a>
# unit_test_api.apis.tags.ref_api.RefApi

All URIs are relative to *https://someserver.com/v1*

Method | HTTP request | Description
------------- | ------------- | -------------
[**post_property_named_ref_that_is_not_a_reference_request_body**](#post_property_named_ref_that_is_not_a_reference_request_body) | **post** /requestBody/postPropertyNamedRefThatIsNotAReferenceRequestBody | 
[**post_property_named_ref_that_is_not_a_reference_response_body_for_content_types**](#post_property_named_ref_that_is_not_a_reference_response_body_for_content_types) | **post** /responseBody/postPropertyNamedRefThatIsNotAReferenceResponseBodyForContentTypes | 
[**post_ref_in_additionalproperties_request_body**](#post_ref_in_additionalproperties_request_body) | **post** /requestBody/postRefInAdditionalpropertiesRequestBody | 
[**post_ref_in_additionalproperties_response_body_for_content_types**](#post_ref_in_additionalproperties_response_body_for_content_types) | **post** /responseBody/postRefInAdditionalpropertiesResponseBodyForContentTypes | 
[**post_ref_in_allof_request_body**](#post_ref_in_allof_request_body) | **post** /requestBody/postRefInAllofRequestBody | 
[**post_ref_in_allof_response_body_for_content_types**](#post_ref_in_allof_response_body_for_content_types) | **post** /responseBody/postRefInAllofResponseBodyForContentTypes | 
[**post_ref_in_anyof_request_body**](#post_ref_in_anyof_request_body) | **post** /requestBody/postRefInAnyofRequestBody | 
[**post_ref_in_anyof_response_body_for_content_types**](#post_ref_in_anyof_response_body_for_content_types) | **post** /responseBody/postRefInAnyofResponseBodyForContentTypes | 
[**post_ref_in_items_request_body**](#post_ref_in_items_request_body) | **post** /requestBody/postRefInItemsRequestBody | 
[**post_ref_in_items_response_body_for_content_types**](#post_ref_in_items_response_body_for_content_types) | **post** /responseBody/postRefInItemsResponseBodyForContentTypes | 
[**post_ref_in_not_request_body**](#post_ref_in_not_request_body) | **post** /requestBody/postRefInNotRequestBody | 
[**post_ref_in_not_response_body_for_content_types**](#post_ref_in_not_response_body_for_content_types) | **post** /responseBody/postRefInNotResponseBodyForContentTypes | 
[**post_ref_in_oneof_request_body**](#post_ref_in_oneof_request_body) | **post** /requestBody/postRefInOneofRequestBody | 
[**post_ref_in_oneof_response_body_for_content_types**](#post_ref_in_oneof_response_body_for_content_types) | **post** /responseBody/postRefInOneofResponseBodyForContentTypes | 
[**post_ref_in_property_request_body**](#post_ref_in_property_request_body) | **post** /requestBody/postRefInPropertyRequestBody | 
[**post_ref_in_property_response_body_for_content_types**](#post_ref_in_property_response_body_for_content_types) | **post** /responseBody/postRefInPropertyResponseBodyForContentTypes | 

# **post_property_named_ref_that_is_not_a_reference_request_body**
<a name="post_property_named_ref_that_is_not_a_reference_request_body"></a>
> post_property_named_ref_that_is_not_a_reference_request_body(property_named_ref_that_is_not_a_reference)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
from unit_test_api.model.property_named_ref_that_is_not_a_reference import PropertyNamedRefThatIsNotAReference
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = ref_api.RefApi(api_client)

    # example passing only required values which don't have defaults set
    body = PropertyNamedRefThatIsNotAReference(None)
    try:
        api_response = api_instance.post_property_named_ref_that_is_not_a_reference_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling RefApi->post_property_named_ref_that_is_not_a_reference_request_body: %s\n" % e)
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
[**PropertyNamedRefThatIsNotAReference**](../../models/PropertyNamedRefThatIsNotAReference.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_property_named_ref_that_is_not_a_reference_request_body.ApiResponseFor200) | success

#### post_property_named_ref_that_is_not_a_reference_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_property_named_ref_that_is_not_a_reference_response_body_for_content_types**
<a name="post_property_named_ref_that_is_not_a_reference_response_body_for_content_types"></a>
> PropertyNamedRefThatIsNotAReference post_property_named_ref_that_is_not_a_reference_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
from unit_test_api.model.property_named_ref_that_is_not_a_reference import PropertyNamedRefThatIsNotAReference
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = ref_api.RefApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_property_named_ref_that_is_not_a_reference_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling RefApi->post_property_named_ref_that_is_not_a_reference_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_property_named_ref_that_is_not_a_reference_response_body_for_content_types.ApiResponseFor200) | success

#### post_property_named_ref_that_is_not_a_reference_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**PropertyNamedRefThatIsNotAReference**](../../models/PropertyNamedRefThatIsNotAReference.md) |  | 


### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_ref_in_additionalproperties_request_body**
<a name="post_ref_in_additionalproperties_request_body"></a>
> post_ref_in_additionalproperties_request_body(ref_in_additionalproperties)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
from unit_test_api.model.ref_in_additionalproperties import RefInAdditionalproperties
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = ref_api.RefApi(api_client)

    # example passing only required values which don't have defaults set
    body = RefInAdditionalproperties(
        key=PropertyNamedRefThatIsNotAReference(None),
    )
    try:
        api_response = api_instance.post_ref_in_additionalproperties_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling RefApi->post_ref_in_additionalproperties_request_body: %s\n" % e)
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
[**RefInAdditionalproperties**](../../models/RefInAdditionalproperties.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_ref_in_additionalproperties_request_body.ApiResponseFor200) | success

#### post_ref_in_additionalproperties_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_ref_in_additionalproperties_response_body_for_content_types**
<a name="post_ref_in_additionalproperties_response_body_for_content_types"></a>
> RefInAdditionalproperties post_ref_in_additionalproperties_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
from unit_test_api.model.ref_in_additionalproperties import RefInAdditionalproperties
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = ref_api.RefApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_ref_in_additionalproperties_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling RefApi->post_ref_in_additionalproperties_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_ref_in_additionalproperties_response_body_for_content_types.ApiResponseFor200) | success

#### post_ref_in_additionalproperties_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**RefInAdditionalproperties**](../../models/RefInAdditionalproperties.md) |  | 


### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_ref_in_allof_request_body**
<a name="post_ref_in_allof_request_body"></a>
> post_ref_in_allof_request_body(ref_in_allof)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
from unit_test_api.model.ref_in_allof import RefInAllof
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = ref_api.RefApi(api_client)

    # example passing only required values which don't have defaults set
    body = RefInAllof(None)
    try:
        api_response = api_instance.post_ref_in_allof_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling RefApi->post_ref_in_allof_request_body: %s\n" % e)
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
[**RefInAllof**](../../models/RefInAllof.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_ref_in_allof_request_body.ApiResponseFor200) | success

#### post_ref_in_allof_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_ref_in_allof_response_body_for_content_types**
<a name="post_ref_in_allof_response_body_for_content_types"></a>
> RefInAllof post_ref_in_allof_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
from unit_test_api.model.ref_in_allof import RefInAllof
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = ref_api.RefApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_ref_in_allof_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling RefApi->post_ref_in_allof_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_ref_in_allof_response_body_for_content_types.ApiResponseFor200) | success

#### post_ref_in_allof_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**RefInAllof**](../../models/RefInAllof.md) |  | 


### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_ref_in_anyof_request_body**
<a name="post_ref_in_anyof_request_body"></a>
> post_ref_in_anyof_request_body(ref_in_anyof)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
from unit_test_api.model.ref_in_anyof import RefInAnyof
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = ref_api.RefApi(api_client)

    # example passing only required values which don't have defaults set
    body = RefInAnyof(None)
    try:
        api_response = api_instance.post_ref_in_anyof_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling RefApi->post_ref_in_anyof_request_body: %s\n" % e)
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
[**RefInAnyof**](../../models/RefInAnyof.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_ref_in_anyof_request_body.ApiResponseFor200) | success

#### post_ref_in_anyof_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_ref_in_anyof_response_body_for_content_types**
<a name="post_ref_in_anyof_response_body_for_content_types"></a>
> RefInAnyof post_ref_in_anyof_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
from unit_test_api.model.ref_in_anyof import RefInAnyof
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = ref_api.RefApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_ref_in_anyof_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling RefApi->post_ref_in_anyof_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_ref_in_anyof_response_body_for_content_types.ApiResponseFor200) | success

#### post_ref_in_anyof_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**RefInAnyof**](../../models/RefInAnyof.md) |  | 


### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_ref_in_items_request_body**
<a name="post_ref_in_items_request_body"></a>
> post_ref_in_items_request_body(ref_in_items)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
from unit_test_api.model.ref_in_items import RefInItems
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = ref_api.RefApi(api_client)

    # example passing only required values which don't have defaults set
    body = RefInItems([
        PropertyNamedRefThatIsNotAReference(None)
    ])
    try:
        api_response = api_instance.post_ref_in_items_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling RefApi->post_ref_in_items_request_body: %s\n" % e)
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
[**RefInItems**](../../models/RefInItems.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_ref_in_items_request_body.ApiResponseFor200) | success

#### post_ref_in_items_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_ref_in_items_response_body_for_content_types**
<a name="post_ref_in_items_response_body_for_content_types"></a>
> RefInItems post_ref_in_items_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
from unit_test_api.model.ref_in_items import RefInItems
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = ref_api.RefApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_ref_in_items_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling RefApi->post_ref_in_items_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_ref_in_items_response_body_for_content_types.ApiResponseFor200) | success

#### post_ref_in_items_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**RefInItems**](../../models/RefInItems.md) |  | 


### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_ref_in_not_request_body**
<a name="post_ref_in_not_request_body"></a>
> post_ref_in_not_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
from unit_test_api.model.property_named_ref_that_is_not_a_reference import PropertyNamedRefThatIsNotAReference
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = ref_api.RefApi(api_client)

    # example passing only required values which don't have defaults set
    body = None
    try:
        api_response = api_instance.post_ref_in_not_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling RefApi->post_ref_in_not_request_body: %s\n" % e)
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
dict, frozendict.frozendict, str, date, datetime, uuid.UUID, int, float, decimal.Decimal, bool, None, list, tuple, bytes, io.FileIO, io.BufferedReader,  | frozendict.frozendict, str, decimal.Decimal, BoolClass, NoneClass, tuple, bytes, FileIO |  | 

### Composed Schemas (allOf/anyOf/oneOf/not)
#### not
Class Name | Input Type | Accessed Type | Description | Notes
------------- | ------------- | ------------- | ------------- | -------------
[PropertyNamedRefThatIsNotAReference]({{complexTypePrefix}}PropertyNamedRefThatIsNotAReference.md) | [**PropertyNamedRefThatIsNotAReference**]({{complexTypePrefix}}PropertyNamedRefThatIsNotAReference.md) | [**PropertyNamedRefThatIsNotAReference**]({{complexTypePrefix}}PropertyNamedRefThatIsNotAReference.md) |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_ref_in_not_request_body.ApiResponseFor200) | success

#### post_ref_in_not_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_ref_in_not_response_body_for_content_types**
<a name="post_ref_in_not_response_body_for_content_types"></a>
> bool, date, datetime, dict, float, int, list, str, none_type post_ref_in_not_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
from unit_test_api.model.property_named_ref_that_is_not_a_reference import PropertyNamedRefThatIsNotAReference
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = ref_api.RefApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_ref_in_not_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling RefApi->post_ref_in_not_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_ref_in_not_response_body_for_content_types.ApiResponseFor200) | success

#### post_ref_in_not_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson

## Model Type Info
Input Type | Accessed Type | Description | Notes
------------ | ------------- | ------------- | -------------
dict, frozendict.frozendict, str, date, datetime, uuid.UUID, int, float, decimal.Decimal, bool, None, list, tuple, bytes, io.FileIO, io.BufferedReader,  | frozendict.frozendict, str, decimal.Decimal, BoolClass, NoneClass, tuple, bytes, FileIO |  | 

### Composed Schemas (allOf/anyOf/oneOf/not)
#### not
Class Name | Input Type | Accessed Type | Description | Notes
------------- | ------------- | ------------- | ------------- | -------------
[PropertyNamedRefThatIsNotAReference]({{complexTypePrefix}}PropertyNamedRefThatIsNotAReference.md) | [**PropertyNamedRefThatIsNotAReference**]({{complexTypePrefix}}PropertyNamedRefThatIsNotAReference.md) | [**PropertyNamedRefThatIsNotAReference**]({{complexTypePrefix}}PropertyNamedRefThatIsNotAReference.md) |  | 

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_ref_in_oneof_request_body**
<a name="post_ref_in_oneof_request_body"></a>
> post_ref_in_oneof_request_body(ref_in_oneof)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
from unit_test_api.model.ref_in_oneof import RefInOneof
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = ref_api.RefApi(api_client)

    # example passing only required values which don't have defaults set
    body = RefInOneof(None)
    try:
        api_response = api_instance.post_ref_in_oneof_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling RefApi->post_ref_in_oneof_request_body: %s\n" % e)
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
[**RefInOneof**](../../models/RefInOneof.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_ref_in_oneof_request_body.ApiResponseFor200) | success

#### post_ref_in_oneof_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_ref_in_oneof_response_body_for_content_types**
<a name="post_ref_in_oneof_response_body_for_content_types"></a>
> RefInOneof post_ref_in_oneof_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
from unit_test_api.model.ref_in_oneof import RefInOneof
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = ref_api.RefApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_ref_in_oneof_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling RefApi->post_ref_in_oneof_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_ref_in_oneof_response_body_for_content_types.ApiResponseFor200) | success

#### post_ref_in_oneof_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**RefInOneof**](../../models/RefInOneof.md) |  | 


### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_ref_in_property_request_body**
<a name="post_ref_in_property_request_body"></a>
> post_ref_in_property_request_body(ref_in_property)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
from unit_test_api.model.ref_in_property import RefInProperty
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = ref_api.RefApi(api_client)

    # example passing only required values which don't have defaults set
    body = RefInProperty(None)
    try:
        api_response = api_instance.post_ref_in_property_request_body(
            body=body,
        )
    except unit_test_api.ApiException as e:
        print("Exception when calling RefApi->post_ref_in_property_request_body: %s\n" % e)
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
[**RefInProperty**](../../models/RefInProperty.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_ref_in_property_request_body.ApiResponseFor200) | success

#### post_ref_in_property_request_body.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

# **post_ref_in_property_response_body_for_content_types**
<a name="post_ref_in_property_response_body_for_content_types"></a>
> RefInProperty post_ref_in_property_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
from unit_test_api.model.ref_in_property import RefInProperty
from pprint import pprint
# Defining the host is optional and defaults to https://someserver.com/v1
# See configuration.py for a list of all supported configuration parameters.
configuration = unit_test_api.Configuration(
    host = "https://someserver.com/v1"
)

# Enter a context with an instance of the API client
with unit_test_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = ref_api.RefApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.post_ref_in_property_response_body_for_content_types()
        pprint(api_response)
    except unit_test_api.ApiException as e:
        print("Exception when calling RefApi->post_ref_in_property_response_body_for_content_types: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#post_ref_in_property_response_body_for_content_types.ApiResponseFor200) | success

#### post_ref_in_property_response_body_for_content_types.ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

# SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**RefInProperty**](../../models/RefInProperty.md) |  | 


### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

