<a name="__pageTop"></a>
# unit_test_api.apis.tags.ref_api.RefApi

All URIs are relative to *https://someserver.com/v1*

Method | HTTP request | Description
------------- | ------------- | -------------
[**post_ref_in_items_request_body**](#post_ref_in_items_request_body) | **post** /requestBody/postRefInItemsRequestBody | 
[**post_ref_in_items_response_body_for_content_types**](#post_ref_in_items_response_body_for_content_types) | **post** /responseBody/postRefInItemsResponseBodyForContentTypes | 

# **post_ref_in_items_request_body**
<a name="post_ref_in_items_request_body"></a>
> post_ref_in_items_request_body(body)



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
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

#### SchemaForRequestBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
typing.Union[dict, frozendict.frozendict, str, date, datetime, uuid.UUID, int, float, decimal.Decimal, bool, None, list, tuple, bytes, ] | |

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

# **post_ref_in_items_response_body_for_content_types**
<a name="post_ref_in_items_response_body_for_content_types"></a>
> bool, date, datetime, dict, float, int, list, str, none_type post_ref_in_items_response_body_for_content_types()



### Example

```python
import unit_test_api
from unit_test_api.apis.tags import ref_api
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
200 | ApiResponseFor200 | success

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
typing.Union[dict, frozendict.frozendict, str, date, datetime, uuid.UUID, int, float, decimal.Decimal, bool, None, list, tuple, bytes, ] | |


**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

[[Back to top]](#__pageTop) [[Back to API list]](../../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../../README.md#documentation-for-models) [[Back to README]](../../../README.md)

