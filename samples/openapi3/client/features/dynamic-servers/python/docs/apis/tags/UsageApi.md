<a name="__pageTop"></a>
# dynamic_servers.apis.tags.usage_api.UsageApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**custom_server**](#custom_server) | **get** /custom | Use custom server
[**default_server**](#default_server) | **get** /default | Use default server

# **custom_server**
<a name="custom_server"></a>
> {str: (bool, date, datetime, dict, float, int, list, str, none_type)} custom_server()

Use custom server

Use custom server

### Example

```python
import dynamic_servers
from dynamic_servers.apis.tags import usage_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = dynamic_servers.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with dynamic_servers.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = usage_api.UsageApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        # Use custom server
        api_response = api_instance.custom_server()
        pprint(api_response)
    except dynamic_servers.ApiException as e:
        print("Exception when calling UsageApi->custom_server: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#custom_server.ApiResponseFor200) | successful operation

#### custom_server.ApiResponseFor200
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

# **default_server**
<a name="default_server"></a>
> {str: (bool, date, datetime, dict, float, int, list, str, none_type)} default_server()

Use default server

Use default server

### Example

```python
import dynamic_servers
from dynamic_servers.apis.tags import usage_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = dynamic_servers.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with dynamic_servers.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = usage_api.UsageApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        # Use default server
        api_response = api_instance.default_server()
        pprint(api_response)
    except dynamic_servers.ApiException as e:
        print("Exception when calling UsageApi->default_server: %s\n" % e)
```
### Parameters
This endpoint does not need any parameter.

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | [ApiResponseFor200](#default_server.ApiResponseFor200) | successful operation

#### default_server.ApiResponseFor200
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

