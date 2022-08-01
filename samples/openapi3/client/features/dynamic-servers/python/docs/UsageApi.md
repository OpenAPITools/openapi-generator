# dynamic_servers.UsageApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**custom_server**](UsageApi.md#custom_server) | **GET** /custom | Use custom server
[**default_server**](UsageApi.md#default_server) | **GET** /default | Use default server


# **custom_server**
> {str: (bool, date, datetime, dict, float, int, list, str, none_type)} custom_server()

Use custom server

Use custom server

### Example


```python
import time
import dynamic_servers
from dynamic_servers.api import usage_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = dynamic_servers.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with dynamic_servers.ApiClient() as api_client:
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

### Return type

**{str: (bool, date, datetime, dict, float, int, list, str, none_type)}**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **default_server**
> {str: (bool, date, datetime, dict, float, int, list, str, none_type)} default_server()

Use default server

Use default server

### Example


```python
import time
import dynamic_servers
from dynamic_servers.api import usage_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = dynamic_servers.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with dynamic_servers.ApiClient() as api_client:
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

### Return type

**{str: (bool, date, datetime, dict, float, int, list, str, none_type)}**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

