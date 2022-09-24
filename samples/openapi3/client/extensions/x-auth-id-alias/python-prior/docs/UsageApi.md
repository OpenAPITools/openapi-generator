# x_auth_id_alias.UsageApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**any_key**](UsageApi.md#any_key) | **GET** /any | Use any API key
[**both_keys**](UsageApi.md#both_keys) | **GET** /both | Use both API keys
[**key_in_header**](UsageApi.md#key_in_header) | **GET** /header | Use API key in header
[**key_in_query**](UsageApi.md#key_in_query) | **GET** /query | Use API key in query


# **any_key**
> {str: (bool, date, datetime, dict, float, int, list, str, none_type)} any_key()

Use any API key

Use any API key

### Example

* Api Key Authentication (api_key):
* Api Key Authentication (api_key_query):

```python
import time
import x_auth_id_alias
from x_auth_id_alias.api import usage_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = x_auth_id_alias.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure API key authorization: api_key
configuration.api_key['api_key'] = 'YOUR_API_KEY'

# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
# configuration.api_key_prefix['api_key'] = 'Bearer'

# Configure API key authorization: api_key_query
configuration.api_key['api_key_query'] = 'YOUR_API_KEY'

# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
# configuration.api_key_prefix['api_key_query'] = 'Bearer'

# Enter a context with an instance of the API client
with x_auth_id_alias.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = usage_api.UsageApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        # Use any API key
        api_response = api_instance.any_key()
        pprint(api_response)
    except x_auth_id_alias.ApiException as e:
        print("Exception when calling UsageApi->any_key: %s\n" % e)
```


### Parameters
This endpoint does not need any parameter.

### Return type

**{str: (bool, date, datetime, dict, float, int, list, str, none_type)}**

### Authorization

[api_key](../README.md#api_key), [api_key_query](../README.md#api_key_query)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **both_keys**
> {str: (bool, date, datetime, dict, float, int, list, str, none_type)} both_keys()

Use both API keys

Use both API keys

### Example

* Api Key Authentication (api_key):
* Api Key Authentication (api_key_query):

```python
import time
import x_auth_id_alias
from x_auth_id_alias.api import usage_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = x_auth_id_alias.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure API key authorization: api_key
configuration.api_key['api_key'] = 'YOUR_API_KEY'

# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
# configuration.api_key_prefix['api_key'] = 'Bearer'

# Configure API key authorization: api_key_query
configuration.api_key['api_key_query'] = 'YOUR_API_KEY'

# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
# configuration.api_key_prefix['api_key_query'] = 'Bearer'

# Enter a context with an instance of the API client
with x_auth_id_alias.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = usage_api.UsageApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        # Use both API keys
        api_response = api_instance.both_keys()
        pprint(api_response)
    except x_auth_id_alias.ApiException as e:
        print("Exception when calling UsageApi->both_keys: %s\n" % e)
```


### Parameters
This endpoint does not need any parameter.

### Return type

**{str: (bool, date, datetime, dict, float, int, list, str, none_type)}**

### Authorization

[api_key](../README.md#api_key), [api_key_query](../README.md#api_key_query)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **key_in_header**
> {str: (bool, date, datetime, dict, float, int, list, str, none_type)} key_in_header()

Use API key in header

Use API key in header

### Example

* Api Key Authentication (api_key):

```python
import time
import x_auth_id_alias
from x_auth_id_alias.api import usage_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = x_auth_id_alias.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure API key authorization: api_key
configuration.api_key['api_key'] = 'YOUR_API_KEY'

# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
# configuration.api_key_prefix['api_key'] = 'Bearer'

# Enter a context with an instance of the API client
with x_auth_id_alias.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = usage_api.UsageApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        # Use API key in header
        api_response = api_instance.key_in_header()
        pprint(api_response)
    except x_auth_id_alias.ApiException as e:
        print("Exception when calling UsageApi->key_in_header: %s\n" % e)
```


### Parameters
This endpoint does not need any parameter.

### Return type

**{str: (bool, date, datetime, dict, float, int, list, str, none_type)}**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **key_in_query**
> {str: (bool, date, datetime, dict, float, int, list, str, none_type)} key_in_query()

Use API key in query

Use API key in query

### Example

* Api Key Authentication (api_key_query):

```python
import time
import x_auth_id_alias
from x_auth_id_alias.api import usage_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = x_auth_id_alias.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure API key authorization: api_key_query
configuration.api_key['api_key_query'] = 'YOUR_API_KEY'

# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
# configuration.api_key_prefix['api_key_query'] = 'Bearer'

# Enter a context with an instance of the API client
with x_auth_id_alias.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = usage_api.UsageApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        # Use API key in query
        api_response = api_instance.key_in_query()
        pprint(api_response)
    except x_auth_id_alias.ApiException as e:
        print("Exception when calling UsageApi->key_in_query: %s\n" % e)
```


### Parameters
This endpoint does not need any parameter.

### Return type

**{str: (bool, date, datetime, dict, float, int, list, str, none_type)}**

### Authorization

[api_key_query](../README.md#api_key_query)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

