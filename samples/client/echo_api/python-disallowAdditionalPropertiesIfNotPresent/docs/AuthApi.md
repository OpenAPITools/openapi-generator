# openapi_client.AuthApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_auth_http_basic**](AuthApi.md#test_auth_http_basic) | **POST** /auth/http/basic | To test HTTP basic authentication
[**test_auth_http_bearer**](AuthApi.md#test_auth_http_bearer) | **POST** /auth/http/bearer | To test HTTP bearer authentication


# **test_auth_http_basic**
> str test_auth_http_basic()

To test HTTP basic authentication

To test HTTP basic authentication

### Example

* Basic Authentication (http_auth):

```python
import openapi_client
from openapi_client.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost:3000
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "http://localhost:3000"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure HTTP basic authorization: http_auth
configuration = openapi_client.Configuration(
    username = os.environ["USERNAME"],
    password = os.environ["PASSWORD"]
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = openapi_client.AuthApi(api_client)

    try:
        # To test HTTP basic authentication
        api_response = api_instance.test_auth_http_basic()
        print("The response of AuthApi->test_auth_http_basic:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling AuthApi->test_auth_http_basic: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

**str**

### Authorization

[http_auth](../README.md#http_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_auth_http_bearer**
> str test_auth_http_bearer()

To test HTTP bearer authentication

To test HTTP bearer authentication

### Example

* Bearer Authentication (http_bearer_auth):

```python
import openapi_client
from openapi_client.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost:3000
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "http://localhost:3000"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure Bearer authorization: http_bearer_auth
configuration = openapi_client.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = openapi_client.AuthApi(api_client)

    try:
        # To test HTTP bearer authentication
        api_response = api_instance.test_auth_http_bearer()
        print("The response of AuthApi->test_auth_http_bearer:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling AuthApi->test_auth_http_bearer: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

**str**

### Authorization

[http_bearer_auth](../README.md#http_bearer_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

