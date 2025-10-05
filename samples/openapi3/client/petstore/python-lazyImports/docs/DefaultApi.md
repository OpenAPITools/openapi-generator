# petstore_api.DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**foo_get**](DefaultApi.md#foo_get) | **GET** /foo | 


# **foo_get**
> FooGetDefaultResponse foo_get()

### Example


```python
import petstore_api
from petstore_api.models.foo_get_default_response import FooGetDefaultResponse
from petstore_api.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.DefaultApi(api_client)

    try:
        api_response = api_instance.foo_get()
        print("The response of DefaultApi->foo_get:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->foo_get: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

[**FooGetDefaultResponse**](FooGetDefaultResponse.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**0** | response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

