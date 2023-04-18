# openapi_client.HeaderApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_header_integer_boolean_string**](HeaderApi.md#test_header_integer_boolean_string) | **GET** /header/integer/boolean/string | Test header parameter(s)


# **test_header_integer_boolean_string**
> str test_header_integer_boolean_string(integer_header=integer_header, boolean_header=boolean_header, string_header=string_header)

Test header parameter(s)

Test header parameter(s)

### Example

```python
import time
import os
import openapi_client
from openapi_client.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost:3000
# See configuration.py for a list of all supported configuration parameters.
configuration = openapi_client.Configuration(
    host = "http://localhost:3000"
)


# Enter a context with an instance of the API client
with openapi_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = openapi_client.HeaderApi(api_client)
    integer_header = 56 # int |  (optional)
    boolean_header = True # bool |  (optional)
    string_header = 'string_header_example' # str |  (optional)

    try:
        # Test header parameter(s)
        api_response = api_instance.test_header_integer_boolean_string(integer_header=integer_header, boolean_header=boolean_header, string_header=string_header)
        print("The response of HeaderApi->test_header_integer_boolean_string:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling HeaderApi->test_header_integer_boolean_string: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **integer_header** | **int**|  | [optional] 
 **boolean_header** | **bool**|  | [optional] 
 **string_header** | **str**|  | [optional] 

### Return type

**str**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

