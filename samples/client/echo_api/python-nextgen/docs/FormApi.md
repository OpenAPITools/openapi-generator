# openapi_client.FormApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_form_integer_boolean_string**](FormApi.md#test_form_integer_boolean_string) | **POST** /form/integer/boolean/string | Test form parameter(s)


# **test_form_integer_boolean_string**
> str test_form_integer_boolean_string(integer_form=integer_form, boolean_form=boolean_form, string_form=string_form)

Test form parameter(s)

Test form parameter(s)

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
    api_instance = openapi_client.FormApi(api_client)
    integer_form = 56 # int |  (optional)
    boolean_form = True # bool |  (optional)
    string_form = 'string_form_example' # str |  (optional)

    try:
        # Test form parameter(s)
        api_response = api_instance.test_form_integer_boolean_string(integer_form=integer_form, boolean_form=boolean_form, string_form=string_form)
        print("The response of FormApi->test_form_integer_boolean_string:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling FormApi->test_form_integer_boolean_string: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **integer_form** | **int**|  | [optional] 
 **boolean_form** | **bool**|  | [optional] 
 **string_form** | **str**|  | [optional] 

### Return type

**str**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

