# openapi_client.FormApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_form_integer_boolean_string**](FormApi.md#test_form_integer_boolean_string) | **POST** /form/integer/boolean/string | Test form parameter(s)
[**test_form_oneof**](FormApi.md#test_form_oneof) | **POST** /form/oneof | Test form parameter(s) for oneOf schema


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

# **test_form_oneof**
> str test_form_oneof(form1=form1, form2=form2, form3=form3, form4=form4, id=id, name=name)

Test form parameter(s) for oneOf schema

Test form parameter(s) for oneOf schema

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
    form1 = 'form1_example' # str |  (optional)
    form2 = 56 # int |  (optional)
    form3 = 'form3_example' # str |  (optional)
    form4 = True # bool |  (optional)
    id = 56 # int |  (optional)
    name = 'name_example' # str |  (optional)

    try:
        # Test form parameter(s) for oneOf schema
        api_response = api_instance.test_form_oneof(form1=form1, form2=form2, form3=form3, form4=form4, id=id, name=name)
        print("The response of FormApi->test_form_oneof:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling FormApi->test_form_oneof: %s\n" % e)
```



### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **form1** | **str**|  | [optional] 
 **form2** | **int**|  | [optional] 
 **form3** | **str**|  | [optional] 
 **form4** | **bool**|  | [optional] 
 **id** | **int**|  | [optional] 
 **name** | **str**|  | [optional] 

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

