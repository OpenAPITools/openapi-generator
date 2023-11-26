# echo_client_disallow_additional_properties_if_not_present.QueryApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_enum_ref_string**](QueryApi.md#test_enum_ref_string) | **GET** /query/enum_ref_string | Test query parameter(s)
[**test_query_datetime_date_string**](QueryApi.md#test_query_datetime_date_string) | **GET** /query/datetime/date/string | Test query parameter(s)
[**test_query_integer_boolean_string**](QueryApi.md#test_query_integer_boolean_string) | **GET** /query/integer/boolean/string | Test query parameter(s)
[**test_query_style_deep_object_explode_true_object**](QueryApi.md#test_query_style_deep_object_explode_true_object) | **GET** /query/style_deepObject/explode_true/object | Test query parameter(s)
[**test_query_style_deep_object_explode_true_object_all_of**](QueryApi.md#test_query_style_deep_object_explode_true_object_all_of) | **GET** /query/style_deepObject/explode_true/object/allOf | Test query parameter(s)
[**test_query_style_form_explode_true_array_string**](QueryApi.md#test_query_style_form_explode_true_array_string) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s)
[**test_query_style_form_explode_true_object**](QueryApi.md#test_query_style_form_explode_true_object) | **GET** /query/style_form/explode_true/object | Test query parameter(s)
[**test_query_style_form_explode_true_object_all_of**](QueryApi.md#test_query_style_form_explode_true_object_all_of) | **GET** /query/style_form/explode_true/object/allOf | Test query parameter(s)


# **test_enum_ref_string**
> str test_enum_ref_string(enum_nonref_string_query=enum_nonref_string_query, enum_ref_string_query=enum_ref_string_query)

Test query parameter(s)

Test query parameter(s)

### Example


```python
import time
import os
import echo_client_disallow_additional_properties_if_not_present
from echo_client_disallow_additional_properties_if_not_present.models.string_enum_ref import StringEnumRef
from echo_client_disallow_additional_properties_if_not_present.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost:3000
# See configuration.py for a list of all supported configuration parameters.
configuration = echo_client_disallow_additional_properties_if_not_present.Configuration(
    host = "http://localhost:3000"
)


# Enter a context with an instance of the API client
with echo_client_disallow_additional_properties_if_not_present.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = echo_client_disallow_additional_properties_if_not_present.QueryApi(api_client)
    enum_nonref_string_query = 'enum_nonref_string_query_example' # str |  (optional)
    enum_ref_string_query = echo_client_disallow_additional_properties_if_not_present.StringEnumRef() # StringEnumRef |  (optional)

    try:
        # Test query parameter(s)
        api_response = api_instance.test_enum_ref_string(enum_nonref_string_query=enum_nonref_string_query, enum_ref_string_query=enum_ref_string_query)
        print("The response of QueryApi->test_enum_ref_string:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling QueryApi->test_enum_ref_string: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_nonref_string_query** | **str**|  | [optional] 
 **enum_ref_string_query** | [**StringEnumRef**](.md)|  | [optional] 

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

# **test_query_datetime_date_string**
> str test_query_datetime_date_string(datetime_query=datetime_query, date_query=date_query, string_query=string_query)

Test query parameter(s)

Test query parameter(s)

### Example


```python
import time
import os
import echo_client_disallow_additional_properties_if_not_present
from echo_client_disallow_additional_properties_if_not_present.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost:3000
# See configuration.py for a list of all supported configuration parameters.
configuration = echo_client_disallow_additional_properties_if_not_present.Configuration(
    host = "http://localhost:3000"
)


# Enter a context with an instance of the API client
with echo_client_disallow_additional_properties_if_not_present.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = echo_client_disallow_additional_properties_if_not_present.QueryApi(api_client)
    datetime_query = '2013-10-20T19:20:30+01:00' # datetime |  (optional)
    date_query = '2013-10-20' # date |  (optional)
    string_query = 'string_query_example' # str |  (optional)

    try:
        # Test query parameter(s)
        api_response = api_instance.test_query_datetime_date_string(datetime_query=datetime_query, date_query=date_query, string_query=string_query)
        print("The response of QueryApi->test_query_datetime_date_string:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling QueryApi->test_query_datetime_date_string: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **datetime_query** | **datetime**|  | [optional] 
 **date_query** | **date**|  | [optional] 
 **string_query** | **str**|  | [optional] 

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

# **test_query_integer_boolean_string**
> str test_query_integer_boolean_string(integer_query=integer_query, boolean_query=boolean_query, string_query=string_query)

Test query parameter(s)

Test query parameter(s)

### Example


```python
import time
import os
import echo_client_disallow_additional_properties_if_not_present
from echo_client_disallow_additional_properties_if_not_present.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost:3000
# See configuration.py for a list of all supported configuration parameters.
configuration = echo_client_disallow_additional_properties_if_not_present.Configuration(
    host = "http://localhost:3000"
)


# Enter a context with an instance of the API client
with echo_client_disallow_additional_properties_if_not_present.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = echo_client_disallow_additional_properties_if_not_present.QueryApi(api_client)
    integer_query = 56 # int |  (optional)
    boolean_query = True # bool |  (optional)
    string_query = 'string_query_example' # str |  (optional)

    try:
        # Test query parameter(s)
        api_response = api_instance.test_query_integer_boolean_string(integer_query=integer_query, boolean_query=boolean_query, string_query=string_query)
        print("The response of QueryApi->test_query_integer_boolean_string:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling QueryApi->test_query_integer_boolean_string: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **integer_query** | **int**|  | [optional] 
 **boolean_query** | **bool**|  | [optional] 
 **string_query** | **str**|  | [optional] 

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

# **test_query_style_deep_object_explode_true_object**
> str test_query_style_deep_object_explode_true_object(query_object=query_object)

Test query parameter(s)

Test query parameter(s)

### Example


```python
import time
import os
import echo_client_disallow_additional_properties_if_not_present
from echo_client_disallow_additional_properties_if_not_present.models.pet import Pet
from echo_client_disallow_additional_properties_if_not_present.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost:3000
# See configuration.py for a list of all supported configuration parameters.
configuration = echo_client_disallow_additional_properties_if_not_present.Configuration(
    host = "http://localhost:3000"
)


# Enter a context with an instance of the API client
with echo_client_disallow_additional_properties_if_not_present.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = echo_client_disallow_additional_properties_if_not_present.QueryApi(api_client)
    query_object = echo_client_disallow_additional_properties_if_not_present.Pet() # Pet |  (optional)

    try:
        # Test query parameter(s)
        api_response = api_instance.test_query_style_deep_object_explode_true_object(query_object=query_object)
        print("The response of QueryApi->test_query_style_deep_object_explode_true_object:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling QueryApi->test_query_style_deep_object_explode_true_object: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query_object** | [**Pet**](.md)|  | [optional] 

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

# **test_query_style_deep_object_explode_true_object_all_of**
> str test_query_style_deep_object_explode_true_object_all_of(query_object=query_object)

Test query parameter(s)

Test query parameter(s)

### Example


```python
import time
import os
import echo_client_disallow_additional_properties_if_not_present
from echo_client_disallow_additional_properties_if_not_present.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost:3000
# See configuration.py for a list of all supported configuration parameters.
configuration = echo_client_disallow_additional_properties_if_not_present.Configuration(
    host = "http://localhost:3000"
)


# Enter a context with an instance of the API client
with echo_client_disallow_additional_properties_if_not_present.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = echo_client_disallow_additional_properties_if_not_present.QueryApi(api_client)
    query_object = echo_client_disallow_additional_properties_if_not_present.TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter() # TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter |  (optional)

    try:
        # Test query parameter(s)
        api_response = api_instance.test_query_style_deep_object_explode_true_object_all_of(query_object=query_object)
        print("The response of QueryApi->test_query_style_deep_object_explode_true_object_all_of:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling QueryApi->test_query_style_deep_object_explode_true_object_all_of: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query_object** | [**TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter**](.md)|  | [optional] 

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

# **test_query_style_form_explode_true_array_string**
> str test_query_style_form_explode_true_array_string(query_object=query_object)

Test query parameter(s)

Test query parameter(s)

### Example


```python
import time
import os
import echo_client_disallow_additional_properties_if_not_present
from echo_client_disallow_additional_properties_if_not_present.models.test_query_style_form_explode_true_array_string_query_object_parameter import TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter
from echo_client_disallow_additional_properties_if_not_present.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost:3000
# See configuration.py for a list of all supported configuration parameters.
configuration = echo_client_disallow_additional_properties_if_not_present.Configuration(
    host = "http://localhost:3000"
)


# Enter a context with an instance of the API client
with echo_client_disallow_additional_properties_if_not_present.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = echo_client_disallow_additional_properties_if_not_present.QueryApi(api_client)
    query_object = echo_client_disallow_additional_properties_if_not_present.TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter() # TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter |  (optional)

    try:
        # Test query parameter(s)
        api_response = api_instance.test_query_style_form_explode_true_array_string(query_object=query_object)
        print("The response of QueryApi->test_query_style_form_explode_true_array_string:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling QueryApi->test_query_style_form_explode_true_array_string: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query_object** | [**TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter**](.md)|  | [optional] 

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

# **test_query_style_form_explode_true_object**
> str test_query_style_form_explode_true_object(query_object=query_object)

Test query parameter(s)

Test query parameter(s)

### Example


```python
import time
import os
import echo_client_disallow_additional_properties_if_not_present
from echo_client_disallow_additional_properties_if_not_present.models.pet import Pet
from echo_client_disallow_additional_properties_if_not_present.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost:3000
# See configuration.py for a list of all supported configuration parameters.
configuration = echo_client_disallow_additional_properties_if_not_present.Configuration(
    host = "http://localhost:3000"
)


# Enter a context with an instance of the API client
with echo_client_disallow_additional_properties_if_not_present.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = echo_client_disallow_additional_properties_if_not_present.QueryApi(api_client)
    query_object = echo_client_disallow_additional_properties_if_not_present.Pet() # Pet |  (optional)

    try:
        # Test query parameter(s)
        api_response = api_instance.test_query_style_form_explode_true_object(query_object=query_object)
        print("The response of QueryApi->test_query_style_form_explode_true_object:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling QueryApi->test_query_style_form_explode_true_object: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query_object** | [**Pet**](.md)|  | [optional] 

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

# **test_query_style_form_explode_true_object_all_of**
> str test_query_style_form_explode_true_object_all_of(query_object=query_object)

Test query parameter(s)

Test query parameter(s)

### Example


```python
import time
import os
import echo_client_disallow_additional_properties_if_not_present
from echo_client_disallow_additional_properties_if_not_present.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost:3000
# See configuration.py for a list of all supported configuration parameters.
configuration = echo_client_disallow_additional_properties_if_not_present.Configuration(
    host = "http://localhost:3000"
)


# Enter a context with an instance of the API client
with echo_client_disallow_additional_properties_if_not_present.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = echo_client_disallow_additional_properties_if_not_present.QueryApi(api_client)
    query_object = echo_client_disallow_additional_properties_if_not_present.DataQuery() # DataQuery |  (optional)

    try:
        # Test query parameter(s)
        api_response = api_instance.test_query_style_form_explode_true_object_all_of(query_object=query_object)
        print("The response of QueryApi->test_query_style_form_explode_true_object_all_of:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling QueryApi->test_query_style_form_explode_true_object_all_of: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query_object** | [**DataQuery**](.md)|  | [optional] 

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

