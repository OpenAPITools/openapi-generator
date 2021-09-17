# petstore_api.FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**create_xml_item**](FakeApi.md#create_xml_item) | **POST** /fake/create_xml_item | creates an XmlItem
[**fake_outer_boolean_serialize**](FakeApi.md#fake_outer_boolean_serialize) | **POST** /fake/outer/boolean | 
[**fake_outer_composite_serialize**](FakeApi.md#fake_outer_composite_serialize) | **POST** /fake/outer/composite | 
[**fake_outer_number_serialize**](FakeApi.md#fake_outer_number_serialize) | **POST** /fake/outer/number | 
[**fake_outer_string_serialize**](FakeApi.md#fake_outer_string_serialize) | **POST** /fake/outer/string | 
[**test_body_with_file_schema**](FakeApi.md#test_body_with_file_schema) | **PUT** /fake/body-with-file-schema | 
[**test_body_with_query_params**](FakeApi.md#test_body_with_query_params) | **PUT** /fake/body-with-query-params | 
[**test_client_model**](FakeApi.md#test_client_model) | **PATCH** /fake | To test \&quot;client\&quot; model
[**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
[**test_enum_parameters**](FakeApi.md#test_enum_parameters) | **GET** /fake | To test enum parameters
[**test_group_parameters**](FakeApi.md#test_group_parameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**test_inline_additional_properties**](FakeApi.md#test_inline_additional_properties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**test_json_form_data**](FakeApi.md#test_json_form_data) | **GET** /fake/jsonFormData | test json serialization of form data
[**test_query_parameter_collection_format**](FakeApi.md#test_query_parameter_collection_format) | **PUT** /fake/test-query-parameters | 


# **create_xml_item**
> create_xml_item(xml_item)

creates an XmlItem

this route creates an XmlItem

### Example

```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeApi(api_client)
    xml_item = petstore_api.XmlItem() # XmlItem | XmlItem Body

    try:
        # creates an XmlItem
        api_instance.create_xml_item(xml_item)
    except ApiException as e:
        print("Exception when calling FakeApi->create_xml_item: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xml_item** | [**XmlItem**](XmlItem.md)| XmlItem Body | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml, application/xml; charset=utf-8, application/xml; charset=utf-16, text/xml, text/xml; charset=utf-8, text/xml; charset=utf-16
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_boolean_serialize**
> bool fake_outer_boolean_serialize(body=body)



Test serialization of outer boolean types

### Example

```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeApi(api_client)
    body = True # bool | Input boolean as post body (optional)

    try:
        api_response = api_instance.fake_outer_boolean_serialize(body=body)
        pprint(api_response)
    except ApiException as e:
        print("Exception when calling FakeApi->fake_outer_boolean_serialize: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **bool**| Input boolean as post body | [optional] 

### Return type

**bool**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output boolean |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_composite_serialize**
> OuterComposite fake_outer_composite_serialize(body=body)



Test serialization of object with outer number type

### Example

```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeApi(api_client)
    body = petstore_api.OuterComposite() # OuterComposite | Input composite as post body (optional)

    try:
        api_response = api_instance.fake_outer_composite_serialize(body=body)
        pprint(api_response)
    except ApiException as e:
        print("Exception when calling FakeApi->fake_outer_composite_serialize: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional] 

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output composite |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_number_serialize**
> float fake_outer_number_serialize(body=body)



Test serialization of outer number types

### Example

```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeApi(api_client)
    body = 3.4 # float | Input number as post body (optional)

    try:
        api_response = api_instance.fake_outer_number_serialize(body=body)
        pprint(api_response)
    except ApiException as e:
        print("Exception when calling FakeApi->fake_outer_number_serialize: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **float**| Input number as post body | [optional] 

### Return type

**float**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output number |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_string_serialize**
> str fake_outer_string_serialize(body=body)



Test serialization of outer string types

### Example

```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeApi(api_client)
    body = 'body_example' # str | Input string as post body (optional)

    try:
        api_response = api_instance.fake_outer_string_serialize(body=body)
        pprint(api_response)
    except ApiException as e:
        print("Exception when calling FakeApi->fake_outer_string_serialize: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **str**| Input string as post body | [optional] 

### Return type

**str**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output string |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_body_with_file_schema**
> test_body_with_file_schema(body)



For this test, the body for this request much reference a schema named `File`.

### Example

```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeApi(api_client)
    body = petstore_api.FileSchemaTestClass() # FileSchemaTestClass | 

    try:
        api_instance.test_body_with_file_schema(body)
    except ApiException as e:
        print("Exception when calling FakeApi->test_body_with_file_schema: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_body_with_query_params**
> test_body_with_query_params(query, body)



### Example

```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeApi(api_client)
    query = 'query_example' # str | 
body = petstore_api.User() # User | 

    try:
        api_instance.test_body_with_query_params(query, body)
    except ApiException as e:
        print("Exception when calling FakeApi->test_body_with_query_params: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **str**|  | 
 **body** | [**User**](User.md)|  | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_client_model**
> Client test_client_model(body)

To test \"client\" model

To test \"client\" model

### Example

```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeApi(api_client)
    body = petstore_api.Client() # Client | client model

    try:
        # To test \"client\" model
        api_response = api_instance.test_client_model(body)
        pprint(api_response)
    except ApiException as e:
        print("Exception when calling FakeApi->test_client_model: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md)| client model | 

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_endpoint_parameters**
> test_endpoint_parameters(number, double, pattern_without_delimiter, byte, integer=integer, int32=int32, int64=int64, float=float, string=string, binary=binary, date=date, date_time=date_time, password=password, param_callback=param_callback)

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

### Example

* Basic Authentication (http_basic_test):
```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# The client must configure the authentication and authorization parameters
# in accordance with the API server security policy.
# Examples for each auth method are provided below, use the example that
# satisfies your auth use case.

# Configure HTTP basic authorization: http_basic_test
configuration = petstore_api.Configuration(
    username = 'YOUR_USERNAME',
    password = 'YOUR_PASSWORD'
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeApi(api_client)
    number = 3.4 # float | None
double = 3.4 # float | None
pattern_without_delimiter = 'pattern_without_delimiter_example' # str | None
byte = 'byte_example' # str | None
integer = 56 # int | None (optional)
int32 = 56 # int | None (optional)
int64 = 56 # int | None (optional)
float = 3.4 # float | None (optional)
string = 'string_example' # str | None (optional)
binary = '/path/to/file' # file | None (optional)
date = '2013-10-20' # date | None (optional)
date_time = '2013-10-20T19:20:30+01:00' # datetime | None (optional)
password = 'password_example' # str | None (optional)
param_callback = 'param_callback_example' # str | None (optional)

    try:
        # Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
        api_instance.test_endpoint_parameters(number, double, pattern_without_delimiter, byte, integer=integer, int32=int32, int64=int64, float=float, string=string, binary=binary, date=date, date_time=date_time, password=password, param_callback=param_callback)
    except ApiException as e:
        print("Exception when calling FakeApi->test_endpoint_parameters: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **float**| None | 
 **double** | **float**| None | 
 **pattern_without_delimiter** | **str**| None | 
 **byte** | **str**| None | 
 **integer** | **int**| None | [optional] 
 **int32** | **int**| None | [optional] 
 **int64** | **int**| None | [optional] 
 **float** | **float**| None | [optional] 
 **string** | **str**| None | [optional] 
 **binary** | **file**| None | [optional] 
 **date** | **date**| None | [optional] 
 **date_time** | **datetime**| None | [optional] 
 **password** | **str**| None | [optional] 
 **param_callback** | **str**| None | [optional] 

### Return type

void (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**400** | Invalid username supplied |  -  |
**404** | User not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_enum_parameters**
> test_enum_parameters(enum_header_string_array=enum_header_string_array, enum_header_string=enum_header_string, enum_query_string_array=enum_query_string_array, enum_query_string=enum_query_string, enum_query_integer=enum_query_integer, enum_query_double=enum_query_double, enum_form_string_array=enum_form_string_array, enum_form_string=enum_form_string)

To test enum parameters

To test enum parameters

### Example

```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeApi(api_client)
    enum_header_string_array = ['enum_header_string_array_example'] # list[str] | Header parameter enum test (string array) (optional)
enum_header_string = '-efg' # str | Header parameter enum test (string) (optional) (default to '-efg')
enum_query_string_array = ['enum_query_string_array_example'] # list[str] | Query parameter enum test (string array) (optional)
enum_query_string = '-efg' # str | Query parameter enum test (string) (optional) (default to '-efg')
enum_query_integer = 56 # int | Query parameter enum test (double) (optional)
enum_query_double = 3.4 # float | Query parameter enum test (double) (optional)
enum_form_string_array = '$' # list[str] | Form parameter enum test (string array) (optional) (default to '$')
enum_form_string = '-efg' # str | Form parameter enum test (string) (optional) (default to '-efg')

    try:
        # To test enum parameters
        api_instance.test_enum_parameters(enum_header_string_array=enum_header_string_array, enum_header_string=enum_header_string, enum_query_string_array=enum_query_string_array, enum_query_string=enum_query_string, enum_query_integer=enum_query_integer, enum_query_double=enum_query_double, enum_form_string_array=enum_form_string_array, enum_form_string=enum_form_string)
    except ApiException as e:
        print("Exception when calling FakeApi->test_enum_parameters: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_header_string_array** | [**list[str]**](str.md)| Header parameter enum test (string array) | [optional] 
 **enum_header_string** | **str**| Header parameter enum test (string) | [optional] [default to &#39;-efg&#39;]
 **enum_query_string_array** | [**list[str]**](str.md)| Query parameter enum test (string array) | [optional] 
 **enum_query_string** | **str**| Query parameter enum test (string) | [optional] [default to &#39;-efg&#39;]
 **enum_query_integer** | **int**| Query parameter enum test (double) | [optional] 
 **enum_query_double** | **float**| Query parameter enum test (double) | [optional] 
 **enum_form_string_array** | [**list[str]**](str.md)| Form parameter enum test (string array) | [optional] [default to &#39;$&#39;]
 **enum_form_string** | **str**| Form parameter enum test (string) | [optional] [default to &#39;-efg&#39;]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**400** | Invalid request |  -  |
**404** | Not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_group_parameters**
> test_group_parameters(required_string_group, required_boolean_group, required_int64_group, string_group=string_group, boolean_group=boolean_group, int64_group=int64_group)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example

```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeApi(api_client)
    required_string_group = 56 # int | Required String in group parameters
required_boolean_group = True # bool | Required Boolean in group parameters
required_int64_group = 56 # int | Required Integer in group parameters
string_group = 56 # int | String in group parameters (optional)
boolean_group = True # bool | Boolean in group parameters (optional)
int64_group = 56 # int | Integer in group parameters (optional)

    try:
        # Fake endpoint to test group parameters (optional)
        api_instance.test_group_parameters(required_string_group, required_boolean_group, required_int64_group, string_group=string_group, boolean_group=boolean_group, int64_group=int64_group)
    except ApiException as e:
        print("Exception when calling FakeApi->test_group_parameters: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **required_string_group** | **int**| Required String in group parameters | 
 **required_boolean_group** | **bool**| Required Boolean in group parameters | 
 **required_int64_group** | **int**| Required Integer in group parameters | 
 **string_group** | **int**| String in group parameters | [optional] 
 **boolean_group** | **bool**| Boolean in group parameters | [optional] 
 **int64_group** | **int**| Integer in group parameters | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**400** | Someting wrong |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_inline_additional_properties**
> test_inline_additional_properties(param)

test inline additionalProperties

### Example

```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeApi(api_client)
    param = {'key': 'param_example'} # dict(str, str) | request body

    try:
        # test inline additionalProperties
        api_instance.test_inline_additional_properties(param)
    except ApiException as e:
        print("Exception when calling FakeApi->test_inline_additional_properties: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | [**dict(str, str)**](str.md)| request body | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_json_form_data**
> test_json_form_data(param, param2)

test json serialization of form data

### Example

```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeApi(api_client)
    param = 'param_example' # str | field1
param2 = 'param2_example' # str | field2

    try:
        # test json serialization of form data
        api_instance.test_json_form_data(param, param2)
    except ApiException as e:
        print("Exception when calling FakeApi->test_json_form_data: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **str**| field1 | 
 **param2** | **str**| field2 | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_query_parameter_collection_format**
> test_query_parameter_collection_format(pipe, ioutil, http, url, context)



To test the collection format in query parameters

### Example

```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeApi(api_client)
    pipe = ['pipe_example'] # list[str] | 
ioutil = ['ioutil_example'] # list[str] | 
http = ['http_example'] # list[str] | 
url = ['url_example'] # list[str] | 
context = ['context_example'] # list[str] | 

    try:
        api_instance.test_query_parameter_collection_format(pipe, ioutil, http, url, context)
    except ApiException as e:
        print("Exception when calling FakeApi->test_query_parameter_collection_format: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**list[str]**](str.md)|  | 
 **ioutil** | [**list[str]**](str.md)|  | 
 **http** | [**list[str]**](str.md)|  | 
 **url** | [**list[str]**](str.md)|  | 
 **context** | [**list[str]**](str.md)|  | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

