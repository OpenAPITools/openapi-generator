# petstore_api.FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**array_model**](FakeApi.md#array_model) | **POST** /fake/refs/arraymodel | 
[**boolean**](FakeApi.md#boolean) | **POST** /fake/refs/boolean | 
[**create_xml_item**](FakeApi.md#create_xml_item) | **POST** /fake/create_xml_item | creates an XmlItem
[**number_with_validations**](FakeApi.md#number_with_validations) | **POST** /fake/refs/number | 
[**object_model_with_ref_props**](FakeApi.md#object_model_with_ref_props) | **POST** /fake/refs/object_model_with_ref_props | 
[**string**](FakeApi.md#string) | **POST** /fake/refs/string | 
[**string_enum**](FakeApi.md#string_enum) | **POST** /fake/refs/enum | 
[**test_body_with_file_schema**](FakeApi.md#test_body_with_file_schema) | **PUT** /fake/body-with-file-schema | 
[**test_body_with_query_params**](FakeApi.md#test_body_with_query_params) | **PUT** /fake/body-with-query-params | 
[**test_client_model**](FakeApi.md#test_client_model) | **PATCH** /fake | To test \&quot;client\&quot; model
[**test_endpoint_enums_length_one**](FakeApi.md#test_endpoint_enums_length_one) | **PUT** /fake/enums-of-length-one/{path_string}/{path_integer} | 
[**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
[**test_enum_parameters**](FakeApi.md#test_enum_parameters) | **GET** /fake | To test enum parameters
[**test_group_parameters**](FakeApi.md#test_group_parameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**test_inline_additional_properties**](FakeApi.md#test_inline_additional_properties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**test_json_form_data**](FakeApi.md#test_json_form_data) | **GET** /fake/jsonFormData | test json serialization of form data


# **array_model**
> AnimalFarm array_model()



Test serialization of ArrayModel

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.animal_farm import AnimalFarm
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)
    body = AnimalFarm([
        Animal(),
    ]) # AnimalFarm | Input model (optional)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        api_response = api_instance.array_model(body=body)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->array_model: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**AnimalFarm**](AnimalFarm.md)| Input model | [optional]

### Return type

[**AnimalFarm**](AnimalFarm.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output model |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **boolean**
> bool boolean()



Test serialization of outer boolean types

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)
    body = True # bool | Input boolean as post body (optional)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        api_response = api_instance.boolean(body=body)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->boolean: %s\n" % e)
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

# **create_xml_item**
> create_xml_item(xml_item)

creates an XmlItem

this route creates an XmlItem

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.xml_item import XmlItem
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)
    xml_item = XmlItem(
        attribute_string="string",
        attribute_number=1.234,
        attribute_integer=-2,
        attribute_boolean=True,
        wrapped_array=[
            1,
        ],
        name_string="string",
        name_number=1.234,
        name_integer=-2,
        name_boolean=True,
        name_array=[
            1,
        ],
        name_wrapped_array=[
            1,
        ],
        prefix_string="string",
        prefix_number=1.234,
        prefix_integer=-2,
        prefix_boolean=True,
        prefix_array=[
            1,
        ],
        prefix_wrapped_array=[
            1,
        ],
        namespace_string="string",
        namespace_number=1.234,
        namespace_integer=-2,
        namespace_boolean=True,
        namespace_array=[
            1,
        ],
        namespace_wrapped_array=[
            1,
        ],
        prefix_ns_string="string",
        prefix_ns_number=1.234,
        prefix_ns_integer=-2,
        prefix_ns_boolean=True,
        prefix_ns_array=[
            1,
        ],
        prefix_ns_wrapped_array=[
            1,
        ],
    ) # XmlItem | XmlItem Body

    # example passing only required values which don't have defaults set
    try:
        # creates an XmlItem
        api_instance.create_xml_item(xml_item)
    except petstore_api.ApiException as e:
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

# **number_with_validations**
> NumberWithValidations number_with_validations()



Test serialization of outer number types

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.number_with_validations import NumberWithValidations
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)
    body = NumberWithValidations(1E+1) # NumberWithValidations | Input number as post body (optional)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        api_response = api_instance.number_with_validations(body=body)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->number_with_validations: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**NumberWithValidations**](NumberWithValidations.md)| Input number as post body | [optional]

### Return type

[**NumberWithValidations**](NumberWithValidations.md)

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

# **object_model_with_ref_props**
> ObjectModelWithRefProps object_model_with_ref_props()



Test serialization of object with $refed properties

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.object_model_with_ref_props import ObjectModelWithRefProps
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)
    body = ObjectModelWithRefProps(
        my_number=NumberWithValidations(1E+1),
        my_string="my_string_example",
        my_boolean=True,
    ) # ObjectModelWithRefProps | Input model (optional)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        api_response = api_instance.object_model_with_ref_props(body=body)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->object_model_with_ref_props: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**ObjectModelWithRefProps**](ObjectModelWithRefProps.md)| Input model | [optional]

### Return type

[**ObjectModelWithRefProps**](ObjectModelWithRefProps.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output model |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **string**
> str string()



Test serialization of outer string types

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)
    body = "body_example" # str | Input string as post body (optional)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        api_response = api_instance.string(body=body)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->string: %s\n" % e)
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

# **string_enum**
> StringEnum string_enum()



Test serialization of outer enum

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.string_enum import StringEnum
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)
    body = StringEnum("placed") # StringEnum | Input enum (optional)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        api_response = api_instance.string_enum(body=body)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->string_enum: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**StringEnum**](StringEnum.md)| Input enum | [optional]

### Return type

[**StringEnum**](StringEnum.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output enum |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_body_with_file_schema**
> test_body_with_file_schema(body)



For this test, the body for this request much reference a schema named `File`.

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.file_schema_test_class import FileSchemaTestClass
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)
    body = FileSchemaTestClass(
        file=File(
            source_uri="source_uri_example",
        ),
        files=[
            File(
                source_uri="source_uri_example",
            ),
        ],
    ) # FileSchemaTestClass | 

    # example passing only required values which don't have defaults set
    try:
        api_instance.test_body_with_file_schema(body)
    except petstore_api.ApiException as e:
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
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.user import User
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)
    query = "query_example" # str | 
    body = User(
        id=1,
        username="username_example",
        first_name="first_name_example",
        last_name="last_name_example",
        email="email_example",
        password="password_example",
        phone="phone_example",
        user_status=1,
    ) # User | 

    # example passing only required values which don't have defaults set
    try:
        api_instance.test_body_with_query_params(query, body)
    except petstore_api.ApiException as e:
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
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.client import Client
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)
    body = Client(
        client="client_example",
    ) # Client | client model

    # example passing only required values which don't have defaults set
    try:
        # To test \"client\" model
        api_response = api_instance.test_client_model(body)
        pprint(api_response)
    except petstore_api.ApiException as e:
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

# **test_endpoint_enums_length_one**
> test_endpoint_enums_length_one()



This route has required values with enums of 1

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only required values which don't have defaults set
    try:
        api_instance.test_endpoint_enums_length_one()
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->test_endpoint_enums_length_one: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query_integer** | **int**|  | defaults to 3
 **query_string** | **str**|  | defaults to "brillig"
 **path_string** | **str**|  | defaults to "hello"
 **path_integer** | **int**|  | defaults to 34
 **header_number** | **float**|  | defaults to 1.234

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

# **test_endpoint_parameters**
> test_endpoint_parameters(number, double, pattern_without_delimiter, byte)

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

### Example

* Basic Authentication (http_basic_test):

```python
import time
import petstore_api
from petstore_api.api import fake_api
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
    api_instance = fake_api.FakeApi(api_client)
    number = 32.1 # float | None
    double = 67.8 # float | None
    pattern_without_delimiter = "Aj" # str | None
    byte = 'YQ==' # str | None
    integer = 10 # int | None (optional)
    int32 = 20 # int | None (optional)
    int64 = 1 # int | None (optional)
    float = 3.14 # float | None (optional)
    string = "A" # str | None (optional)
    binary = open('/path/to/file', 'rb') # file_type | None (optional)
    date = dateutil_parser('1970-01-01').date() # date | None (optional)
    date_time = dateutil_parser('1970-01-01T00:00:00.00Z') # datetime | None (optional)
    password = "password_example" # str | None (optional)
    param_callback = "param_callback_example" # str | None (optional)

    # example passing only required values which don't have defaults set
    try:
        # Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
        api_instance.test_endpoint_parameters(number, double, pattern_without_delimiter, byte)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->test_endpoint_parameters: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
        api_instance.test_endpoint_parameters(number, double, pattern_without_delimiter, byte, integer=integer, int32=int32, int64=int64, float=float, string=string, binary=binary, date=date, date_time=date_time, password=password, param_callback=param_callback)
    except petstore_api.ApiException as e:
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
 **binary** | **file_type**| None | [optional]
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
> test_enum_parameters()

To test enum parameters

To test enum parameters

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)
    enum_header_string_array = [
        "$",
    ] # [str] | Header parameter enum test (string array) (optional)
    enum_header_string = "-efg" # str | Header parameter enum test (string) (optional) if omitted the server will use the default value of "-efg"
    enum_query_string_array = [
        "$",
    ] # [str] | Query parameter enum test (string array) (optional)
    enum_query_string = "-efg" # str | Query parameter enum test (string) (optional) if omitted the server will use the default value of "-efg"
    enum_query_integer = 1 # int | Query parameter enum test (double) (optional)
    enum_query_double = 1.1 # float | Query parameter enum test (double) (optional)
    enum_form_string_array = [
        "$",
    ] # [str] | Form parameter enum test (string array) (optional) if omitted the server will use the default value of "$"
    enum_form_string = "-efg" # str | Form parameter enum test (string) (optional) if omitted the server will use the default value of "-efg"

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # To test enum parameters
        api_instance.test_enum_parameters(enum_header_string_array=enum_header_string_array, enum_header_string=enum_header_string, enum_query_string_array=enum_query_string_array, enum_query_string=enum_query_string, enum_query_integer=enum_query_integer, enum_query_double=enum_query_double, enum_form_string_array=enum_form_string_array, enum_form_string=enum_form_string)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->test_enum_parameters: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_header_string_array** | **[str]**| Header parameter enum test (string array) | [optional]
 **enum_header_string** | **str**| Header parameter enum test (string) | [optional] if omitted the server will use the default value of "-efg"
 **enum_query_string_array** | **[str]**| Query parameter enum test (string array) | [optional]
 **enum_query_string** | **str**| Query parameter enum test (string) | [optional] if omitted the server will use the default value of "-efg"
 **enum_query_integer** | **int**| Query parameter enum test (double) | [optional]
 **enum_query_double** | **float**| Query parameter enum test (double) | [optional]
 **enum_form_string_array** | **[str]**| Form parameter enum test (string array) | [optional] if omitted the server will use the default value of "$"
 **enum_form_string** | **str**| Form parameter enum test (string) | [optional] if omitted the server will use the default value of "-efg"

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
> test_group_parameters(required_string_group, required_boolean_group, required_int64_group)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)
    required_string_group = 1 # int | Required String in group parameters
    required_boolean_group = True # bool | Required Boolean in group parameters
    required_int64_group = 1 # int | Required Integer in group parameters
    string_group = 1 # int | String in group parameters (optional)
    boolean_group = True # bool | Boolean in group parameters (optional)
    int64_group = 1 # int | Integer in group parameters (optional)

    # example passing only required values which don't have defaults set
    try:
        # Fake endpoint to test group parameters (optional)
        api_instance.test_group_parameters(required_string_group, required_boolean_group, required_int64_group)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->test_group_parameters: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Fake endpoint to test group parameters (optional)
        api_instance.test_group_parameters(required_string_group, required_boolean_group, required_int64_group, string_group=string_group, boolean_group=boolean_group, int64_group=int64_group)
    except petstore_api.ApiException as e:
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
import time
import petstore_api
from petstore_api.api import fake_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)
    param = {
        "key": "key_example",
    } # {str: (str,)} | request body

    # example passing only required values which don't have defaults set
    try:
        # test inline additionalProperties
        api_instance.test_inline_additional_properties(param)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->test_inline_additional_properties: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **{str: (str,)}**| request body |

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
import time
import petstore_api
from petstore_api.api import fake_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)
    param = "param_example" # str | field1
    param2 = "param2_example" # str | field2

    # example passing only required values which don't have defaults set
    try:
        # test json serialization of form data
        api_instance.test_json_form_data(param, param2)
    except petstore_api.ApiException as e:
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

