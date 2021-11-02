# petstore_api.FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**additional_properties_with_array_of_enums**](FakeApi.md#additional_properties_with_array_of_enums) | **GET** /fake/additional-properties-with-array-of-enums | Additional Properties with Array of Enums
[**array_model**](FakeApi.md#array_model) | **POST** /fake/refs/arraymodel | 
[**array_of_enums**](FakeApi.md#array_of_enums) | **POST** /fake/refs/array-of-enums | Array of Enums
[**boolean**](FakeApi.md#boolean) | **POST** /fake/refs/boolean | 
[**composed_one_of_number_with_validations**](FakeApi.md#composed_one_of_number_with_validations) | **POST** /fake/refs/composed_one_of_number_with_validations | 
[**download_attachment**](FakeApi.md#download_attachment) | **GET** /{fileName} | downloads a file using Content-Disposition
[**enum_test**](FakeApi.md#enum_test) | **POST** /fake/refs/enum-test | Object contains enum properties and array properties containing enums
[**fake_health_get**](FakeApi.md#fake_health_get) | **GET** /fake/health | Health check endpoint
[**mammal**](FakeApi.md#mammal) | **POST** /fake/refs/mammal | 
[**number_with_validations**](FakeApi.md#number_with_validations) | **POST** /fake/refs/number | 
[**object_model_with_ref_props**](FakeApi.md#object_model_with_ref_props) | **POST** /fake/refs/object_model_with_ref_props | 
[**post_inline_additional_properties_payload**](FakeApi.md#post_inline_additional_properties_payload) | **POST** /fake/postInlineAdditionalPropertiesPayload | 
[**post_inline_additional_properties_ref_payload**](FakeApi.md#post_inline_additional_properties_ref_payload) | **POST** /fake/postInlineAdditionalPropertiesRefPayload | 
[**string**](FakeApi.md#string) | **POST** /fake/refs/string | 
[**string_enum**](FakeApi.md#string_enum) | **POST** /fake/refs/enum | 
[**test_body_with_file_schema**](FakeApi.md#test_body_with_file_schema) | **PUT** /fake/body-with-file-schema | 
[**test_body_with_query_params**](FakeApi.md#test_body_with_query_params) | **PUT** /fake/body-with-query-params | 
[**test_client_model**](FakeApi.md#test_client_model) | **PATCH** /fake | To test \&quot;client\&quot; model
[**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**test_enum_parameters**](FakeApi.md#test_enum_parameters) | **GET** /fake | To test enum parameters
[**test_group_parameters**](FakeApi.md#test_group_parameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**test_inline_additional_properties**](FakeApi.md#test_inline_additional_properties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**test_json_form_data**](FakeApi.md#test_json_form_data) | **GET** /fake/jsonFormData | test json serialization of form data
[**test_query_parameter_collection_format**](FakeApi.md#test_query_parameter_collection_format) | **PUT** /fake/test-query-parameters | 
[**tx_rx_any_of_model**](FakeApi.md#tx_rx_any_of_model) | **POST** /fake/TxRxAnyOfModel | 
[**upload_download_file**](FakeApi.md#upload_download_file) | **POST** /fake/uploadDownloadFile | uploads a file and downloads a file using application/octet-stream
[**upload_file**](FakeApi.md#upload_file) | **POST** /fake/uploadFile | uploads a file using multipart/form-data
[**upload_files**](FakeApi.md#upload_files) | **POST** /fake/uploadFiles | uploads files using multipart/form-data


# **additional_properties_with_array_of_enums**
> AdditionalPropertiesWithArrayOfEnums additional_properties_with_array_of_enums()

Additional Properties with Array of Enums

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.additional_properties_with_array_of_enums import AdditionalPropertiesWithArrayOfEnums
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
    additional_properties_with_array_of_enums = AdditionalPropertiesWithArrayOfEnums(
        key=[
            EnumClass("-efg"),
        ],
    ) # AdditionalPropertiesWithArrayOfEnums | Input enum (optional)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Additional Properties with Array of Enums
        api_response = api_instance.additional_properties_with_array_of_enums(additional_properties_with_array_of_enums=additional_properties_with_array_of_enums)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->additional_properties_with_array_of_enums: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **additional_properties_with_array_of_enums** | [**AdditionalPropertiesWithArrayOfEnums**](AdditionalPropertiesWithArrayOfEnums.md)| Input enum | [optional]

### Return type

[**AdditionalPropertiesWithArrayOfEnums**](AdditionalPropertiesWithArrayOfEnums.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Got object with additional properties with array of enums |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

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

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output model |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **array_of_enums**
> ArrayOfEnums array_of_enums()

Array of Enums

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.array_of_enums import ArrayOfEnums
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
    array_of_enums = ArrayOfEnums([
        StringEnum("placed"),
    ]) # ArrayOfEnums | Input enum (optional)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Array of Enums
        api_response = api_instance.array_of_enums(array_of_enums=array_of_enums)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->array_of_enums: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **array_of_enums** | [**ArrayOfEnums**](ArrayOfEnums.md)| Input enum | [optional]

### Return type

[**ArrayOfEnums**](ArrayOfEnums.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Got named array of enums |  -  |

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

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output boolean |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **composed_one_of_number_with_validations**
> ComposedOneOfNumberWithValidations composed_one_of_number_with_validations()



Test serialization of object with $refed properties

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.composed_one_of_number_with_validations import ComposedOneOfNumberWithValidations
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
    composed_one_of_number_with_validations = ComposedOneOfNumberWithValidations(None) # ComposedOneOfNumberWithValidations | Input model (optional)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        api_response = api_instance.composed_one_of_number_with_validations(composed_one_of_number_with_validations=composed_one_of_number_with_validations)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->composed_one_of_number_with_validations: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **composed_one_of_number_with_validations** | [**ComposedOneOfNumberWithValidations**](ComposedOneOfNumberWithValidations.md)| Input model | [optional]

### Return type

[**ComposedOneOfNumberWithValidations**](ComposedOneOfNumberWithValidations.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output model |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **download_attachment**
> file_type download_attachment(file_name)

downloads a file using Content-Disposition

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
    file_name = "fileName_example" # str | file name

    # example passing only required values which don't have defaults set
    try:
        # downloads a file using Content-Disposition
        api_response = api_instance.download_attachment(file_name)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->download_attachment: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **file_name** | **str**| file name |

### Return type

**file_type**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  * Content-Disposition - describes the received file. Looks like: &#39;attachment; filename&#x3D;fileName.txt&#39; <br>  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **enum_test**
> EnumTest enum_test()

Object contains enum properties and array properties containing enums

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.enum_test import EnumTest
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
    enum_test = EnumTest(
        enum_string="UPPER",
        enum_string_required="UPPER",
        enum_integer=1,
        enum_number=1.1,
        enum_bool=False,
        bool_enum=BooleanEnum(True),
        string_enum=StringEnum("placed"),
        integer_enum=IntegerEnum(0),
        string_enum_with_default_value=StringEnumWithDefaultValue("placed"),
        integer_enum_with_default_value=IntegerEnumWithDefaultValue(0),
        integer_enum_one_value=IntegerEnumOneValue(0),
        inline_array_of_str_enum=[
            StringEnum("placed"),
        ],
        array_of_str_enum=ArrayOfEnums([
            StringEnum("placed"),
        ]),
    ) # EnumTest | Input object (optional)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Object contains enum properties and array properties containing enums
        api_response = api_instance.enum_test(enum_test=enum_test)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->enum_test: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_test** | [**EnumTest**](EnumTest.md)| Input object | [optional]

### Return type

[**EnumTest**](EnumTest.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Got object containing enums |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_health_get**
> HealthCheckResult fake_health_get()

Health check endpoint

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.health_check_result import HealthCheckResult
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

    # example, this endpoint has no required or optional parameters
    try:
        # Health check endpoint
        api_response = api_instance.fake_health_get()
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->fake_health_get: %s\n" % e)
```


### Parameters
This endpoint does not need any parameter.

### Return type

[**HealthCheckResult**](HealthCheckResult.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | The instance started successfully |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **mammal**
> Mammal mammal(mammal)



Test serialization of mammals

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.mammal import Mammal
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
    mammal = Mammal(None) # Mammal | Input mammal

    # example passing only required values which don't have defaults set
    try:
        api_response = api_instance.mammal(mammal)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->mammal: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **mammal** | [**Mammal**](Mammal.md)| Input mammal |

### Return type

[**Mammal**](Mammal.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output mammal |  -  |

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
    body = NumberWithValidations(10) # NumberWithValidations | Input number as post body (optional)

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

 - **Content-Type**: application/json
 - **Accept**: application/json


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
        my_number=NumberWithValidations(10),
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

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output model |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **post_inline_additional_properties_payload**
> InlineObject6 post_inline_additional_properties_payload()



### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.inline_object6 import InlineObject6
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
    inline_object6 = InlineObject6(
        array_data=[
            FakePostInlineAdditionalPropertiesPayloadArrayData(
                labels=[
                    "labels_example",
                ],
            ),
        ],
    ) # InlineObject6 |  (optional)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        api_response = api_instance.post_inline_additional_properties_payload(inline_object6=inline_object6)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->post_inline_additional_properties_payload: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **inline_object6** | [**InlineObject6**](InlineObject6.md)|  | [optional]

### Return type

[**InlineObject6**](InlineObject6.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | InlineAdditionalPropertiesPayload |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **post_inline_additional_properties_ref_payload**
> InlineAdditionalPropertiesRefPayload post_inline_additional_properties_ref_payload()



### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.inline_additional_properties_ref_payload import InlineAdditionalPropertiesRefPayload
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
    inline_additional_properties_ref_payload = InlineAdditionalPropertiesRefPayload(
        array_data=[
            FakePostInlineAdditionalPropertiesPayloadArrayData(
                labels=[
                    "labels_example",
                ],
            ),
        ],
    ) # InlineAdditionalPropertiesRefPayload |  (optional)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        api_response = api_instance.post_inline_additional_properties_ref_payload(inline_additional_properties_ref_payload=inline_additional_properties_ref_payload)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->post_inline_additional_properties_ref_payload: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **inline_additional_properties_ref_payload** | [**InlineAdditionalPropertiesRefPayload**](InlineAdditionalPropertiesRefPayload.md)|  | [optional]

### Return type

[**InlineAdditionalPropertiesRefPayload**](InlineAdditionalPropertiesRefPayload.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | InlineAdditionalPropertiesRefPayload |  -  |

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

 - **Content-Type**: application/json
 - **Accept**: application/json


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

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output enum |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_body_with_file_schema**
> test_body_with_file_schema(file_schema_test_class)



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
    file_schema_test_class = FileSchemaTestClass(
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
        api_instance.test_body_with_file_schema(file_schema_test_class)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->test_body_with_file_schema: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **file_schema_test_class** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  |

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
> test_body_with_query_params(query, user)



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
    user = User(
        id=1,
        username="username_example",
        first_name="first_name_example",
        last_name="last_name_example",
        email="email_example",
        password="password_example",
        phone="phone_example",
        user_status=1,
        object_with_no_declared_props={},
        object_with_no_declared_props_nullable={},
        any_type_prop=None,
        any_type_prop_nullable=None,
    ) # User | 

    # example passing only required values which don't have defaults set
    try:
        api_instance.test_body_with_query_params(query, user)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->test_body_with_query_params: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **str**|  |
 **user** | [**User**](User.md)|  |

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
> Client test_client_model(client)

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
    client = Client(
        client="client_example",
    ) # Client | client model

    # example passing only required values which don't have defaults set
    try:
        # To test \"client\" model
        api_response = api_instance.test_client_model(client)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->test_client_model: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | [**Client**](Client.md)| client model |

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
> test_endpoint_parameters(number, double, pattern_without_delimiter, byte)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

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
    date_time = dateutil_parser('2020-02-02T20:20:20.22222Z') # datetime | None (optional) if omitted the server will use the default value of dateutil_parser('2010-02-01T10:20:10.11111+01:00')
    password = "password_example" # str | None (optional)
    param_callback = "param_callback_example" # str | None (optional)

    # example passing only required values which don't have defaults set
    try:
        # Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
        api_instance.test_endpoint_parameters(number, double, pattern_without_delimiter, byte)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->test_endpoint_parameters: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
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
 **date_time** | **datetime**| None | [optional] if omitted the server will use the default value of dateutil_parser('2010-02-01T10:20:10.11111+01:00')
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

* Bearer (JWT) Authentication (bearer_test):

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

# Configure Bearer authorization (JWT): bearer_test
configuration = petstore_api.Configuration(
    access_token = 'YOUR_BEARER_TOKEN'
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
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

[bearer_test](../README.md#bearer_test)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**400** | Someting wrong |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_inline_additional_properties**
> test_inline_additional_properties(request_body)

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
    request_body = {
        "key": "key_example",
    } # {str: (str,)} | request body

    # example passing only required values which don't have defaults set
    try:
        # test inline additionalProperties
        api_instance.test_inline_additional_properties(request_body)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->test_inline_additional_properties: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **request_body** | **{str: (str,)}**| request body |

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

# **test_query_parameter_collection_format**
> test_query_parameter_collection_format(pipe, ioutil, http, url, context)



To test the collection format in query parameters

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
    pipe = [
        "pipe_example",
    ] # [str] | 
    ioutil = [
        "ioutil_example",
    ] # [str] | 
    http = [
        "http_example",
    ] # [str] | 
    url = [
        "url_example",
    ] # [str] | 
    context = [
        "context_example",
    ] # [str] | 

    # example passing only required values which don't have defaults set
    try:
        api_instance.test_query_parameter_collection_format(pipe, ioutil, http, url, context)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->test_query_parameter_collection_format: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | **[str]**|  |
 **ioutil** | **[str]**|  |
 **http** | **[str]**|  |
 **url** | **[str]**|  |
 **context** | **[str]**|  |

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

# **tx_rx_any_of_model**
> GmFruitNoProperties tx_rx_any_of_model()



### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.gm_fruit_no_properties import GmFruitNoProperties
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
    gm_fruit_no_properties = GmFruitNoProperties(None) # GmFruitNoProperties |  (optional)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        api_response = api_instance.tx_rx_any_of_model(gm_fruit_no_properties=gm_fruit_no_properties)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->tx_rx_any_of_model: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **gm_fruit_no_properties** | [**GmFruitNoProperties**](GmFruitNoProperties.md)|  | [optional]

### Return type

[**GmFruitNoProperties**](GmFruitNoProperties.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | TxRxAnyOfModel |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **upload_download_file**
> file_type upload_download_file(body)

uploads a file and downloads a file using application/octet-stream

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
    body = open('/path/to/file', 'rb') # file_type | 

    # example passing only required values which don't have defaults set
    try:
        # uploads a file and downloads a file using application/octet-stream
        api_response = api_instance.upload_download_file(body)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->upload_download_file: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **file_type**|  |

### Return type

**file_type**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/octet-stream
 - **Accept**: application/octet-stream


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **upload_file**
> ApiResponse upload_file(file)

uploads a file using multipart/form-data

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.api_response import ApiResponse
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
    file = open('/path/to/file', 'rb') # file_type | file to upload
    additional_metadata = "additional_metadata_example" # str | Additional data to pass to server (optional)

    # example passing only required values which don't have defaults set
    try:
        # uploads a file using multipart/form-data
        api_response = api_instance.upload_file(file)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->upload_file: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # uploads a file using multipart/form-data
        api_response = api_instance.upload_file(file, additional_metadata=additional_metadata)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->upload_file: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **file** | **file_type**| file to upload |
 **additional_metadata** | **str**| Additional data to pass to server | [optional]

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **upload_files**
> ApiResponse upload_files()

uploads files using multipart/form-data

### Example


```python
import time
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.api_response import ApiResponse
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
    files = [
        open('/path/to/file', 'rb'),
    ] # [file_type] |  (optional)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # uploads files using multipart/form-data
        api_response = api_instance.upload_files(files=files)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->upload_files: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **files** | **[file_type]**|  | [optional]

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json


### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

