# petstore_api.FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fake_any_type_request_body**](FakeApi.md#fake_any_type_request_body) | **POST** /fake/any_type_body | test any type request body
[**fake_enum_ref_query_parameter**](FakeApi.md#fake_enum_ref_query_parameter) | **GET** /fake/enum_ref_query_parameter | test enum reference query parameter
[**fake_health_get**](FakeApi.md#fake_health_get) | **GET** /fake/health | Health check endpoint
[**fake_http_signature_test**](FakeApi.md#fake_http_signature_test) | **GET** /fake/http-signature-test | test http signature authentication
[**fake_outer_boolean_serialize**](FakeApi.md#fake_outer_boolean_serialize) | **POST** /fake/outer/boolean | 
[**fake_outer_composite_serialize**](FakeApi.md#fake_outer_composite_serialize) | **POST** /fake/outer/composite | 
[**fake_outer_number_serialize**](FakeApi.md#fake_outer_number_serialize) | **POST** /fake/outer/number | 
[**fake_outer_string_serialize**](FakeApi.md#fake_outer_string_serialize) | **POST** /fake/outer/string | 
[**fake_property_enum_integer_serialize**](FakeApi.md#fake_property_enum_integer_serialize) | **POST** /fake/property/enum-int | 
[**fake_ref_enum_string**](FakeApi.md#fake_ref_enum_string) | **GET** /fake/ref_enum_string | test ref to enum string
[**fake_return_boolean**](FakeApi.md#fake_return_boolean) | **GET** /fake/return_boolean | test returning boolean
[**fake_return_byte_like_json**](FakeApi.md#fake_return_byte_like_json) | **GET** /fake/return_byte_like_json | test byte like json
[**fake_return_enum**](FakeApi.md#fake_return_enum) | **GET** /fake/return_enum | test returning enum
[**fake_return_enum_like_json**](FakeApi.md#fake_return_enum_like_json) | **GET** /fake/return_enum_like_json | test enum like json
[**fake_return_float**](FakeApi.md#fake_return_float) | **GET** /fake/return_float | test returning float
[**fake_return_int**](FakeApi.md#fake_return_int) | **GET** /fake/return_int | test returning int
[**fake_return_list_of_objects**](FakeApi.md#fake_return_list_of_objects) | **GET** /fake/return_list_of_object | test returning list of objects
[**fake_return_str_like_json**](FakeApi.md#fake_return_str_like_json) | **GET** /fake/return_str_like_json | test str like json
[**fake_return_string**](FakeApi.md#fake_return_string) | **GET** /fake/return_string | test returning string
[**fake_uuid_example**](FakeApi.md#fake_uuid_example) | **GET** /fake/uuid_example | test uuid example
[**test_additional_properties_reference**](FakeApi.md#test_additional_properties_reference) | **POST** /fake/additionalProperties-reference | test referenced additionalProperties
[**test_body_with_binary**](FakeApi.md#test_body_with_binary) | **PUT** /fake/body-with-binary | 
[**test_body_with_file_schema**](FakeApi.md#test_body_with_file_schema) | **PUT** /fake/body-with-file-schema | 
[**test_body_with_query_params**](FakeApi.md#test_body_with_query_params) | **PUT** /fake/body-with-query-params | 
[**test_client_model**](FakeApi.md#test_client_model) | **PATCH** /fake | To test \&quot;client\&quot; model
[**test_date_time_query_parameter**](FakeApi.md#test_date_time_query_parameter) | **PUT** /fake/date-time-query-params | 
[**test_empty_and_non_empty_responses**](FakeApi.md#test_empty_and_non_empty_responses) | **POST** /fake/empty_and_non_empty_responses | test empty and non-empty responses
[**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**test_error_responses_with_model**](FakeApi.md#test_error_responses_with_model) | **POST** /fake/error_responses_with_model | test error responses with model
[**test_group_parameters**](FakeApi.md#test_group_parameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**test_inline_additional_properties**](FakeApi.md#test_inline_additional_properties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**test_inline_freeform_additional_properties**](FakeApi.md#test_inline_freeform_additional_properties) | **POST** /fake/inline-freeform-additionalProperties | test inline free-form additionalProperties
[**test_json_form_data**](FakeApi.md#test_json_form_data) | **GET** /fake/jsonFormData | test json serialization of form data
[**test_object_for_multipart_requests**](FakeApi.md#test_object_for_multipart_requests) | **POST** /fake/object_for_multipart_requests | 
[**test_query_parameter_collection_format**](FakeApi.md#test_query_parameter_collection_format) | **PUT** /fake/test-query-parameters | 
[**test_string_map_reference**](FakeApi.md#test_string_map_reference) | **POST** /fake/stringMap-reference | test referenced string map
[**upload_file_with_additional_properties**](FakeApi.md#upload_file_with_additional_properties) | **POST** /fake/upload_file_with_additional_properties | uploads a file and additional properties using multipart/form-data


# **fake_any_type_request_body**
> fake_any_type_request_body(body=body)

test any type request body

### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)
    body = None # object |  (optional)

    try:
        # test any type request body
        api_instance.fake_any_type_request_body(body=body)
    except Exception as e:
        print("Exception when calling FakeApi->fake_any_type_request_body: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **object**|  | [optional] 

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
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_enum_ref_query_parameter**
> fake_enum_ref_query_parameter(enum_ref=enum_ref)

test enum reference query parameter

### Example


```python
import petstore_api
from petstore_api.models.enum_class import EnumClass
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
    api_instance = petstore_api.FakeApi(api_client)
    enum_ref = -efg # EnumClass | enum reference (optional) (default to -efg)

    try:
        # test enum reference query parameter
        api_instance.fake_enum_ref_query_parameter(enum_ref=enum_ref)
    except Exception as e:
        print("Exception when calling FakeApi->fake_enum_ref_query_parameter: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_ref** | [**EnumClass**](.md)| enum reference | [optional] [default to -efg]

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
**200** | Get successful |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_health_get**
> HealthCheckResult fake_health_get()

Health check endpoint

### Example


```python
import petstore_api
from petstore_api.models.health_check_result import HealthCheckResult
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
    api_instance = petstore_api.FakeApi(api_client)

    try:
        # Health check endpoint
        api_response = api_instance.fake_health_get()
        print("The response of FakeApi->fake_health_get:\n")
        pprint(api_response)
    except Exception as e:
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

# **fake_http_signature_test**
> fake_http_signature_test(pet, query_1=query_1, header_1=header_1)

test http signature authentication

### Example


```python
import petstore_api
from petstore_api.models.pet import Pet
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

# Configure HTTP message signature: http_signature_test
# The HTTP Signature Header mechanism that can be used by a client to
# authenticate the sender of a message and ensure that particular headers
# have not been modified in transit.
#
# You can specify the signing key-id, private key path, signing scheme,
# signing algorithm, list of signed headers and signature max validity.
# The 'key_id' parameter is an opaque string that the API server can use
# to lookup the client and validate the signature.
# The 'private_key_path' parameter should be the path to a file that
# contains a DER or base-64 encoded private key.
# The 'private_key_passphrase' parameter is optional. Set the passphrase
# if the private key is encrypted.
# The 'signed_headers' parameter is used to specify the list of
# HTTP headers included when generating the signature for the message.
# You can specify HTTP headers that you want to protect with a cryptographic
# signature. Note that proxies may add, modify or remove HTTP headers
# for legitimate reasons, so you should only add headers that you know
# will not be modified. For example, if you want to protect the HTTP request
# body, you can specify the Digest header. In that case, the client calculates
# the digest of the HTTP request body and includes the digest in the message
# signature.
# The 'signature_max_validity' parameter is optional. It is configured as a
# duration to express when the signature ceases to be valid. The client calculates
# the expiration date every time it generates the cryptographic signature
# of an HTTP request. The API server may have its own security policy
# that controls the maximum validity of the signature. The client max validity
# must be lower than the server max validity.
# The time on the client and server must be synchronized, otherwise the
# server may reject the client signature.
#
# The client must use a combination of private key, signing scheme,
# signing algorithm and hash algorithm that matches the security policy of
# the API server.
#
# See petstore_api.signing for a list of all supported parameters.
from petstore_api import signing
import datetime

configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2",
    signing_info = petstore_api.HttpSigningConfiguration(
        key_id = 'my-key-id',
        private_key_path = 'private_key.pem',
        private_key_passphrase = 'YOUR_PASSPHRASE',
        signing_scheme = petstore_api.signing.SCHEME_HS2019,
        signing_algorithm = petstore_api.signing.ALGORITHM_ECDSA_MODE_FIPS_186_3,
        hash_algorithm = petstore_api.signing.SCHEME_RSA_SHA256,
        signed_headers = [
                            petstore_api.signing.HEADER_REQUEST_TARGET,
                            petstore_api.signing.HEADER_CREATED,
                            petstore_api.signing.HEADER_EXPIRES,
                            petstore_api.signing.HEADER_HOST,
                            petstore_api.signing.HEADER_DATE,
                            petstore_api.signing.HEADER_DIGEST,
                            'Content-Type',
                            'Content-Length',
                            'User-Agent'
                         ],
        signature_max_validity = datetime.timedelta(minutes=5)
    )
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeApi(api_client)
    pet = petstore_api.Pet() # Pet | Pet object that needs to be added to the store
    query_1 = 'query_1_example' # str | query parameter (optional)
    header_1 = 'header_1_example' # str | header parameter (optional)

    try:
        # test http signature authentication
        api_instance.fake_http_signature_test(pet, query_1=query_1, header_1=header_1)
    except Exception as e:
        print("Exception when calling FakeApi->fake_http_signature_test: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 
 **query_1** | **str**| query parameter | [optional] 
 **header_1** | **str**| header parameter | [optional] 

### Return type

void (empty response body)

### Authorization

[http_signature_test](../README.md#http_signature_test)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | The instance started successfully |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_boolean_serialize**
> bool fake_outer_boolean_serialize(body=body)

Test serialization of outer boolean types

### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)
    body = True # bool | Input boolean as post body (optional)

    try:
        api_response = api_instance.fake_outer_boolean_serialize(body=body)
        print("The response of FakeApi->fake_outer_boolean_serialize:\n")
        pprint(api_response)
    except Exception as e:
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

 - **Content-Type**: application/json
 - **Accept**: */*

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output boolean |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_outer_composite_serialize**
> OuterComposite fake_outer_composite_serialize(outer_composite=outer_composite)

Test serialization of object with outer number type

### Example


```python
import petstore_api
from petstore_api.models.outer_composite import OuterComposite
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
    api_instance = petstore_api.FakeApi(api_client)
    outer_composite = petstore_api.OuterComposite() # OuterComposite | Input composite as post body (optional)

    try:
        api_response = api_instance.fake_outer_composite_serialize(outer_composite=outer_composite)
        print("The response of FakeApi->fake_outer_composite_serialize:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling FakeApi->fake_outer_composite_serialize: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outer_composite** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional] 

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
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
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)
    body = 3.4 # float | Input number as post body (optional)

    try:
        api_response = api_instance.fake_outer_number_serialize(body=body)
        print("The response of FakeApi->fake_outer_number_serialize:\n")
        pprint(api_response)
    except Exception as e:
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

 - **Content-Type**: application/json
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
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)
    body = 'body_example' # str | Input string as post body (optional)

    try:
        api_response = api_instance.fake_outer_string_serialize(body=body)
        print("The response of FakeApi->fake_outer_string_serialize:\n")
        pprint(api_response)
    except Exception as e:
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

 - **Content-Type**: application/json
 - **Accept**: */*

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output string |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_property_enum_integer_serialize**
> OuterObjectWithEnumProperty fake_property_enum_integer_serialize(outer_object_with_enum_property, param=param)

Test serialization of enum (int) properties with examples

### Example


```python
import petstore_api
from petstore_api.models.outer_enum_integer import OuterEnumInteger
from petstore_api.models.outer_object_with_enum_property import OuterObjectWithEnumProperty
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
    api_instance = petstore_api.FakeApi(api_client)
    outer_object_with_enum_property = petstore_api.OuterObjectWithEnumProperty() # OuterObjectWithEnumProperty | Input enum (int) as post body
    param = [petstore_api.OuterEnumInteger()] # List[OuterEnumInteger] |  (optional)

    try:
        api_response = api_instance.fake_property_enum_integer_serialize(outer_object_with_enum_property, param=param)
        print("The response of FakeApi->fake_property_enum_integer_serialize:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling FakeApi->fake_property_enum_integer_serialize: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outer_object_with_enum_property** | [**OuterObjectWithEnumProperty**](OuterObjectWithEnumProperty.md)| Input enum (int) as post body | 
 **param** | [**List[OuterEnumInteger]**](OuterEnumInteger.md)|  | [optional] 

### Return type

[**OuterObjectWithEnumProperty**](OuterObjectWithEnumProperty.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Output enum (int) |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_ref_enum_string**
> EnumClass fake_ref_enum_string()

test ref to enum string

### Example


```python
import petstore_api
from petstore_api.models.enum_class import EnumClass
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
    api_instance = petstore_api.FakeApi(api_client)

    try:
        # test ref to enum string
        api_response = api_instance.fake_ref_enum_string()
        print("The response of FakeApi->fake_ref_enum_string:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling FakeApi->fake_ref_enum_string: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

[**EnumClass**](EnumClass.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: plain/text

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_return_boolean**
> bool fake_return_boolean()

test returning boolean

### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)

    try:
        # test returning boolean
        api_response = api_instance.fake_return_boolean()
        print("The response of FakeApi->fake_return_boolean:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling FakeApi->fake_return_boolean: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

**bool**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_return_byte_like_json**
> bytearray fake_return_byte_like_json()

test byte like json

### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)

    try:
        # test byte like json
        api_response = api_instance.fake_return_byte_like_json()
        print("The response of FakeApi->fake_return_byte_like_json:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling FakeApi->fake_return_byte_like_json: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

**bytearray**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: plain/text

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_return_enum**
> str fake_return_enum()

test returning enum

### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)

    try:
        # test returning enum
        api_response = api_instance.fake_return_enum()
        print("The response of FakeApi->fake_return_enum:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling FakeApi->fake_return_enum: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

**str**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_return_enum_like_json**
> str fake_return_enum_like_json()

test enum like json

### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)

    try:
        # test enum like json
        api_response = api_instance.fake_return_enum_like_json()
        print("The response of FakeApi->fake_return_enum_like_json:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling FakeApi->fake_return_enum_like_json: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

**str**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: plain/text

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_return_float**
> float fake_return_float()

test returning float

### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)

    try:
        # test returning float
        api_response = api_instance.fake_return_float()
        print("The response of FakeApi->fake_return_float:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling FakeApi->fake_return_float: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

**float**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_return_int**
> int fake_return_int()

test returning int

### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)

    try:
        # test returning int
        api_response = api_instance.fake_return_int()
        print("The response of FakeApi->fake_return_int:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling FakeApi->fake_return_int: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

**int**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_return_list_of_objects**
> List[List[Tag]] fake_return_list_of_objects()

test returning list of objects

### Example


```python
import petstore_api
from petstore_api.models.tag import Tag
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
    api_instance = petstore_api.FakeApi(api_client)

    try:
        # test returning list of objects
        api_response = api_instance.fake_return_list_of_objects()
        print("The response of FakeApi->fake_return_list_of_objects:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling FakeApi->fake_return_list_of_objects: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

**List[List[Tag]]**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_return_str_like_json**
> str fake_return_str_like_json()

test str like json

### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)

    try:
        # test str like json
        api_response = api_instance.fake_return_str_like_json()
        print("The response of FakeApi->fake_return_str_like_json:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling FakeApi->fake_return_str_like_json: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

**str**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: plain/text

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_return_string**
> str fake_return_string()

test returning string

### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)

    try:
        # test returning string
        api_response = api_instance.fake_return_string()
        print("The response of FakeApi->fake_return_string:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling FakeApi->fake_return_string: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

**str**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_uuid_example**
> fake_uuid_example(uuid_example)

test uuid example

### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)
    uuid_example = '84529ad2-2265-4e15-b76b-c17025d848f6' # str | uuid example

    try:
        # test uuid example
        api_instance.fake_uuid_example(uuid_example)
    except Exception as e:
        print("Exception when calling FakeApi->fake_uuid_example: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **uuid_example** | **str**| uuid example | 

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
**200** | Get successful |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_additional_properties_reference**
> test_additional_properties_reference(request_body)

test referenced additionalProperties



### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)
    request_body = None # Dict[str, object] | request body

    try:
        # test referenced additionalProperties
        api_instance.test_additional_properties_reference(request_body)
    except Exception as e:
        print("Exception when calling FakeApi->test_additional_properties_reference: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **request_body** | [**Dict[str, object]**](object.md)| request body | 

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

# **test_body_with_binary**
> test_body_with_binary(body)

For this test, the body has to be a binary file.

### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)
    body = None # bytearray | image to upload

    try:
        api_instance.test_body_with_binary(body)
    except Exception as e:
        print("Exception when calling FakeApi->test_body_with_binary: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **bytearray**| image to upload | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: image/png
 - **Accept**: Not defined

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_body_with_file_schema**
> test_body_with_file_schema(file_schema_test_class)

For this test, the body for this request must reference a schema named `File`.

### Example


```python
import petstore_api
from petstore_api.models.file_schema_test_class import FileSchemaTestClass
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
    api_instance = petstore_api.FakeApi(api_client)
    file_schema_test_class = petstore_api.FileSchemaTestClass() # FileSchemaTestClass | 

    try:
        api_instance.test_body_with_file_schema(file_schema_test_class)
    except Exception as e:
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
import petstore_api
from petstore_api.models.user import User
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
    api_instance = petstore_api.FakeApi(api_client)
    query = 'query_example' # str | 
    user = petstore_api.User() # User | 

    try:
        api_instance.test_body_with_query_params(query, user)
    except Exception as e:
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

To test "client" model

### Example


```python
import petstore_api
from petstore_api.models.client import Client
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
    api_instance = petstore_api.FakeApi(api_client)
    client = petstore_api.Client() # Client | client model

    try:
        # To test \"client\" model
        api_response = api_instance.test_client_model(client)
        print("The response of FakeApi->test_client_model:\n")
        pprint(api_response)
    except Exception as e:
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

# **test_date_time_query_parameter**
> test_date_time_query_parameter(date_time_query, str_query)

### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)
    date_time_query = '2013-10-20T19:20:30+01:00' # datetime | 
    str_query = 'str_query_example' # str | 

    try:
        api_instance.test_date_time_query_parameter(date_time_query, str_query)
    except Exception as e:
        print("Exception when calling FakeApi->test_date_time_query_parameter: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **date_time_query** | **datetime**|  | 
 **str_query** | **str**|  | 

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

# **test_empty_and_non_empty_responses**
> test_empty_and_non_empty_responses()

test empty and non-empty responses



### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)

    try:
        # test empty and non-empty responses
        api_instance.test_empty_and_non_empty_responses()
    except Exception as e:
        print("Exception when calling FakeApi->test_empty_and_non_empty_responses: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**204** | Success, but no response content |  -  |
**206** | Partial response content |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_endpoint_parameters**
> test_endpoint_parameters(number, double, pattern_without_delimiter, byte, integer=integer, int32=int32, int64=int64, var_float=var_float, string=string, binary=binary, byte_with_max_length=byte_with_max_length, var_date=var_date, date_time=date_time, password=password, param_callback=param_callback)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters
假端點
偽のエンドポイント
가짜 엔드 포인트


### Example

* Basic Authentication (http_basic_test):

```python
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
    username = os.environ["USERNAME"],
    password = os.environ["PASSWORD"]
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeApi(api_client)
    number = 3.4 # float | None
    double = 3.4 # float | None
    pattern_without_delimiter = 'pattern_without_delimiter_example' # str | None
    byte = None # bytearray | None
    integer = 56 # int | None (optional)
    int32 = 56 # int | None (optional)
    int64 = 56 # int | None (optional)
    var_float = 3.4 # float | None (optional)
    string = 'string_example' # str | None (optional)
    binary = None # bytearray | None (optional)
    byte_with_max_length = None # bytearray | None (optional)
    var_date = '2013-10-20' # date | None (optional)
    date_time = '2013-10-20T19:20:30+01:00' # datetime | None (optional)
    password = 'password_example' # str | None (optional)
    param_callback = 'param_callback_example' # str | None (optional)

    try:
        # Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
        api_instance.test_endpoint_parameters(number, double, pattern_without_delimiter, byte, integer=integer, int32=int32, int64=int64, var_float=var_float, string=string, binary=binary, byte_with_max_length=byte_with_max_length, var_date=var_date, date_time=date_time, password=password, param_callback=param_callback)
    except Exception as e:
        print("Exception when calling FakeApi->test_endpoint_parameters: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **float**| None | 
 **double** | **float**| None | 
 **pattern_without_delimiter** | **str**| None | 
 **byte** | **bytearray**| None | 
 **integer** | **int**| None | [optional] 
 **int32** | **int**| None | [optional] 
 **int64** | **int**| None | [optional] 
 **var_float** | **float**| None | [optional] 
 **string** | **str**| None | [optional] 
 **binary** | **bytearray**| None | [optional] 
 **byte_with_max_length** | **bytearray**| None | [optional] 
 **var_date** | **date**| None | [optional] 
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

# **test_error_responses_with_model**
> test_error_responses_with_model()

test error responses with model

### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)

    try:
        # test error responses with model
        api_instance.test_error_responses_with_model()
    except Exception as e:
        print("Exception when calling FakeApi->test_error_responses_with_model: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**204** | Success, but no response content |  -  |
**400** |  |  -  |
**404** |  |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_group_parameters**
> test_group_parameters(required_string_group, required_boolean_group, required_int64_group, string_group=string_group, boolean_group=boolean_group, int64_group=int64_group)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example

* Bearer (JWT) Authentication (bearer_test):

```python
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

# Configure Bearer authorization (JWT): bearer_test
configuration = petstore_api.Configuration(
    access_token = os.environ["BEARER_TOKEN"]
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
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
    except Exception as e:
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
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)
    request_body = {'key': 'request_body_example'} # Dict[str, str] | request body

    try:
        # test inline additionalProperties
        api_instance.test_inline_additional_properties(request_body)
    except Exception as e:
        print("Exception when calling FakeApi->test_inline_additional_properties: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **request_body** | [**Dict[str, str]**](str.md)| request body | 

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

# **test_inline_freeform_additional_properties**
> test_inline_freeform_additional_properties(test_inline_freeform_additional_properties_request)

test inline free-form additionalProperties



### Example


```python
import petstore_api
from petstore_api.models.test_inline_freeform_additional_properties_request import TestInlineFreeformAdditionalPropertiesRequest
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
    api_instance = petstore_api.FakeApi(api_client)
    test_inline_freeform_additional_properties_request = petstore_api.TestInlineFreeformAdditionalPropertiesRequest() # TestInlineFreeformAdditionalPropertiesRequest | request body

    try:
        # test inline free-form additionalProperties
        api_instance.test_inline_freeform_additional_properties(test_inline_freeform_additional_properties_request)
    except Exception as e:
        print("Exception when calling FakeApi->test_inline_freeform_additional_properties: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **test_inline_freeform_additional_properties_request** | [**TestInlineFreeformAdditionalPropertiesRequest**](TestInlineFreeformAdditionalPropertiesRequest.md)| request body | 

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
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)
    param = 'param_example' # str | field1
    param2 = 'param2_example' # str | field2

    try:
        # test json serialization of form data
        api_instance.test_json_form_data(param, param2)
    except Exception as e:
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

# **test_object_for_multipart_requests**
> test_object_for_multipart_requests(marker)

### Example


```python
import petstore_api
from petstore_api.models.test_object_for_multipart_requests_request_marker import TestObjectForMultipartRequestsRequestMarker
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
    api_instance = petstore_api.FakeApi(api_client)
    marker = petstore_api.TestObjectForMultipartRequestsRequestMarker() # TestObjectForMultipartRequestsRequestMarker | 

    try:
        api_instance.test_object_for_multipart_requests(marker)
    except Exception as e:
        print("Exception when calling FakeApi->test_object_for_multipart_requests: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **marker** | [**TestObjectForMultipartRequestsRequestMarker**](TestObjectForMultipartRequestsRequestMarker.md)|  | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: Not defined

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Success |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_query_parameter_collection_format**
> test_query_parameter_collection_format(pipe, ioutil, http, url, context, allow_empty, language=language)

To test the collection format in query parameters

### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)
    pipe = ['pipe_example'] # List[str] | 
    ioutil = ['ioutil_example'] # List[str] | 
    http = ['http_example'] # List[str] | 
    url = ['url_example'] # List[str] | 
    context = ['context_example'] # List[str] | 
    allow_empty = 'allow_empty_example' # str | 
    language = {'key': 'language_example'} # Dict[str, str] |  (optional)

    try:
        api_instance.test_query_parameter_collection_format(pipe, ioutil, http, url, context, allow_empty, language=language)
    except Exception as e:
        print("Exception when calling FakeApi->test_query_parameter_collection_format: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**List[str]**](str.md)|  | 
 **ioutil** | [**List[str]**](str.md)|  | 
 **http** | [**List[str]**](str.md)|  | 
 **url** | [**List[str]**](str.md)|  | 
 **context** | [**List[str]**](str.md)|  | 
 **allow_empty** | **str**|  | 
 **language** | [**Dict[str, str]**](str.md)|  | [optional] 

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

# **test_string_map_reference**
> test_string_map_reference(request_body)

test referenced string map



### Example


```python
import petstore_api
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
    api_instance = petstore_api.FakeApi(api_client)
    request_body = {'key': 'request_body_example'} # Dict[str, str] | request body

    try:
        # test referenced string map
        api_instance.test_string_map_reference(request_body)
    except Exception as e:
        print("Exception when calling FakeApi->test_string_map_reference: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **request_body** | [**Dict[str, str]**](str.md)| request body | 

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

# **upload_file_with_additional_properties**
> ModelApiResponse upload_file_with_additional_properties(file, object=object, count=count)

uploads a file and additional properties using multipart/form-data



### Example


```python
import petstore_api
from petstore_api.models.model_api_response import ModelApiResponse
from petstore_api.models.upload_file_with_additional_properties_request_object import UploadFileWithAdditionalPropertiesRequestObject
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
    api_instance = petstore_api.FakeApi(api_client)
    file = None # bytearray | file to upload
    object = petstore_api.UploadFileWithAdditionalPropertiesRequestObject() # UploadFileWithAdditionalPropertiesRequestObject |  (optional)
    count = 56 # int | Integer count (optional)

    try:
        # uploads a file and additional properties using multipart/form-data
        api_response = api_instance.upload_file_with_additional_properties(file, object=object, count=count)
        print("The response of FakeApi->upload_file_with_additional_properties:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling FakeApi->upload_file_with_additional_properties: %s\n" % e)
```



### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **file** | **bytearray**| file to upload | 
 **object** | [**UploadFileWithAdditionalPropertiesRequestObject**](UploadFileWithAdditionalPropertiesRequestObject.md)|  | [optional] 
 **count** | **int**| Integer count | [optional] 

### Return type

[**ModelApiResponse**](ModelApiResponse.md)

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

