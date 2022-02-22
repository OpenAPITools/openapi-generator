# petstore_api.FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**additional_properties_with_array_of_enums**](FakeApi.md#additional_properties_with_array_of_enums) | **GET** /fake/additional-properties-with-array-of-enums | Additional Properties with Array of Enums
[**array_model**](FakeApi.md#array_model) | **POST** /fake/refs/arraymodel | 
[**array_of_enums**](FakeApi.md#array_of_enums) | **POST** /fake/refs/array-of-enums | Array of Enums
[**body_with_file_schema**](FakeApi.md#body_with_file_schema) | **PUT** /fake/body-with-file-schema | 
[**body_with_query_params**](FakeApi.md#body_with_query_params) | **PUT** /fake/body-with-query-params | 
[**boolean**](FakeApi.md#boolean) | **POST** /fake/refs/boolean | 
[**case_sensitive_params**](FakeApi.md#case_sensitive_params) | **PUT** /fake/case-sensitive-params | 
[**client_model**](FakeApi.md#client_model) | **PATCH** /fake | To test \&quot;client\&quot; model
[**composed_one_of_different_types**](FakeApi.md#composed_one_of_different_types) | **POST** /fake/refs/composed_one_of_number_with_validations | 
[**endpoint_parameters**](FakeApi.md#endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**enum_parameters**](FakeApi.md#enum_parameters) | **GET** /fake | To test enum parameters
[**fake_health_get**](FakeApi.md#fake_health_get) | **GET** /fake/health | Health check endpoint
[**group_parameters**](FakeApi.md#group_parameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**inline_additional_properties**](FakeApi.md#inline_additional_properties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**json_form_data**](FakeApi.md#json_form_data) | **GET** /fake/jsonFormData | test json serialization of form data
[**mammal**](FakeApi.md#mammal) | **POST** /fake/refs/mammal | 
[**number_with_validations**](FakeApi.md#number_with_validations) | **POST** /fake/refs/number | 
[**object_model_with_ref_props**](FakeApi.md#object_model_with_ref_props) | **POST** /fake/refs/object_model_with_ref_props | 
[**parameter_collisions**](FakeApi.md#parameter_collisions) | **POST** /fake/parameterCollisions/{1}/{aB}/{Ab}/{self}/{A-B}/ | parameter collision case
[**query_parameter_collection_format**](FakeApi.md#query_parameter_collection_format) | **PUT** /fake/test-query-paramters | 
[**string**](FakeApi.md#string) | **POST** /fake/refs/string | 
[**string_enum**](FakeApi.md#string_enum) | **POST** /fake/refs/enum | 
[**upload_download_file**](FakeApi.md#upload_download_file) | **POST** /fake/uploadDownloadFile | uploads a file and downloads a file using application/octet-stream
[**upload_file**](FakeApi.md#upload_file) | **POST** /fake/uploadFile | uploads a file using multipart/form-data
[**upload_files**](FakeApi.md#upload_files) | **POST** /fake/uploadFiles | uploads files using multipart/form-data

# **additional_properties_with_array_of_enums**
> AdditionalPropertiesWithArrayOfEnums additional_properties_with_array_of_enums()

Additional Properties with Array of Enums

### Example

```python
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
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only optional values
    body = AdditionalPropertiesWithArrayOfEnums(
        key=[
            EnumClass("-efg"),
        ],
    )
    try:
        # Additional Properties with Array of Enums
        api_response = api_instance.additional_properties_with_array_of_enums(
            body=body,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->additional_properties_with_array_of_enums: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AdditionalPropertiesWithArrayOfEnums**](AdditionalPropertiesWithArrayOfEnums.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | Got object with additional properties with array of enums 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AdditionalPropertiesWithArrayOfEnums**](AdditionalPropertiesWithArrayOfEnums.md) |  | 



[**AdditionalPropertiesWithArrayOfEnums**](AdditionalPropertiesWithArrayOfEnums.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **array_model**
> AnimalFarm array_model()



Test serialization of ArrayModel

### Example

```python
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
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only optional values
    body = AnimalFarm([
        Animal(),
    ])
    try:
        api_response = api_instance.array_model(
            body=body,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->array_model: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AnimalFarm**](AnimalFarm.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | Output model 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**AnimalFarm**](AnimalFarm.md) |  | 



[**AnimalFarm**](AnimalFarm.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **array_of_enums**
> ArrayOfEnums array_of_enums()

Array of Enums

### Example

```python
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
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only optional values
    body = ArrayOfEnums([
        StringEnum("placed"),
    ])
    try:
        # Array of Enums
        api_response = api_instance.array_of_enums(
            body=body,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->array_of_enums: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ArrayOfEnums**](ArrayOfEnums.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | Got named array of enums 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ArrayOfEnums**](ArrayOfEnums.md) |  | 



[**ArrayOfEnums**](ArrayOfEnums.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **body_with_file_schema**
> body_with_file_schema(file_schema_test_class)



For this test, the body for this request much reference a schema named `File`.

### Example

```python
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
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only required values which don't have defaults set
    body = FileSchemaTestClass(
        file=File(
            source_uri="source_uri_example",
        ),
        files=[
            File(
                source_uri="source_uri_example",
            ),
        ],
    )
    try:
        api_response = api_instance.body_with_file_schema(
            body=body,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->body_with_file_schema: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson] | required |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**FileSchemaTestClass**](FileSchemaTestClass.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | Success 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **body_with_query_params**
> body_with_query_params(queryuser)



### Example

```python
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
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only required values which don't have defaults set
    query_params = {
        'query': "query_example",
    }
    body = User(
        id=1,
        username="username_example",
        first_name="first_name_example",
        last_name="last_name_example",
        email="email_example",
        password="password_example",
        phone="phone_example",
        user_status=1,
        object_with_no_declared_props=dict(),
        object_with_no_declared_props_nullable=dict(),
        any_type_prop=None,
        any_type_prop_nullable=None,
    )
    try:
        api_response = api_instance.body_with_query_params(
            query_params=query_params,
            body=body,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->body_with_query_params: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson] | required |
query_params | RequestQueryParams | |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**User**](User.md) |  | 


### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query | QuerySchema | | 


#### QuerySchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | Success 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **boolean**
> bool boolean()



Test serialization of outer boolean types

### Example

```python
import petstore_api
from petstore_api.api import fake_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only optional values
    body = True
    try:
        api_response = api_instance.boolean(
            body=body,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->boolean: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | Output boolean 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | 


**bool**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **case_sensitive_params**
> case_sensitive_params(some_varsome_var2some_var3)



Ensures that original naming is used in endpoint params, that way we on't have collisions

### Example

```python
import petstore_api
from petstore_api.api import fake_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only required values which don't have defaults set
    query_params = {
        'someVar': "someVar_example",
        'SomeVar': "SomeVar_example",
        'some_var': "some_var_example",
    }
    try:
        api_response = api_instance.case_sensitive_params(
            query_params=query_params,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->case_sensitive_params: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query_params | RequestQueryParams | |
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
someVar | SomeVarSchema | | 
SomeVar | SomeVarSchema | | 
some_var | SomeVarSchema | | 


#### SomeVarSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### SomeVarSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### SomeVarSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | Success 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **client_model**
> Client client_model(client)

To test \"client\" model

To test \"client\" model

### Example

```python
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
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only required values which don't have defaults set
    body = Client(
        client="client_example",
    )
    try:
        # To test \"client\" model
        api_response = api_instance.client_model(
            body=body,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->client_model: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson] | required |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**Client**](Client.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | successful operation 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**Client**](Client.md) |  | 



[**Client**](Client.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **composed_one_of_different_types**
> ComposedOneOfDifferentTypes composed_one_of_different_types()



Test serialization of object with $refed properties

### Example

```python
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.composed_one_of_different_types import ComposedOneOfDifferentTypes
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only optional values
    body = ComposedOneOfDifferentTypes()
    try:
        api_response = api_instance.composed_one_of_different_types(
            body=body,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->composed_one_of_different_types: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ComposedOneOfDifferentTypes**](ComposedOneOfDifferentTypes.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | Output model 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ComposedOneOfDifferentTypes**](ComposedOneOfDifferentTypes.md) |  | 



[**ComposedOneOfDifferentTypes**](ComposedOneOfDifferentTypes.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **endpoint_parameters**
> endpoint_parameters()

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example

* Basic Authentication (http_basic_test):
```python
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

    # example passing only optional values
    body = dict(
        integer=10,
        int32=20,
        int64=1,
        number=32.1,
        _float=3.14,
        double=67.8,
        string="a",
        pattern_without_delimiter="AUR,rZ#UM/?R,Fp^l6$ARjbhJk C",
        byte='YQ==',
        binary=open('/path/to/file', 'rb'),
        date=isoparse('1970-01-01').date(),
        date_time=isoparse('2020-02-02T20:20:20.22222Z'),
        password="password_example",
        callback="callback_example",
    )
    try:
        # Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
        api_response = api_instance.endpoint_parameters(
            body=body,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->endpoint_parameters: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationXWwwFormUrlencoded, Unset] | optional, default is unset |
content_type | str | optional, default is 'application/x-www-form-urlencoded' | Selects the schema and serialization of the request body
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationXWwwFormUrlencoded

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**integer** | **int** | None | [optional] 
**int32** | **int** | None | [optional] 
**int64** | **int** | None | [optional] 
**number** | **int, float** | None | 
**float** | **int, float** | None | [optional] 
**double** | **int, float** | None | 
**string** | **str** | None | [optional] 
**pattern_without_delimiter** | **str** | None | 
**byte** | **str** | None | 
**binary** | **file_type** | None | [optional] 
**date** | **date** | None | [optional] 
**dateTime** | **datetime** | None | [optional]  if omitted the server will use the default value of isoparse('2010-02-01T10:20:10.11111+01:00')
**password** | **str** | None | [optional] 
**callback** | **str** | None | [optional] 
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
400 | ApiResponseFor400 | Invalid username supplied 
404 | ApiResponseFor404 | User not found 

#### ApiResponseFor400
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

#### ApiResponseFor404
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **enum_parameters**
> enum_parameters()

To test enum parameters

To test enum parameters

### Example

```python
import petstore_api
from petstore_api.api import fake_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only optional values
    query_params = {
        'enum_query_string_array': [
        "$",
    ],
        'enum_query_string': "-efg",
        'enum_query_integer': 1,
        'enum_query_double': 1.1,
    }
    header_params = {
        'enum_header_string_array': [
        "$",
    ],
        'enum_header_string': "-efg",
    }
    body = dict(
        enum_form_string_array=[
            "$",
        ],
        enum_form_string="-efg",
    )
    try:
        # To test enum parameters
        api_response = api_instance.enum_parameters(
            query_params=query_params,
            header_params=header_params,
            body=body,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->enum_parameters: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationXWwwFormUrlencoded, Unset] | optional, default is unset |
query_params | RequestQueryParams | |
header_params | RequestHeaderParams | |
content_type | str | optional, default is 'application/x-www-form-urlencoded' | Selects the schema and serialization of the request body
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationXWwwFormUrlencoded

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**enum_form_string_array** | **[str]** | Form parameter enum test (string array) | [optional] 
**enum_form_string** | **str** | Form parameter enum test (string) | [optional]  if omitted the server will use the default value of "-efg"
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
enum_query_string_array | EnumQueryStringArraySchema | | optional
enum_query_string | EnumQueryStringSchema | | optional
enum_query_integer | EnumQueryIntegerSchema | | optional
enum_query_double | EnumQueryDoubleSchema | | optional


#### EnumQueryStringArraySchema

Type | Description | Notes
------------- | ------------- | -------------
**[str]** |  | 

#### EnumQueryStringSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | defaults to "-efg",  must be one of ["_abc", "-efg", "(xyz)", ]

#### EnumQueryIntegerSchema

Type | Description | Notes
------------- | ------------- | -------------
**int** |  |  must be one of [1, -2, ]

#### EnumQueryDoubleSchema

Type | Description | Notes
------------- | ------------- | -------------
**int, float** |  |  must be one of [1.1, -1.2, ]

### header_params
#### RequestHeaderParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
enum_header_string_array | EnumHeaderStringArraySchema | | optional
enum_header_string | EnumHeaderStringSchema | | optional

#### EnumHeaderStringArraySchema

Type | Description | Notes
------------- | ------------- | -------------
**[str]** |  | 

#### EnumHeaderStringSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | defaults to "-efg",  must be one of ["_abc", "-efg", "(xyz)", ]

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
400 | ApiResponseFor400 | Invalid request 
404 | ApiResponseFor404 | Not found 

#### ApiResponseFor400
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |

#### ApiResponseFor404
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fake_health_get**
> HealthCheckResult fake_health_get()

Health check endpoint

### Example

```python
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
with petstore_api.ApiClient(configuration) as api_client:
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

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | The instance started successfully 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**HealthCheckResult**](HealthCheckResult.md) |  | 



[**HealthCheckResult**](HealthCheckResult.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **group_parameters**
> group_parameters(required_string_grouprequired_boolean_grouprequired_int64_group)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example

* Bearer (JWT) Authentication (bearer_test):
```python
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

    # example passing only required values which don't have defaults set
    query_params = {
        'required_string_group': 1,
        'required_int64_group': 1,
    }
    header_params = {
        'required_boolean_group': True,
    }
    try:
        # Fake endpoint to test group parameters (optional)
        api_response = api_instance.group_parameters(
            query_params=query_params,
            header_params=header_params,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->group_parameters: %s\n" % e)

    # example passing only optional values
    query_params = {
        'required_string_group': 1,
        'required_int64_group': 1,
        'string_group': 1,
        'int64_group': 1,
    }
    header_params = {
        'required_boolean_group': True,
        'boolean_group': True,
    }
    try:
        # Fake endpoint to test group parameters (optional)
        api_response = api_instance.group_parameters(
            query_params=query_params,
            header_params=header_params,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->group_parameters: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query_params | RequestQueryParams | |
header_params | RequestHeaderParams | |
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
required_string_group | RequiredStringGroupSchema | | 
required_int64_group | RequiredInt64GroupSchema | | 
string_group | StringGroupSchema | | optional
int64_group | Int64GroupSchema | | optional


#### RequiredStringGroupSchema

Type | Description | Notes
------------- | ------------- | -------------
**int** |  | 

#### RequiredInt64GroupSchema

Type | Description | Notes
------------- | ------------- | -------------
**int** |  | 

#### StringGroupSchema

Type | Description | Notes
------------- | ------------- | -------------
**int** |  | 

#### Int64GroupSchema

Type | Description | Notes
------------- | ------------- | -------------
**int** |  | 

### header_params
#### RequestHeaderParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
required_boolean_group | RequiredBooleanGroupSchema | | 
boolean_group | BooleanGroupSchema | | optional

#### RequiredBooleanGroupSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | 

#### BooleanGroupSchema

Type | Description | Notes
------------- | ------------- | -------------
**bool** |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
400 | ApiResponseFor400 | Someting wrong 

#### ApiResponseFor400
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

[bearer_test](../README.md#bearer_test)

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **inline_additional_properties**
> inline_additional_properties(request_body)

test inline additionalProperties

### Example

```python
import petstore_api
from petstore_api.api import fake_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only required values which don't have defaults set
    body = dict(
        "key": "key_example",
    )
    try:
        # test inline additionalProperties
        api_response = api_instance.inline_additional_properties(
            body=body,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->inline_additional_properties: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson] | required |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**any string name** | **str** | any string name can be used but the value must be the correct type | [optional]

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | successful operation 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **json_form_data**
> json_form_data()

test json serialization of form data

### Example

```python
import petstore_api
from petstore_api.api import fake_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only optional values
    body = dict(
        param="param_example",
        param2="param2_example",
    )
    try:
        # test json serialization of form data
        api_response = api_instance.json_form_data(
            body=body,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->json_form_data: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationXWwwFormUrlencoded, Unset] | optional, default is unset |
content_type | str | optional, default is 'application/x-www-form-urlencoded' | Selects the schema and serialization of the request body
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationXWwwFormUrlencoded

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**param** | **str** | field1 | 
**param2** | **str** | field2 | 
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | successful operation 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **mammal**
> Mammal mammal(mammal)



Test serialization of mammals

### Example

```python
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
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only required values which don't have defaults set
    body = Mammal(
        has_baleen=True,
        has_teeth=True,
        class_name="whale",
    )
    try:
        api_response = api_instance.mammal(
            body=body,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->mammal: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson] | required |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**Mammal**](Mammal.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | Output mammal 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**Mammal**](Mammal.md) |  | 



[**Mammal**](Mammal.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **number_with_validations**
> NumberWithValidations number_with_validations()



Test serialization of outer number types

### Example

```python
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
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only optional values
    body = NumberWithValidations(10)
    try:
        api_response = api_instance.number_with_validations(
            body=body,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->number_with_validations: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**NumberWithValidations**](NumberWithValidations.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | Output number 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**NumberWithValidations**](NumberWithValidations.md) |  | 



[**NumberWithValidations**](NumberWithValidations.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **object_model_with_ref_props**
> ObjectModelWithRefProps object_model_with_ref_props()



Test serialization of object with $refed properties

### Example

```python
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
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only optional values
    body = ObjectModelWithRefProps(
        my_number=NumberWithValidations(10),
        my_string="my_string_example",
        my_boolean=True,
    )
    try:
        api_response = api_instance.object_model_with_ref_props(
            body=body,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->object_model_with_ref_props: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ObjectModelWithRefProps**](ObjectModelWithRefProps.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | Output model 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ObjectModelWithRefProps**](ObjectModelWithRefProps.md) |  | 



[**ObjectModelWithRefProps**](ObjectModelWithRefProps.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **parameter_collisions**
> bool, date, datetime, dict, float, int, list, str, none_type parameter_collisions(_3a_b5ab2_self3a_b6)

parameter collision case

### Example

```python
import petstore_api
from petstore_api.api import fake_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only required values which don't have defaults set
    path_params = {
        '1': "1_example",
        'aB': "aB_example",
        'Ab': "Ab_example",
        'self': "self_example",
        'A-B': "A-B_example",
    }
    query_params = {
    }
    cookie_params = {
    }
    header_params = {
    }
    try:
        # parameter collision case
        api_response = api_instance.parameter_collisions(
            path_params=path_params,
            query_params=query_params,
            header_params=header_params,
            cookie_params=cookie_params,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->parameter_collisions: %s\n" % e)

    # example passing only optional values
    path_params = {
        '1': "1_example",
        'aB': "aB_example",
        'Ab': "Ab_example",
        'self': "self_example",
        'A-B': "A-B_example",
    }
    query_params = {
        '1': "1_example",
        'aB': "aB_example",
        'Ab': "Ab_example",
        'self': "self_example",
        'A-B': "A-B_example",
    }
    cookie_params = {
        '1': "1_example",
        'aB': "aB_example",
        'Ab': "Ab_example",
        'self': "self_example",
        'A-B': "A-B_example",
    }
    header_params = {
        '1': "1_example",
        'aB': "aB_example",
        'self': "self_example",
        'A-B': "A-B_example",
    }
    body = None
    try:
        # parameter collision case
        api_response = api_instance.parameter_collisions(
            path_params=path_params,
            query_params=query_params,
            header_params=header_params,
            cookie_params=cookie_params,
            body=body,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->parameter_collisions: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
query_params | RequestQueryParams | |
header_params | RequestHeaderParams | |
path_params | RequestPathParams | |
cookie_params | RequestCookieParams | |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
typing.Union[dict, frozendict, str, date, datetime, int, float, bool, Decimal, None, list, tuple, bytes] | |

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
1 | Model1Schema | | optional
aB | ABSchema | | optional
Ab | AbSchema | | optional
self | ModelSelfSchema | | optional
A-B | ABSchema | | optional


#### Model1Schema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ABSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### AbSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ModelSelfSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ABSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

### header_params
#### RequestHeaderParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
1 | Model1Schema | | optional
aB | ABSchema | | optional
self | ModelSelfSchema | | optional
A-B | ABSchema | | optional

#### Model1Schema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ABSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ModelSelfSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ABSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

### path_params
#### RequestPathParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
1 | Model1Schema | | 
aB | ABSchema | | 
Ab | AbSchema | | 
self | ModelSelfSchema | | 
A-B | ABSchema | | 

#### Model1Schema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ABSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### AbSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ModelSelfSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ABSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

### cookie_params
#### RequestCookieParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
1 | Model1Schema | | optional
aB | ABSchema | | optional
Ab | AbSchema | | optional
self | ModelSelfSchema | | optional
A-B | ABSchema | | optional

#### Model1Schema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ABSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### AbSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ModelSelfSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

#### ABSchema

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | success 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
typing.Union[dict, frozendict, str, date, datetime, int, float, bool, Decimal, None, list, tuple, bytes] | |


**bool, date, datetime, dict, float, int, list, str, none_type**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **query_parameter_collection_format**
> query_parameter_collection_format(pipeioutilhttpurlcontextref_param)



To test the collection format in query parameters

### Example

```python
import petstore_api
from petstore_api.api import fake_api
from petstore_api.model.string_with_validation import StringWithValidation
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only required values which don't have defaults set
    query_params = {
        'pipe': [
        "pipe_example",
    ],
        'ioutil': [
        "ioutil_example",
    ],
        'http': [
        "http_example",
    ],
        'url': [
        "url_example",
    ],
        'context': [
        "context_example",
    ],
        'refParam': StringWithValidation("refParam_example"),
    }
    try:
        api_response = api_instance.query_parameter_collection_format(
            query_params=query_params,
        )
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->query_parameter_collection_format: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
query_params | RequestQueryParams | |
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### query_params
#### RequestQueryParams

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
pipe | PipeSchema | | 
ioutil | IoutilSchema | | 
http | HttpSchema | | 
url | UrlSchema | | 
context | ContextSchema | | 
refParam | RefParamSchema | | 


#### PipeSchema

Type | Description | Notes
------------- | ------------- | -------------
**[str]** |  | 

#### IoutilSchema

Type | Description | Notes
------------- | ------------- | -------------
**[str]** |  | 

#### HttpSchema

Type | Description | Notes
------------- | ------------- | -------------
**[str]** |  | 

#### UrlSchema

Type | Description | Notes
------------- | ------------- | -------------
**[str]** |  | 

#### ContextSchema

Type | Description | Notes
------------- | ------------- | -------------
**[str]** |  | 

#### RefParamSchema
Type | Description  | Notes
------------- | ------------- | -------------
[**StringWithValidation**](StringWithValidation.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | Success 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | Unset | body was not defined |
headers | Unset | headers were not defined |


void (empty response body)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **string**
> str string()



Test serialization of outer string types

### Example

```python
import petstore_api
from petstore_api.api import fake_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only optional values
    body = "body_example"
    try:
        api_response = api_instance.string(
            body=body,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->string: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | Output string 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson

Type | Description | Notes
------------- | ------------- | -------------
**str** |  | 


**str**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **string_enum**
> StringEnum string_enum()



Test serialization of outer enum

### Example

```python
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
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only optional values
    body = StringEnum("placed")
    try:
        api_response = api_instance.string_enum(
            body=body,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->string_enum: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationJson, Unset] | optional, default is unset |
content_type | str | optional, default is 'application/json' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**StringEnum**](StringEnum.md) |  | 


### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | Output enum 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**StringEnum**](StringEnum.md) |  | 



[**StringEnum**](StringEnum.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **upload_download_file**
> file_type upload_download_file(body)

uploads a file and downloads a file using application/octet-stream

### Example

```python
import petstore_api
from petstore_api.api import fake_api
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only required values which don't have defaults set
    body = open('/path/to/file', 'rb')
    try:
        # uploads a file and downloads a file using application/octet-stream
        api_response = api_instance.upload_download_file(
            body=body,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->upload_download_file: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyApplicationOctetStream] | required |
content_type | str | optional, default is 'application/octet-stream' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/octet-stream', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyApplicationOctetStream

file to upload

Type | Description | Notes
------------- | ------------- | -------------
**file_type** | file to upload | 

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | successful operation 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationOctetStream, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationOctetStream

file to download

Type | Description | Notes
------------- | ------------- | -------------
**file_type** | file to download | 


**file_type**

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **upload_file**
> ApiResponse upload_file()

uploads a file using multipart/form-data

### Example

```python
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
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only optional values
    body = dict(
        additional_metadata="additional_metadata_example",
        file=open('/path/to/file', 'rb'),
    )
    try:
        # uploads a file using multipart/form-data
        api_response = api_instance.upload_file(
            body=body,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->upload_file: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyMultipartFormData, Unset] | optional, default is unset |
content_type | str | optional, default is 'multipart/form-data' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyMultipartFormData

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**additionalMetadata** | **str** | Additional data to pass to server | [optional] 
**file** | **file_type** | file to upload | 
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | successful operation 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ApiResponse**](ApiResponse.md) |  | 



[**ApiResponse**](ApiResponse.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **upload_files**
> ApiResponse upload_files()

uploads files using multipart/form-data

### Example

```python
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
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = fake_api.FakeApi(api_client)

    # example passing only optional values
    body = dict(
        files=[
            open('/path/to/file', 'rb'),
        ],
    )
    try:
        # uploads files using multipart/form-data
        api_response = api_instance.upload_files(
            body=body,
        )
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeApi->upload_files: %s\n" % e)
```
### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
body | typing.Union[SchemaForRequestBodyMultipartFormData, Unset] | optional, default is unset |
content_type | str | optional, default is 'multipart/form-data' | Selects the schema and serialization of the request body
accept_content_types | typing.Tuple[str] | default is ('application/json', ) | Tells the server the content type(s) that are accepted by the client
stream | bool | default is False | if True then the response.content will be streamed and loaded from a file like object. When downloading a file, set this to True to force the code to deserialize the content to a FileSchema file
timeout | typing.Optional[typing.Union[int, typing.Tuple]] | default is None | the timeout used by the rest client
skip_deserialization | bool | default is False | when True, headers and body will be unset and an instance of api_client.ApiResponseWithoutDeserialization will be returned

### body

#### SchemaForRequestBodyMultipartFormData

#### Properties
Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**files** | **[file_type]** |  | [optional] 
**any string name** | **bool, date, datetime, dict, float, int, list, str, none_type** | any string name can be used but the value must be the correct type | [optional]

### Return Types, Responses

Code | Class | Description
------------- | ------------- | -------------
n/a | api_client.ApiResponseWithoutDeserialization | When skip_deserialization is True this response is returned
200 | ApiResponseFor200 | successful operation 

#### ApiResponseFor200
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
response | urllib3.HTTPResponse | Raw response |
body | typing.Union[SchemaFor200ResponseBodyApplicationJson, ] |  |
headers | Unset | headers were not defined |

#### SchemaFor200ResponseBodyApplicationJson
Type | Description  | Notes
------------- | ------------- | -------------
[**ApiResponse**](ApiResponse.md) |  | 



[**ApiResponse**](ApiResponse.md)

### Authorization

No authorization required

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

