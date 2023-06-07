# openapi_client.BodyApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_binary_gif**](BodyApi.md#test_binary_gif) | **POST** /binary/gif | Test binary (gif) response body
[**test_body_application_octetstream_binary**](BodyApi.md#test_body_application_octetstream_binary) | **POST** /body/application/octetstream/binary | Test body parameter(s)
[**test_echo_body_free_form_object_response_string**](BodyApi.md#test_echo_body_free_form_object_response_string) | **POST** /echo/body/FreeFormObject/response_string | Test free form object
[**test_echo_body_pet**](BodyApi.md#test_echo_body_pet) | **POST** /echo/body/Pet | Test body parameter(s)
[**test_echo_body_pet_response_string**](BodyApi.md#test_echo_body_pet_response_string) | **POST** /echo/body/Pet/response_string | Test empty response body
[**test_echo_body_tag_response_string**](BodyApi.md#test_echo_body_tag_response_string) | **POST** /echo/body/Tag/response_string | Test empty json (request body)


# **test_binary_gif**
> bytearray test_binary_gif()

Test binary (gif) response body

Test binary (gif) response body

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
    api_instance = openapi_client.BodyApi(api_client)

    try:
        # Test binary (gif) response body
        api_response = api_instance.test_binary_gif()
        print("The response of BodyApi->test_binary_gif:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling BodyApi->test_binary_gif: %s\n" % e)
```


### Parameters
This endpoint does not need any parameter.

### Return type

**bytearray**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: image/gif

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_body_application_octetstream_binary**
> str test_body_application_octetstream_binary(body=body)

Test body parameter(s)

Test body parameter(s)

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
    api_instance = openapi_client.BodyApi(api_client)
    body = None # bytearray |  (optional)

    try:
        # Test body parameter(s)
        api_response = api_instance.test_body_application_octetstream_binary(body=body)
        print("The response of BodyApi->test_body_application_octetstream_binary:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling BodyApi->test_body_application_octetstream_binary: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **bytearray**|  | [optional] 

### Return type

**str**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/octet-stream
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_echo_body_free_form_object_response_string**
> str test_echo_body_free_form_object_response_string(body=body)

Test free form object

Test free form object

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
    api_instance = openapi_client.BodyApi(api_client)
    body = None # object | Free form object (optional)

    try:
        # Test free form object
        api_response = api_instance.test_echo_body_free_form_object_response_string(body=body)
        print("The response of BodyApi->test_echo_body_free_form_object_response_string:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling BodyApi->test_echo_body_free_form_object_response_string: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **object**| Free form object | [optional] 

### Return type

**str**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_echo_body_pet**
> Pet test_echo_body_pet(pet=pet)

Test body parameter(s)

Test body parameter(s)

### Example

```python
import time
import os
import openapi_client
from openapi_client.models.pet import Pet
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
    api_instance = openapi_client.BodyApi(api_client)
    pet = openapi_client.Pet() # Pet | Pet object that needs to be added to the store (optional)

    try:
        # Test body parameter(s)
        api_response = api_instance.test_echo_body_pet(pet=pet)
        print("The response of BodyApi->test_echo_body_pet:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling BodyApi->test_echo_body_pet: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

[**Pet**](Pet.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_echo_body_pet_response_string**
> str test_echo_body_pet_response_string(pet=pet)

Test empty response body

Test empty response body

### Example

```python
import time
import os
import openapi_client
from openapi_client.models.pet import Pet
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
    api_instance = openapi_client.BodyApi(api_client)
    pet = openapi_client.Pet() # Pet | Pet object that needs to be added to the store (optional)

    try:
        # Test empty response body
        api_response = api_instance.test_echo_body_pet_response_string(pet=pet)
        print("The response of BodyApi->test_echo_body_pet_response_string:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling BodyApi->test_echo_body_pet_response_string: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

**str**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_echo_body_tag_response_string**
> str test_echo_body_tag_response_string(tag=tag)

Test empty json (request body)

Test empty json (request body)

### Example

```python
import time
import os
import openapi_client
from openapi_client.models.tag import Tag
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
    api_instance = openapi_client.BodyApi(api_client)
    tag = openapi_client.Tag() # Tag | Tag object (optional)

    try:
        # Test empty json (request body)
        api_response = api_instance.test_echo_body_tag_response_string(tag=tag)
        print("The response of BodyApi->test_echo_body_tag_response_string:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling BodyApi->test_echo_body_tag_response_string: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tag** | [**Tag**](Tag.md)| Tag object | [optional] 

### Return type

**str**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

