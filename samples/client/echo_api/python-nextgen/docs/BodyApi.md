# openapi_client.BodyApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_echo_body_pet**](BodyApi.md#test_echo_body_pet) | **POST** /echo/body/Pet | Test body parameter(s)
[**test_echo_body_pet_response_string**](BodyApi.md#test_echo_body_pet_response_string) | **POST** /echo/body/Pet/response_string | Test empty response body


# **test_echo_body_pet**
> Pet test_echo_body_pet(pet=pet)

Test body parameter(s)

Test body parameter(s)

### Example

```python
from __future__ import print_function
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
from __future__ import print_function
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

