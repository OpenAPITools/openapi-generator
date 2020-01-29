# petstore_api.FakeClassnameTags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_classname**](FakeClassnameTags123Api.md#test_classname) | **PATCH** /fake_classname_test | To test class name in snake case


# **test_classname**
> client.Client test_classname(body)

To test class name in snake case

To test class name in snake case

### Example

* Api Key Authentication (api_key_query):
```python
from __future__ import print_function
import time
import petstore_api
from pprint import pprint
configuration = petstore_api.Configuration()
# Configure API key authorization: api_key_query
configuration.api_key['api_key_query'] = 'YOUR_API_KEY'
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
# configuration.api_key_prefix['api_key_query'] = 'Bearer'

# Defining host is optional and default to http://petstore.swagger.io:80/v2
configuration.host = "http://petstore.swagger.io:80/v2"
# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.FakeClassnameTags123Api(api_client)
    body = petstore_api.Client() # client.Client | client model
    
    # example passing only required values which don't have defaults set
    try:
        # To test class name in snake case
        api_response = api_instance.test_classname(body)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FakeClassnameTags123Api->test_classname: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**client.Client**](Client.md)| client model |

### Return type

[**client.Client**](Client.md)

### Authorization

[api_key_query](../README.md#api_key_query)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

