# petstore_api.AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**call_123_test_special_tags**](AnotherFakeApi.md#call_123_test_special_tags) | **PATCH** /another-fake/dummy | To test special tags


# **call_123_test_special_tags**
> client.Client call_123_test_special_tags(client_client)

To test special tags

To test special tags and operation ID starting with number

### Example

```python
from __future__ import print_function
import time
import petstore_api
from pprint import pprint

# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.AnotherFakeApi(api_client)
    client_client = petstore_api.Client() # client.Client | client model
    
    # example passing only required values which don't have defaults set
    try:
        # To test special tags
        api_response = api_instance.call_123_test_special_tags(client_client)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling AnotherFakeApi->call_123_test_special_tags: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client_client** | [**client.Client**](Client.md)| client model |

### Return type

[**client.Client**](Client.md)

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

