# one_of.DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**root_get**](DefaultApi.md#root_get) | **GET** / | 


# **root_get**
> Fruit root_get()



### Example

```python
import time
import one_of
from one_of.api import default_api
from one_of.model.fruit import Fruit
from pprint import pprint
# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = one_of.Configuration(
    host = "http://localhost"
)


# Enter a context with an instance of the API client
with one_of.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = default_api.DefaultApi(api_client)

    # example, this endpoint has no required or optional parameters
    try:
        api_response = api_instance.root_get()
        pprint(api_response)
    except one_of.ApiException as e:
        print("Exception when calling DefaultApi->root_get: %s\n" % e)
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**Fruit**](Fruit.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | desc |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

