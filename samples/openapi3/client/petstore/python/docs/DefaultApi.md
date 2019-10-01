# petstore_api.DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**foo_get**](DefaultApi.md#foo_get) | **GET** /foo | 


# **foo_get**
> InlineResponseDefault foo_get()



### Example

```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint

# Create an instance of the API class
api_instance = petstore_api.DefaultApi()

try:
    api_response = api_instance.foo_get()
    pprint(api_response)
except ApiException as e:
    print("Exception when calling DefaultApi->foo_get: %s\n" % e)
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**InlineResponseDefault**](InlineResponseDefault.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**0** | response |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

