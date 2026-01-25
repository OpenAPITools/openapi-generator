# petstore_api.ImportTestDatetimeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**import_test_return_datetime**](ImportTestDatetimeApi.md#import_test_return_datetime) | **GET** /import_test/return_datetime | test date time


# **import_test_return_datetime**
> datetime import_test_return_datetime()

test date time

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
    api_instance = petstore_api.ImportTestDatetimeApi(api_client)

    try:
        # test date time
        api_response = api_instance.import_test_return_datetime()
        print("The response of ImportTestDatetimeApi->import_test_return_datetime:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling ImportTestDatetimeApi->import_test_return_datetime: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

**datetime**

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

