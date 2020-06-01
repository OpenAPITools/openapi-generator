# petstore_api.FarmApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**farm**](FarmApi.md#farm) | **GET** /farm/animals | Animal Farm


# **farm**
> animal_farm.AnimalFarm farm()

Animal Farm

### Example

```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.api import farm_api
from petstore_api.model import animal_farm
from pprint import pprint
# Defining the host is optional and defaults to http://petstore.swagger.io:80/v2
# See configuration.py for a list of all supported configuration parameters.
configuration = petstore_api.Configuration(
    host = "http://petstore.swagger.io:80/v2"
)


# Enter a context with an instance of the API client
with petstore_api.ApiClient() as api_client:
    # Create an instance of the API class
    api_instance = farm_api.FarmApi(api_client)
    
    # example, this endpoint has no required or optional parameters
    try:
        # Animal Farm
        api_response = api_instance.farm()
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling FarmApi->farm: %s\n" % e)
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**animal_farm.AnimalFarm**](AnimalFarm.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Got named array of enums |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

