# petstore_api.AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_special_tags**](AnotherFakeApi.md#test_special_tags) | **PATCH** /another-fake/dummy | To test special tags


# **test_special_tags**
> Client test_special_tags(body)

To test special tags

To test special tags

### Example 
```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint

# create an instance of the API class
api_instance = petstore_api.AnotherFakeApi()
body = petstore_api.Client() # Client | client model

try: 
    # To test special tags
    api_response = api_instance.test_special_tags(body)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling AnotherFakeApi->test_special_tags: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md)| client model | 

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

