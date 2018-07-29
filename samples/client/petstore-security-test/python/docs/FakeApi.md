# petstore_api.FakeApi

All URIs are relative to *petstore.swagger.io */ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r/v2 */ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_code_inject____end__rn_n_r**](FakeApi.md#test_code_inject____end__rn_n_r) | **PUT** /fake | To test code injection */ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r


# **test_code_inject____end__rn_n_r**
> test_code_inject____end__rn_n_r(unknown_base_type=unknown_base_type)

To test code injection */ ' \" =end -- \\r\\n \\n \\r

### Example
```python
from __future__ import print_function
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint

# create an instance of the API class
api_instance = petstore_api.FakeApi()
unknown_base_type = petstore_api.UNKNOWN_BASE_TYPE() # object |  (optional)

try:
    # To test code injection */ ' \" =end -- \\r\\n \\n \\r
    api_instance.test_code_inject____end__rn_n_r(unknown_base_type=unknown_base_type)
except ApiException as e:
    print("Exception when calling FakeApi->test_code_inject____end__rn_n_r: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **unknown_base_type** | [**object**](UNKNOWN_BASE_TYPE.md)|  | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, */  \" =end --       
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

