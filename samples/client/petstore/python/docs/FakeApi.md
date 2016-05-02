# swagger_client.FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters


# **test_endpoint_parameters**
> test_endpoint_parameters(number, double, string, byte, integer=integer, int32=int32, int64=int64, float=float, binary=binary, date=date, date_time=date_time, password=password)

Fake endpoint for testing various parameters

Fake endpoint for testing various parameters

### Example 
```python
import time
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint

# create an instance of the API class
api_instance = swagger_client.FakeApi()
number = 3.4 # float | None
double = 1.2 # float | None
string = 'string_example' # str | None
byte = 'B' # str | None
integer = 56 # int | None (optional)
int32 = 56 # int | None (optional)
int64 = 789 # int | None (optional)
float = 3.4 # float | None (optional)
binary = 'B' # str | None (optional)
date = '2013-10-20' # date | None (optional)
date_time = '2013-10-20T19:20:30+01:00' # datetime | None (optional)
password = 'password_example' # str | None (optional)

try: 
    # Fake endpoint for testing various parameters
    api_instance.test_endpoint_parameters(number, double, string, byte, integer=integer, int32=int32, int64=int64, float=float, binary=binary, date=date, date_time=date_time, password=password)
except ApiException as e:
    print "Exception when calling FakeApi->test_endpoint_parameters: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **float**| None | 
 **double** | **float**| None | 
 **string** | **str**| None | 
 **byte** | **str**| None | 
 **integer** | **int**| None | [optional] 
 **int32** | **int**| None | [optional] 
 **int64** | **int**| None | [optional] 
 **float** | **float**| None | [optional] 
 **binary** | **str**| None | [optional] 
 **date** | **date**| None | [optional] 
 **date_time** | **datetime**| None | [optional] 
 **password** | **str**| None | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

