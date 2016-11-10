# petstore_api.FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_client_model**](FakeApi.md#test_client_model) | **PATCH** /fake | To test \&quot;client\&quot; model
[**test_endpoint_parameters**](FakeApi.md#test_endpoint_parameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**test_enum_parameters**](FakeApi.md#test_enum_parameters) | **GET** /fake | To test enum parameters


# **test_client_model**
> Client test_client_model(body)

To test \"client\" model

### Example 
```python
from __future__ import print_statement
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint

# create an instance of the API class
api_instance = petstore_api.FakeApi()
body = petstore_api.Client() # Client | client model

try: 
    # To test \"client\" model
    api_response = api_instance.test_client_model(body)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling FakeApi->test_client_model: %s\n" % e)
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

# **test_endpoint_parameters**
> test_endpoint_parameters(number, double, pattern_without_delimiter, byte, integer=integer, int32=int32, int64=int64, float=float, string=string, binary=binary, date=date, date_time=date_time, password=password, param_callback=param_callback)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example 
```python
from __future__ import print_statement
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint

# Configure HTTP basic authorization: http_basic_test
petstore_api.configuration.username = 'YOUR_USERNAME'
petstore_api.configuration.password = 'YOUR_PASSWORD'

# create an instance of the API class
api_instance = petstore_api.FakeApi()
number = 3.4 # float | None
double = 1.2 # float | None
pattern_without_delimiter = 'pattern_without_delimiter_example' # str | None
byte = 'B' # str | None
integer = 56 # int | None (optional)
int32 = 56 # int | None (optional)
int64 = 789 # int | None (optional)
float = 3.4 # float | None (optional)
string = 'string_example' # str | None (optional)
binary = 'B' # str | None (optional)
date = '2013-10-20' # date | None (optional)
date_time = '2013-10-20T19:20:30+01:00' # datetime | None (optional)
password = 'password_example' # str | None (optional)
param_callback = 'param_callback_example' # str | None (optional)

try: 
    # Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
    api_instance.test_endpoint_parameters(number, double, pattern_without_delimiter, byte, integer=integer, int32=int32, int64=int64, float=float, string=string, binary=binary, date=date, date_time=date_time, password=password, param_callback=param_callback)
except ApiException as e:
    print("Exception when calling FakeApi->test_endpoint_parameters: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **float**| None | 
 **double** | **float**| None | 
 **pattern_without_delimiter** | **str**| None | 
 **byte** | **str**| None | 
 **integer** | **int**| None | [optional] 
 **int32** | **int**| None | [optional] 
 **int64** | **int**| None | [optional] 
 **float** | **float**| None | [optional] 
 **string** | **str**| None | [optional] 
 **binary** | **str**| None | [optional] 
 **date** | **date**| None | [optional] 
 **date_time** | **datetime**| None | [optional] 
 **password** | **str**| None | [optional] 
 **param_callback** | **str**| None | [optional] 

### Return type

void (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: application/xml; charset=utf-8, application/json; charset=utf-8

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **test_enum_parameters**
> test_enum_parameters(enum_form_string_array=enum_form_string_array, enum_form_string=enum_form_string, enum_header_string_array=enum_header_string_array, enum_header_string=enum_header_string, enum_query_string_array=enum_query_string_array, enum_query_string=enum_query_string, enum_query_integer=enum_query_integer, enum_query_double=enum_query_double)

To test enum parameters

### Example 
```python
from __future__ import print_statement
import time
import petstore_api
from petstore_api.rest import ApiException
from pprint import pprint

# create an instance of the API class
api_instance = petstore_api.FakeApi()
enum_form_string_array = ['enum_form_string_array_example'] # list[str] | Form parameter enum test (string array) (optional)
enum_form_string = '-efg' # str | Form parameter enum test (string) (optional) (default to -efg)
enum_header_string_array = ['enum_header_string_array_example'] # list[str] | Header parameter enum test (string array) (optional)
enum_header_string = '-efg' # str | Header parameter enum test (string) (optional) (default to -efg)
enum_query_string_array = ['enum_query_string_array_example'] # list[str] | Query parameter enum test (string array) (optional)
enum_query_string = '-efg' # str | Query parameter enum test (string) (optional) (default to -efg)
enum_query_integer = 3.4 # float | Query parameter enum test (double) (optional)
enum_query_double = 1.2 # float | Query parameter enum test (double) (optional)

try: 
    # To test enum parameters
    api_instance.test_enum_parameters(enum_form_string_array=enum_form_string_array, enum_form_string=enum_form_string, enum_header_string_array=enum_header_string_array, enum_header_string=enum_header_string, enum_query_string_array=enum_query_string_array, enum_query_string=enum_query_string, enum_query_integer=enum_query_integer, enum_query_double=enum_query_double)
except ApiException as e:
    print("Exception when calling FakeApi->test_enum_parameters: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_form_string_array** | [**list[str]**](str.md)| Form parameter enum test (string array) | [optional] 
 **enum_form_string** | **str**| Form parameter enum test (string) | [optional] [default to -efg]
 **enum_header_string_array** | [**list[str]**](str.md)| Header parameter enum test (string array) | [optional] 
 **enum_header_string** | **str**| Header parameter enum test (string) | [optional] [default to -efg]
 **enum_query_string_array** | [**list[str]**](str.md)| Query parameter enum test (string array) | [optional] 
 **enum_query_string** | **str**| Query parameter enum test (string) | [optional] [default to -efg]
 **enum_query_integer** | **float**| Query parameter enum test (double) | [optional] 
 **enum_query_double** | **float**| Query parameter enum test (double) | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

