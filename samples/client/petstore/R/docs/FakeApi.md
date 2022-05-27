# FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**FakeDataFile**](FakeApi.md#FakeDataFile) | **GET** /fake/data_file | test data_file to ensure it&#39;s escaped correctly


# **FakeDataFile**
> User FakeDataFile(dummy, var_data_file=var.var_data_file)

test data_file to ensure it's escaped correctly



### Example
```R
library(petstore)

var.dummy <- 'dummy_example' # character | dummy required parameter
var.var_data_file <- 'var_data_file_example' # character | header data file

#test data_file to ensure it's escaped correctly
api.instance <- FakeApi$new()
result <- api.instance$FakeDataFile(var.dummy, var_data_file=var.var_data_file)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **dummy** | **character**| dummy required parameter | 
 **var_data_file** | **character**| header data file | [optional] 

### Return type

[**User**](User.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

