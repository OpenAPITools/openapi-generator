# FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**FakeDataFile**](FakeApi.md#FakeDataFile) | **GET** /fake/data_file | test data_file to ensure it&#39;s escaped correctly


# **FakeDataFile**
> User FakeDataFile(dummy, var_data_file = var.var_data_file)

test data_file to ensure it's escaped correctly



### Example
```R
library(petstore)

var_dummy <- 'dummy_example' # character | dummy required parameter
var_var_data_file <- 'var_data_file_example' # character | header data file

#test data_file to ensure it's escaped correctly
api_instance <- FakeApi$new()
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$FakeDataFile(var_dummy, var_data_file = var_var_data_file, data_file = "result.txt"),
             api_instance$FakeDataFile(var_dummy, var_data_file = var_var_data_file),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  cat(result$ApiException$toString())
} else {
  # deserialized response object
  response.object <- result$content
  # response headers
  response.headers <- result$response$headers
  # response status code
  response.status.code <- result$response$status_code
}
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

