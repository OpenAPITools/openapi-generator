# FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**FakeDataFile**](FakeApi.md#FakeDataFile) | **GET** /fake/data_file | test data_file to ensure it&#39;s escaped correctly
[**FakeRegularExpression**](FakeApi.md#FakeRegularExpression) | **GET** /fake/regular_expression | test regular expression to ensure no exception


# **FakeDataFile**
> User FakeDataFile(dummy, var_data_file = var.var_data_file)

test data_file to ensure it's escaped correctly



### Example
```R
library(petstore)

var_dummy <- "dummy_example" # character | dummy required parameter
var_var_data_file <- "var_data_file_example" # character | header data file (Optional)

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
  print("Exception occurs when calling `FakeDataFile`:")
  dput(result$ApiException$toString())
  # error object
  dput(result$ApiException$error_object)
} else {
  # deserialized response object
  print("The response is ...")
  dput(result$toString())
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

# **FakeRegularExpression**
> FakeRegularExpression(reg_exp_test)

test regular expression to ensure no exception



### Example
```R
library(petstore)

var_reg_exp_test <- "reg_exp_test_example" # character | dummy required parameter

#test regular expression to ensure no exception
api_instance <- FakeApi$new()
result <- tryCatch(
             api_instance$FakeRegularExpression(var_reg_exp_test),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `FakeRegularExpression`:")
  dput(result$ApiException$toString())
  # error object
  dput(result$ApiException$error_object)
}
# This endpoint doesn't return data
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **reg_exp_test** | **character**| dummy required parameter | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

