# FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**FakeDataFile**](FakeApi.md#FakeDataFile) | **GET** /fake/data_file | test data_file to ensure it&#39;s escaped correctly
[**FakePathArray**](FakeApi.md#FakePathArray) | **GET** /fake/path_array/{path_array}/testing | test array parameter in path
[**FakeRegularExpression**](FakeApi.md#FakeRegularExpression) | **GET** /fake/regular_expression | test regular expression to ensure no exception
[**FakeSetQuery**](FakeApi.md#FakeSetQuery) | **GET** /fake/set_query_parameter | test set query parameter


# **FakeDataFile**
> User FakeDataFile(dummy, var_data_file = var.var_data_file)

test data_file to ensure it's escaped correctly



### Example
```R
library(petstore)

# test data_file to ensure it's escaped correctly
#
# prepare function argument(s)
var_dummy <- "dummy_example" # character | dummy required parameter
var_var_data_file <- "var_data_file_example" # character | header data file (Optional)

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
  dput(result$ApiException$error_object$toJSONString())
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

# **FakePathArray**
> FakePathArray(path_array)

test array parameter in path



### Example
```R
library(petstore)

# test array parameter in path
#
# prepare function argument(s)
var_path_array <- list("inner_example") # array[character] | dummy path parameter

api_instance <- FakeApi$new()
result <- tryCatch(
             api_instance$FakePathArray(var_path_array),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `FakePathArray`:")
  dput(result$ApiException$toString())
  # error object
  dput(result$ApiException$error_object$toJSONString())
}
# This endpoint doesn't return data
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **path_array** | list( **character** )| dummy path parameter | 

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

# **FakeRegularExpression**
> FakeRegularExpression(reg_exp_test)

test regular expression to ensure no exception



### Example
```R
library(petstore)

# test regular expression to ensure no exception
#
# prepare function argument(s)
var_reg_exp_test <- "reg_exp_test_example" # character | dummy required parameter

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
  dput(result$ApiException$error_object$toJSONString())
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

# **FakeSetQuery**
> FakeSetQuery(set_dummy, array_dummy)

test set query parameter



### Example
```R
library(petstore)

# test set query parameter
#
# prepare function argument(s)
var_set_dummy <- list("inner_example") # set[character] | set query
var_array_dummy <- list("inner_example") # array[character] | array query

api_instance <- FakeApi$new()
result <- tryCatch(
             api_instance$FakeSetQuery(var_set_dummy, var_array_dummy),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `FakeSetQuery`:")
  dput(result$ApiException$toString())
  # error object
  dput(result$ApiException$error_object$toJSONString())
}
# This endpoint doesn't return data
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **set_dummy** | list( **character** )| set query | 
 **array_dummy** | list( **character** )| array query | 

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

