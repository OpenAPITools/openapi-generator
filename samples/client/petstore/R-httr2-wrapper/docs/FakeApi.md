# FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fake_data_file**](FakeApi.md#fake_data_file) | **GET** /fake/data_file | test data_file to ensure it&#39;s escaped correctly
[**fake_regular_expression**](FakeApi.md#fake_regular_expression) | **GET** /fake/regular_expression | test regular expression to ensure no exception
[**fake_set_query**](FakeApi.md#fake_set_query) | **GET** /fake/set_query_parameter | test set query parameter


# **fake_data_file**
> User fake_data_file(dummy, var_data_file = var.var_data_file)

test data_file to ensure it's escaped correctly



### Example
```R
library(petstore)

# test data_file to ensure it's escaped correctly
#
# prepare function argument(s)
var_dummy <- "dummy_example" # character | dummy required parameter
var_var_data_file <- "var_data_file_example" # character | header data file (Optional)

api_instance <- petstore_api$new()
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$fake_api$fake_data_file(var_dummy, var_data_file = var_var_data_file, data_file = "result.txt"),
             api_instance$fake_api$fake_data_file(var_dummy, var_data_file = var_var_data_file),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `fake_data_file`:")
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

# **fake_regular_expression**
> fake_regular_expression(reg_exp_test)

test regular expression to ensure no exception



### Example
```R
library(petstore)

# test regular expression to ensure no exception
#
# prepare function argument(s)
var_reg_exp_test <- "reg_exp_test_example" # character | dummy required parameter

api_instance <- petstore_api$new()
result <- tryCatch(
             api_instance$fake_api$fake_regular_expression(var_reg_exp_test),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `fake_regular_expression`:")
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

# **fake_set_query**
> fake_set_query(set_dummy, array_dummy)

test set query parameter



### Example
```R
library(petstore)

# test set query parameter
#
# prepare function argument(s)
var_set_dummy <- list("inner_example") # set[character] | set query
var_array_dummy <- list("inner_example") # array[character] | array query

api_instance <- petstore_api$new()
result <- tryCatch(
             api_instance$fake_api$fake_set_query(var_set_dummy, var_array_dummy),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `fake_set_query`:")
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

