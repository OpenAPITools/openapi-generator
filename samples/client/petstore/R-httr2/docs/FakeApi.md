# FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**add_pet_optional**](FakeApi.md#add_pet_optional) | **POST** /fake/test_optional_body_parameter | Add a new pet to the store (optional body)
[**fake_data_file**](FakeApi.md#fake_data_file) | **GET** /fake/data_file | test data_file to ensure it&#39;s escaped correctly
[**fake_path_array**](FakeApi.md#fake_path_array) | **GET** /fake/path_array/{path_array}/testing | test array parameter in path
[**fake_regular_expression**](FakeApi.md#fake_regular_expression) | **GET** /fake/regular_expression | test regular expression to ensure no exception
[**fake_set_query**](FakeApi.md#fake_set_query) | **GET** /fake/set_query_parameter | test set query parameter


# **add_pet_optional**
> Pet add_pet_optional(pet = var.pet)

Add a new pet to the store (optional body)



### Example
```R
library(petstore)

# Add a new pet to the store (optional body)
#
# prepare function argument(s)
var_pet <- Pet$new("name_example", c("photoUrls_example"), 123, Category$new(123, "name_example"), c(Tag$new(123, "name_example")), "available") # Pet | Pet object that needs to be added to the store (Optional)

api_instance <- FakeApi$new()
# Configure HTTP basic authorization: http_auth
api_instance$api_client$username <- Sys.getenv("USERNAME")
api_instance$api_client$password <- Sys.getenv("PASSWORD")
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$add_pet_optional(pet = var_pet, data_file = "result.txt"),
             api_instance$add_pet_optional(pet = var_pet),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `add_pet_optional`:")
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
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

[**Pet**](Pet.md)

### Authorization

[http_auth](../README.md#http_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml, multipart/related
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **405** | Invalid input |  -  |

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

api_instance <- FakeApi$new()
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$fake_data_file(var_dummy, var_data_file = var_var_data_file, data_file = "result.txt"),
             api_instance$fake_data_file(var_dummy, var_data_file = var_var_data_file),
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

# **fake_path_array**
> fake_path_array(path_array)

test array parameter in path



### Example
```R
library(petstore)

# test array parameter in path
#
# prepare function argument(s)
var_path_array <- c("inner_example") # array[character] | dummy path parameter

api_instance <- FakeApi$new()
result <- tryCatch(
             api_instance$fake_path_array(var_path_array),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `fake_path_array`:")
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

api_instance <- FakeApi$new()
result <- tryCatch(
             api_instance$fake_regular_expression(var_reg_exp_test),
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
var_set_dummy <- c("inner_example") # set[character] | set query
var_array_dummy <- c("inner_example") # array[character] | array query

api_instance <- FakeApi$new()
result <- tryCatch(
             api_instance$fake_set_query(var_set_dummy, var_array_dummy),
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

