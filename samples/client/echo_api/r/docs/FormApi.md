# FormApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestFormIntegerBooleanString**](FormApi.md#TestFormIntegerBooleanString) | **POST** /form/integer/boolean/string | Test form parameter(s)
[**TestFormObjectMultipart**](FormApi.md#TestFormObjectMultipart) | **POST** /form/object/multipart | Test form parameter(s) for multipart schema
[**TestFormOneof**](FormApi.md#TestFormOneof) | **POST** /form/oneof | Test form parameter(s) for oneOf schema


# **TestFormIntegerBooleanString**
> character TestFormIntegerBooleanString(integer_form = var.integer_form, boolean_form = var.boolean_form, string_form = var.string_form)

Test form parameter(s)

Test form parameter(s)

### Example
```R
library(openapi)

# Test form parameter(s)
#
# prepare function argument(s)
var_integer_form <- 56 # integer |  (Optional)
var_boolean_form <- "boolean_form_example" # character |  (Optional)
var_string_form <- "string_form_example" # character |  (Optional)

api_instance <- FormApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestFormIntegerBooleanString(integer_form = var_integer_form, boolean_form = var_boolean_form, string_form = var_string_formdata_file = "result.txt")
result <- api_instance$TestFormIntegerBooleanString(integer_form = var_integer_form, boolean_form = var_boolean_form, string_form = var_string_form)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **integer_form** | **integer**|  | [optional] 
 **boolean_form** | **character**|  | [optional] 
 **string_form** | **character**|  | [optional] 

### Return type

**character**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

# **TestFormObjectMultipart**
> character TestFormObjectMultipart(marker)

Test form parameter(s) for multipart schema

Test form parameter(s) for multipart schema

### Example
```R
library(openapi)

# Test form parameter(s) for multipart schema
#
# prepare function argument(s)
var_marker <- test_form_object_multipart_request_marker$new("name_example") # TestFormObjectMultipartRequestMarker | 

api_instance <- FormApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestFormObjectMultipart(var_markerdata_file = "result.txt")
result <- api_instance$TestFormObjectMultipart(var_marker)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **marker** | [**TestFormObjectMultipartRequestMarker**](test_form_object_multipart_request_marker.md)|  | 

### Return type

**character**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

# **TestFormOneof**
> character TestFormOneof(form1 = var.form1, form2 = var.form2, form3 = var.form3, form4 = var.form4, id = var.id, name = var.name)

Test form parameter(s) for oneOf schema

Test form parameter(s) for oneOf schema

### Example
```R
library(openapi)

# Test form parameter(s) for oneOf schema
#
# prepare function argument(s)
var_form1 <- "form1_example" # character |  (Optional)
var_form2 <- 56 # integer |  (Optional)
var_form3 <- "form3_example" # character |  (Optional)
var_form4 <- "form4_example" # character |  (Optional)
var_id <- 56 # integer |  (Optional)
var_name <- "name_example" # character |  (Optional)

api_instance <- FormApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestFormOneof(form1 = var_form1, form2 = var_form2, form3 = var_form3, form4 = var_form4, id = var_id, name = var_namedata_file = "result.txt")
result <- api_instance$TestFormOneof(form1 = var_form1, form2 = var_form2, form3 = var_form3, form4 = var_form4, id = var_id, name = var_name)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **form1** | **character**|  | [optional] 
 **form2** | **integer**|  | [optional] 
 **form3** | **character**|  | [optional] 
 **form4** | **character**|  | [optional] 
 **id** | **integer**|  | [optional] 
 **name** | **character**|  | [optional] 

### Return type

**character**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

