# HeaderApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestHeaderIntegerBooleanStringEnums**](HeaderApi.md#TestHeaderIntegerBooleanStringEnums) | **GET** /header/integer/boolean/string/enums | Test header parameter(s)


# **TestHeaderIntegerBooleanStringEnums**
> character TestHeaderIntegerBooleanStringEnums(integer_header = var.integer_header, boolean_header = var.boolean_header, string_header = var.string_header, enum_nonref_string_header = var.enum_nonref_string_header, enum_ref_string_header = var.enum_ref_string_header)

Test header parameter(s)

Test header parameter(s)

### Example
```R
library(openapi)

# Test header parameter(s)
#
# prepare function argument(s)
var_integer_header <- 56 # integer |  (Optional)
var_boolean_header <- "boolean_header_example" # character |  (Optional)
var_string_header <- "string_header_example" # character |  (Optional)
var_enum_nonref_string_header <- "enum_nonref_string_header_example" # character |  (Optional)
var_enum_ref_string_header <- StringEnumRef$new() # StringEnumRef |  (Optional)

api_instance <- HeaderApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestHeaderIntegerBooleanStringEnums(integer_header = var_integer_header, boolean_header = var_boolean_header, string_header = var_string_header, enum_nonref_string_header = var_enum_nonref_string_header, enum_ref_string_header = var_enum_ref_string_headerdata_file = "result.txt")
result <- api_instance$TestHeaderIntegerBooleanStringEnums(integer_header = var_integer_header, boolean_header = var_boolean_header, string_header = var_string_header, enum_nonref_string_header = var_enum_nonref_string_header, enum_ref_string_header = var_enum_ref_string_header)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **integer_header** | **integer**|  | [optional] 
 **boolean_header** | **character**|  | [optional] 
 **string_header** | **character**|  | [optional] 
 **enum_nonref_string_header** | Enum [success, failure, unclassified] |  | [optional] 
 **enum_ref_string_header** | [**StringEnumRef**](.md)|  | [optional] 

### Return type

**character**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

