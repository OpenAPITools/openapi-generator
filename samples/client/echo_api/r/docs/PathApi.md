# PathApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**](PathApi.md#TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath) | **GET** /path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path} | Test path parameter(s)


# **TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath**
> character TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(path_string, path_integer, enum_nonref_string_path, enum_ref_string_path)

Test path parameter(s)

Test path parameter(s)

### Example
```R
library(openapi)

# Test path parameter(s)
#
# prepare function argument(s)
var_path_string <- "path_string_example" # character | 
var_path_integer <- 56 # integer | 
var_enum_nonref_string_path <- "enum_nonref_string_path_example" # character | 
var_enum_ref_string_path <- StringEnumRef$new() # StringEnumRef | 

api_instance <- PathApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(var_path_string, var_path_integer, var_enum_nonref_string_path, var_enum_ref_string_pathdata_file = "result.txt")
result <- api_instance$TestsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath(var_path_string, var_path_integer, var_enum_nonref_string_path, var_enum_ref_string_path)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **path_string** | **character**|  | 
 **path_integer** | **integer**|  | 
 **enum_nonref_string_path** | Enum [success, failure, unclassified] |  | 
 **enum_ref_string_path** | [**StringEnumRef**](.md)|  | 

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

