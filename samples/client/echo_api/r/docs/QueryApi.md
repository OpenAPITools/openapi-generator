# QueryApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestEnumRefString**](QueryApi.md#TestEnumRefString) | **GET** /query/enum_ref_string | Test query parameter(s)
[**TestQueryDatetimeDateString**](QueryApi.md#TestQueryDatetimeDateString) | **GET** /query/datetime/date/string | Test query parameter(s)
[**TestQueryIntegerBooleanString**](QueryApi.md#TestQueryIntegerBooleanString) | **GET** /query/integer/boolean/string | Test query parameter(s)
[**TestQueryStyleDeepObjectExplodeTrueObject**](QueryApi.md#TestQueryStyleDeepObjectExplodeTrueObject) | **GET** /query/style_deepObject/explode_true/object | Test query parameter(s)
[**TestQueryStyleDeepObjectExplodeTrueObjectAllOf**](QueryApi.md#TestQueryStyleDeepObjectExplodeTrueObjectAllOf) | **GET** /query/style_deepObject/explode_true/object/allOf | Test query parameter(s)
[**TestQueryStyleFormExplodeFalseArrayInteger**](QueryApi.md#TestQueryStyleFormExplodeFalseArrayInteger) | **GET** /query/style_form/explode_false/array_integer | Test query parameter(s)
[**TestQueryStyleFormExplodeFalseArrayString**](QueryApi.md#TestQueryStyleFormExplodeFalseArrayString) | **GET** /query/style_form/explode_false/array_string | Test query parameter(s)
[**TestQueryStyleFormExplodeTrueArrayString**](QueryApi.md#TestQueryStyleFormExplodeTrueArrayString) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s)
[**TestQueryStyleFormExplodeTrueObject**](QueryApi.md#TestQueryStyleFormExplodeTrueObject) | **GET** /query/style_form/explode_true/object | Test query parameter(s)
[**TestQueryStyleFormExplodeTrueObjectAllOf**](QueryApi.md#TestQueryStyleFormExplodeTrueObjectAllOf) | **GET** /query/style_form/explode_true/object/allOf | Test query parameter(s)
[**TestQueryStyleJsonSerializationObject**](QueryApi.md#TestQueryStyleJsonSerializationObject) | **GET** /query/style_jsonSerialization/object | Test query parameter(s)


# **TestEnumRefString**
> character TestEnumRefString(enum_nonref_string_query = var.enum_nonref_string_query, enum_ref_string_query = var.enum_ref_string_query)

Test query parameter(s)

Test query parameter(s)

### Example
```R
library(openapi)

# Test query parameter(s)
#
# prepare function argument(s)
var_enum_nonref_string_query <- "enum_nonref_string_query_example" # character |  (Optional)
var_enum_ref_string_query <- StringEnumRef$new() # StringEnumRef |  (Optional)

api_instance <- QueryApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestEnumRefString(enum_nonref_string_query = var_enum_nonref_string_query, enum_ref_string_query = var_enum_ref_string_querydata_file = "result.txt")
result <- api_instance$TestEnumRefString(enum_nonref_string_query = var_enum_nonref_string_query, enum_ref_string_query = var_enum_ref_string_query)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_nonref_string_query** | Enum [success, failure, unclassified] |  | [optional] 
 **enum_ref_string_query** | [**StringEnumRef**](.md)|  | [optional] 

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

# **TestQueryDatetimeDateString**
> character TestQueryDatetimeDateString(datetime_query = var.datetime_query, date_query = var.date_query, string_query = var.string_query)

Test query parameter(s)

Test query parameter(s)

### Example
```R
library(openapi)

# Test query parameter(s)
#
# prepare function argument(s)
var_datetime_query <- "datetime_query_example" # character |  (Optional)
var_date_query <- "date_query_example" # character |  (Optional)
var_string_query <- "string_query_example" # character |  (Optional)

api_instance <- QueryApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestQueryDatetimeDateString(datetime_query = var_datetime_query, date_query = var_date_query, string_query = var_string_querydata_file = "result.txt")
result <- api_instance$TestQueryDatetimeDateString(datetime_query = var_datetime_query, date_query = var_date_query, string_query = var_string_query)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **datetime_query** | **character**|  | [optional] 
 **date_query** | **character**|  | [optional] 
 **string_query** | **character**|  | [optional] 

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

# **TestQueryIntegerBooleanString**
> character TestQueryIntegerBooleanString(integer_query = var.integer_query, boolean_query = var.boolean_query, string_query = var.string_query)

Test query parameter(s)

Test query parameter(s)

### Example
```R
library(openapi)

# Test query parameter(s)
#
# prepare function argument(s)
var_integer_query <- 56 # integer |  (Optional)
var_boolean_query <- "boolean_query_example" # character |  (Optional)
var_string_query <- "string_query_example" # character |  (Optional)

api_instance <- QueryApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestQueryIntegerBooleanString(integer_query = var_integer_query, boolean_query = var_boolean_query, string_query = var_string_querydata_file = "result.txt")
result <- api_instance$TestQueryIntegerBooleanString(integer_query = var_integer_query, boolean_query = var_boolean_query, string_query = var_string_query)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **integer_query** | **integer**|  | [optional] 
 **boolean_query** | **character**|  | [optional] 
 **string_query** | **character**|  | [optional] 

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

# **TestQueryStyleDeepObjectExplodeTrueObject**
> character TestQueryStyleDeepObjectExplodeTrueObject(query_object = var.query_object)

Test query parameter(s)

Test query parameter(s)

### Example
```R
library(openapi)

# Test query parameter(s)
#
# prepare function argument(s)
var_query_object <- Pet$new("name_example", c("photoUrls_example"), 123, Category$new(123, "name_example"), c(Tag$new(123, "name_example")), "available") # Pet |  (Optional)

api_instance <- QueryApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestQueryStyleDeepObjectExplodeTrueObject(query_object = var_query_objectdata_file = "result.txt")
result <- api_instance$TestQueryStyleDeepObjectExplodeTrueObject(query_object = var_query_object)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query_object** | [**Pet**](.md)|  | [optional] 

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

# **TestQueryStyleDeepObjectExplodeTrueObjectAllOf**
> character TestQueryStyleDeepObjectExplodeTrueObjectAllOf(query_object = var.query_object)

Test query parameter(s)

Test query parameter(s)

### Example
```R
library(openapi)

# Test query parameter(s)
#
# prepare function argument(s)
var_query_object <- test_query_style_deepObject_explode_true_object_allOf_query_object_parameter$new("size_example", "color_example", 123, "name_example") # TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter |  (Optional)

api_instance <- QueryApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestQueryStyleDeepObjectExplodeTrueObjectAllOf(query_object = var_query_objectdata_file = "result.txt")
result <- api_instance$TestQueryStyleDeepObjectExplodeTrueObjectAllOf(query_object = var_query_object)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query_object** | [**TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter**](.md)|  | [optional] 

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

# **TestQueryStyleFormExplodeFalseArrayInteger**
> character TestQueryStyleFormExplodeFalseArrayInteger(query_object = var.query_object)

Test query parameter(s)

Test query parameter(s)

### Example
```R
library(openapi)

# Test query parameter(s)
#
# prepare function argument(s)
var_query_object <- c(123) # array[integer] |  (Optional)

api_instance <- QueryApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestQueryStyleFormExplodeFalseArrayInteger(query_object = var_query_objectdata_file = "result.txt")
result <- api_instance$TestQueryStyleFormExplodeFalseArrayInteger(query_object = var_query_object)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query_object** | list( **integer** )|  | [optional] 

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

# **TestQueryStyleFormExplodeFalseArrayString**
> character TestQueryStyleFormExplodeFalseArrayString(query_object = var.query_object)

Test query parameter(s)

Test query parameter(s)

### Example
```R
library(openapi)

# Test query parameter(s)
#
# prepare function argument(s)
var_query_object <- c("inner_example") # array[character] |  (Optional)

api_instance <- QueryApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestQueryStyleFormExplodeFalseArrayString(query_object = var_query_objectdata_file = "result.txt")
result <- api_instance$TestQueryStyleFormExplodeFalseArrayString(query_object = var_query_object)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query_object** | list( **character** )|  | [optional] 

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

# **TestQueryStyleFormExplodeTrueArrayString**
> character TestQueryStyleFormExplodeTrueArrayString(query_object = var.query_object)

Test query parameter(s)

Test query parameter(s)

### Example
```R
library(openapi)

# Test query parameter(s)
#
# prepare function argument(s)
var_query_object <- test_query_style_form_explode_true_array_string_query_object_parameter$new(c("values_example")) # TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter |  (Optional)

api_instance <- QueryApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestQueryStyleFormExplodeTrueArrayString(query_object = var_query_objectdata_file = "result.txt")
result <- api_instance$TestQueryStyleFormExplodeTrueArrayString(query_object = var_query_object)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query_object** | [**TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter**](.md)|  | [optional] 

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

# **TestQueryStyleFormExplodeTrueObject**
> character TestQueryStyleFormExplodeTrueObject(query_object = var.query_object)

Test query parameter(s)

Test query parameter(s)

### Example
```R
library(openapi)

# Test query parameter(s)
#
# prepare function argument(s)
var_query_object <- Pet$new("name_example", c("photoUrls_example"), 123, Category$new(123, "name_example"), c(Tag$new(123, "name_example")), "available") # Pet |  (Optional)

api_instance <- QueryApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestQueryStyleFormExplodeTrueObject(query_object = var_query_objectdata_file = "result.txt")
result <- api_instance$TestQueryStyleFormExplodeTrueObject(query_object = var_query_object)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query_object** | [**Pet**](.md)|  | [optional] 

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

# **TestQueryStyleFormExplodeTrueObjectAllOf**
> character TestQueryStyleFormExplodeTrueObjectAllOf(query_object = var.query_object)

Test query parameter(s)

Test query parameter(s)

### Example
```R
library(openapi)

# Test query parameter(s)
#
# prepare function argument(s)
var_query_object <- DataQuery$new(123, c("SUCCESS"), "suffix_example", "text_example", "date_example") # DataQuery |  (Optional)

api_instance <- QueryApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestQueryStyleFormExplodeTrueObjectAllOf(query_object = var_query_objectdata_file = "result.txt")
result <- api_instance$TestQueryStyleFormExplodeTrueObjectAllOf(query_object = var_query_object)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query_object** | [**DataQuery**](.md)|  | [optional] 

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

# **TestQueryStyleJsonSerializationObject**
> character TestQueryStyleJsonSerializationObject(json_serialized_object_ref_string_query = var.json_serialized_object_ref_string_query, json_serialized_object_array_ref_string_query = var.json_serialized_object_array_ref_string_query)

Test query parameter(s)

Test query parameter(s)

### Example
```R
library(openapi)

# Test query parameter(s)
#
# prepare function argument(s)
var_json_serialized_object_ref_string_query <- Pet$new("name_example", c("photoUrls_example"), 123, Category$new(123, "name_example"), c(Tag$new(123, "name_example")), "available") # Pet |  (Optional)
var_json_serialized_object_array_ref_string_query <- c(Pet$new("name_example", c("photoUrls_example"), 123, Category$new(123, "name_example"), c(Tag$new(123, "name_example")), "available")) # array[Pet] |  (Optional)

api_instance <- QueryApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestQueryStyleJsonSerializationObject(json_serialized_object_ref_string_query = var_json_serialized_object_ref_string_query, json_serialized_object_array_ref_string_query = var_json_serialized_object_array_ref_string_querydata_file = "result.txt")
result <- api_instance$TestQueryStyleJsonSerializationObject(json_serialized_object_ref_string_query = var_json_serialized_object_ref_string_query, json_serialized_object_array_ref_string_query = var_json_serialized_object_array_ref_string_query)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **json_serialized_object_ref_string_query** | [**Pet**](.md)|  | [optional] 
 **json_serialized_object_array_ref_string_query** | list( [**Pet**](Pet.md) )|  | [optional] 

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

