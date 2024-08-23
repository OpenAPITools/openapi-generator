# BodyApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestBinaryGif**](BodyApi.md#TestBinaryGif) | **POST** /binary/gif | Test binary (gif) response body
[**TestBodyApplicationOctetstreamBinary**](BodyApi.md#TestBodyApplicationOctetstreamBinary) | **POST** /body/application/octetstream/binary | Test body parameter(s)
[**TestBodyMultipartFormdataArrayOfBinary**](BodyApi.md#TestBodyMultipartFormdataArrayOfBinary) | **POST** /body/application/octetstream/array_of_binary | Test array of binary in multipart mime
[**TestBodyMultipartFormdataSingleBinary**](BodyApi.md#TestBodyMultipartFormdataSingleBinary) | **POST** /body/application/octetstream/single_binary | Test single binary in multipart mime
[**TestEchoBodyAllOfPet**](BodyApi.md#TestEchoBodyAllOfPet) | **POST** /echo/body/allOf/Pet | Test body parameter(s)
[**TestEchoBodyFreeFormObjectResponseString**](BodyApi.md#TestEchoBodyFreeFormObjectResponseString) | **POST** /echo/body/FreeFormObject/response_string | Test free form object
[**TestEchoBodyPet**](BodyApi.md#TestEchoBodyPet) | **POST** /echo/body/Pet | Test body parameter(s)
[**TestEchoBodyPetResponseString**](BodyApi.md#TestEchoBodyPetResponseString) | **POST** /echo/body/Pet/response_string | Test empty response body
[**TestEchoBodyStringEnum**](BodyApi.md#TestEchoBodyStringEnum) | **POST** /echo/body/string_enum | Test string enum response body
[**TestEchoBodyTagResponseString**](BodyApi.md#TestEchoBodyTagResponseString) | **POST** /echo/body/Tag/response_string | Test empty json (request body)


# **TestBinaryGif**
> data.frame TestBinaryGif()

Test binary (gif) response body

Test binary (gif) response body

### Example
```R
library(openapi)

# Test binary (gif) response body
#

api_instance <- BodyApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestBinaryGif(data_file = "result.txt")
result <- api_instance$TestBinaryGif()
dput(result)
```

### Parameters
This endpoint does not need any parameter.

### Return type

**data.frame**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: image/gif

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

# **TestBodyApplicationOctetstreamBinary**
> character TestBodyApplicationOctetstreamBinary(body = var.body)

Test body parameter(s)

Test body parameter(s)

### Example
```R
library(openapi)

# Test body parameter(s)
#
# prepare function argument(s)
var_body <- File.new('/path/to/file') # data.frame |  (Optional)

api_instance <- BodyApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestBodyApplicationOctetstreamBinary(body = var_bodydata_file = "result.txt")
result <- api_instance$TestBodyApplicationOctetstreamBinary(body = var_body)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **data.frame**|  | [optional] 

### Return type

**character**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/octet-stream
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

# **TestBodyMultipartFormdataArrayOfBinary**
> character TestBodyMultipartFormdataArrayOfBinary(files)

Test array of binary in multipart mime

Test array of binary in multipart mime

### Example
```R
library(openapi)

# Test array of binary in multipart mime
#
# prepare function argument(s)
var_files <- c(123) # array[data.frame] | 

api_instance <- BodyApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestBodyMultipartFormdataArrayOfBinary(var_filesdata_file = "result.txt")
result <- api_instance$TestBodyMultipartFormdataArrayOfBinary(var_files)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **files** | list( **data.frame** )|  | 

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

# **TestBodyMultipartFormdataSingleBinary**
> character TestBodyMultipartFormdataSingleBinary(my_file = var.my_file)

Test single binary in multipart mime

Test single binary in multipart mime

### Example
```R
library(openapi)

# Test single binary in multipart mime
#
# prepare function argument(s)
var_my_file <- File.new('/path/to/file') # data.frame |  (Optional)

api_instance <- BodyApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestBodyMultipartFormdataSingleBinary(my_file = var_my_filedata_file = "result.txt")
result <- api_instance$TestBodyMultipartFormdataSingleBinary(my_file = var_my_file)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **my_file** | **data.frame**|  | [optional] 

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

# **TestEchoBodyAllOfPet**
> Pet TestEchoBodyAllOfPet(pet = var.pet)

Test body parameter(s)

Test body parameter(s)

### Example
```R
library(openapi)

# Test body parameter(s)
#
# prepare function argument(s)
var_pet <- Pet$new("name_example", c("photoUrls_example"), 123, Category$new(123, "name_example"), c(Tag$new(123, "name_example")), "available") # Pet | Pet object that needs to be added to the store (Optional)

api_instance <- BodyApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestEchoBodyAllOfPet(pet = var_petdata_file = "result.txt")
result <- api_instance$TestEchoBodyAllOfPet(pet = var_pet)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

[**Pet**](Pet.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

# **TestEchoBodyFreeFormObjectResponseString**
> character TestEchoBodyFreeFormObjectResponseString(body = var.body)

Test free form object

Test free form object

### Example
```R
library(openapi)

# Test free form object
#
# prepare function argument(s)
var_body <- c(key = TODO) # object | Free form object (Optional)

api_instance <- BodyApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestEchoBodyFreeFormObjectResponseString(body = var_bodydata_file = "result.txt")
result <- api_instance$TestEchoBodyFreeFormObjectResponseString(body = var_body)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **object**| Free form object | [optional] 

### Return type

**character**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

# **TestEchoBodyPet**
> Pet TestEchoBodyPet(pet = var.pet)

Test body parameter(s)

Test body parameter(s)

### Example
```R
library(openapi)

# Test body parameter(s)
#
# prepare function argument(s)
var_pet <- Pet$new("name_example", c("photoUrls_example"), 123, Category$new(123, "name_example"), c(Tag$new(123, "name_example")), "available") # Pet | Pet object that needs to be added to the store (Optional)

api_instance <- BodyApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestEchoBodyPet(pet = var_petdata_file = "result.txt")
result <- api_instance$TestEchoBodyPet(pet = var_pet)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

[**Pet**](Pet.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

# **TestEchoBodyPetResponseString**
> character TestEchoBodyPetResponseString(pet = var.pet)

Test empty response body

Test empty response body

### Example
```R
library(openapi)

# Test empty response body
#
# prepare function argument(s)
var_pet <- Pet$new("name_example", c("photoUrls_example"), 123, Category$new(123, "name_example"), c(Tag$new(123, "name_example")), "available") # Pet | Pet object that needs to be added to the store (Optional)

api_instance <- BodyApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestEchoBodyPetResponseString(pet = var_petdata_file = "result.txt")
result <- api_instance$TestEchoBodyPetResponseString(pet = var_pet)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

**character**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

# **TestEchoBodyStringEnum**
> StringEnumRef TestEchoBodyStringEnum(body = var.body)

Test string enum response body

Test string enum response body

### Example
```R
library(openapi)

# Test string enum response body
#
# prepare function argument(s)
var_body <- "body_example" # character | String enum (Optional)

api_instance <- BodyApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestEchoBodyStringEnum(body = var_bodydata_file = "result.txt")
result <- api_instance$TestEchoBodyStringEnum(body = var_body)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **character**| String enum | [optional] 

### Return type

[**StringEnumRef**](StringEnumRef.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

# **TestEchoBodyTagResponseString**
> character TestEchoBodyTagResponseString(tag = var.tag)

Test empty json (request body)

Test empty json (request body)

### Example
```R
library(openapi)

# Test empty json (request body)
#
# prepare function argument(s)
var_tag <- Tag$new(123, "name_example") # Tag | Tag object (Optional)

api_instance <- BodyApi$new()
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestEchoBodyTagResponseString(tag = var_tagdata_file = "result.txt")
result <- api_instance$TestEchoBodyTagResponseString(tag = var_tag)
dput(result)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tag** | [**Tag**](Tag.md)| Tag object | [optional] 

### Return type

**character**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

