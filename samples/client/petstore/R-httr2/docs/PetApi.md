# PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**add_pet**](PetApi.md#add_pet) | **POST** /pet | Add a new pet to the store
[**delete_pet**](PetApi.md#delete_pet) | **DELETE** /pet/{petId} | Deletes a pet
[**find_pets_by_status**](PetApi.md#find_pets_by_status) | **GET** /pet/findByStatus | Finds Pets by status
[**find_pets_by_tags**](PetApi.md#find_pets_by_tags) | **GET** /pet/findByTags | Finds Pets by tags
[**get_pet_by_id**](PetApi.md#get_pet_by_id) | **GET** /pet/{petId} | Find pet by ID
[**get_pet_by_id_streaming**](PetApi.md#get_pet_by_id_streaming) | **GET** /pet/{petId}?streaming | Find pet by ID (streaming)
[**test_header**](PetApi.md#test_header) | **GET** /pet_header_test | Header test
[**update_pet**](PetApi.md#update_pet) | **PUT** /pet | Update an existing pet
[**update_pet_with_form**](PetApi.md#update_pet_with_form) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**upload_file**](PetApi.md#upload_file) | **POST** /pet/{petId}/uploadImage | uploads an image


# **add_pet**
> Pet add_pet(pet)

Add a new pet to the store



### Example
```R
library(petstore)

# Add a new pet to the store
#
# prepare function argument(s)
var_pet <- Pet$new("name_example", c("photoUrls_example"), 123, Category$new(123, "name_example"), c(Tag$new(123, "name_example")), "available") # Pet | Pet object that needs to be added to the store

api_instance <- PetApi$new()
# Configure HTTP basic authorization: http_auth
api_instance$api_client$username <- Sys.getenv("USERNAME")
api_instance$api_client$password <- Sys.getenv("PASSWORD")
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$add_pet(var_pet, data_file = "result.txt"),
             api_instance$add_pet(var_pet),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `add_pet`:")
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
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

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

# **delete_pet**
> delete_pet(pet_id, api_key = var.api_key)

Deletes a pet



### Example
```R
library(petstore)

# Deletes a pet
#
# prepare function argument(s)
var_pet_id <- 56 # integer | Pet id to delete
var_api_key <- "api_key_example" # character |  (Optional)

api_instance <- PetApi$new()
# Configure OAuth2 access token for authorization: petstore_auth
api_instance$api_client$access_token <- Sys.getenv("ACCESS_TOKEN")
result <- tryCatch(
             api_instance$delete_pet(var_pet_id, api_key = var_api_key),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `delete_pet`:")
  dput(result$ApiException$toString())
  # error object
  dput(result$ApiException$error_object$toJSONString())
}
# This endpoint doesn't return data
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **integer**| Pet id to delete | 
 **api_key** | **character**|  | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **400** | Invalid pet value |  -  |

# **find_pets_by_status**
> array[Pet] find_pets_by_status(status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```R
library(petstore)

# Finds Pets by status
#
# prepare function argument(s)
var_status <- c("available") # array[character] | Status values that need to be considered for filter

api_instance <- PetApi$new()
# Configure OAuth2 access token for authorization: petstore_auth
api_instance$api_client$access_token <- Sys.getenv("ACCESS_TOKEN")
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$find_pets_by_status(var_status, data_file = "result.txt"),
             api_instance$find_pets_by_status(var_status),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `find_pets_by_status`:")
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
 **status** | Enum [available, pending, sold] | Status values that need to be considered for filter | 

### Return type

[**array[Pet]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid status value |  -  |

# **find_pets_by_tags**
> array[Pet] find_pets_by_tags(tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```R
library(petstore)

# Finds Pets by tags
#
# prepare function argument(s)
var_tags <- c("inner_example") # array[character] | Tags to filter by

api_instance <- PetApi$new()
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$find_pets_by_tags(var_tags, data_file = "result.txt"),
             api_instance$find_pets_by_tags(var_tags),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `find_pets_by_tags`:")
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
 **tags** | list( **character** )| Tags to filter by | 

### Return type

[**array[Pet]**](Pet.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid tag value |  -  |

# **get_pet_by_id**
> Pet get_pet_by_id(pet_id)

Find pet by ID

Returns a single pet

### Example
```R
library(petstore)

# Find pet by ID
#
# prepare function argument(s)
var_pet_id <- 56 # integer | ID of pet to return

api_instance <- PetApi$new()
# Configure HTTP bearer authorization: BearerToken
api_instance$api_client$bearer_token <- Sys.getenv("BEARER_TOKEN")
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$get_pet_by_id(var_pet_id, data_file = "result.txt"),
             api_instance$get_pet_by_id(var_pet_id),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `get_pet_by_id`:")
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
 **pet_id** | **integer**| ID of pet to return | 

### Return type

[**Pet**](Pet.md)

### Authorization

[BearerToken](../README.md#BearerToken)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid ID supplied |  -  |
| **404** | Pet not found |  -  |

# **get_pet_by_id_streaming**
> Pet get_pet_by_id_streaming(pet_id)

Find pet by ID (streaming)

Returns a single pet

### Example
```R
library(petstore)

# Find pet by ID (streaming)
#
# prepare function argument(s)
var_pet_id <- 56 # integer | ID of pet to return

api_instance <- PetApi$new()
# Configure API key authorization: api_key
api_instance$api_client$api_keys["api_key"] <- Sys.getenv("API_KEY")
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$get_pet_by_id_streaming(var_pet_id, data_file = "result.txt"),
             # this endpoint supports data streaming via a callback function using the optional `stream_callback` parameter, e.g.
             # api_instance$get_pet_by_id_streaming(var_pet_id, stream_callback = function(x){ print(length(x)) }),
             api_instance$get_pet_by_id_streaming(var_pet_id),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `get_pet_by_id_streaming`:")
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
 **pet_id** | **integer**| ID of pet to return | 

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid ID supplied |  -  |
| **404** | Pet not found |  -  |

# **test_header**
> Pet test_header(header_test_int)

Header test

Header test

### Example
```R
library(petstore)

# Header test
#
# prepare function argument(s)
var_header_test_int <- 56 # integer | header test int

api_instance <- PetApi$new()
# Configure API key authorization: api_key
api_instance$api_client$api_keys["api_key"] <- Sys.getenv("API_KEY")
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$test_header(var_header_test_int, data_file = "result.txt"),
             # this endpoint supports data streaming via a callback function using the optional `stream_callback` parameter, e.g.
             # api_instance$test_header(var_header_test_int, stream_callback = function(x){ print(length(x)) }),
             api_instance$test_header(var_header_test_int),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `test_header`:")
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
 **header_test_int** | **integer**| header test int | 

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid ID supplied |  -  |
| **404** | Pet not found |  -  |

# **update_pet**
> Pet update_pet(pet)

Update an existing pet



### Example
```R
library(petstore)

# Update an existing pet
#
# prepare function argument(s)
var_pet <- Pet$new("name_example", c("photoUrls_example"), 123, Category$new(123, "name_example"), c(Tag$new(123, "name_example")), "available") # Pet | Pet object that needs to be added to the store

api_instance <- PetApi$new()
# Configure OAuth2 access token for authorization: petstore_auth
api_instance$api_client$access_token <- Sys.getenv("ACCESS_TOKEN")
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$update_pet(var_pet, data_file = "result.txt"),
             api_instance$update_pet(var_pet),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `update_pet`:")
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
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

[**Pet**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml, multipart/related
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid ID supplied |  -  |
| **404** | Pet not found |  -  |
| **405** | Validation exception |  -  |

# **update_pet_with_form**
> update_pet_with_form(pet_id, name = var.name, status = var.status)

Updates a pet in the store with form data



### Example
```R
library(petstore)

# Updates a pet in the store with form data
#
# prepare function argument(s)
var_pet_id <- 56 # integer | ID of pet that needs to be updated
var_name <- "name_example" # character | Updated name of the pet (Optional)
var_status <- "status_example" # character | Updated status of the pet (Optional)

api_instance <- PetApi$new()
result <- tryCatch(
             api_instance$update_pet_with_form(var_pet_id, name = var_name, status = var_status),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `update_pet_with_form`:")
  dput(result$ApiException$toString())
  # error object
  dput(result$ApiException$error_object$toJSONString())
}
# This endpoint doesn't return data
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **integer**| ID of pet that needs to be updated | 
 **name** | **character**| Updated name of the pet | [optional] 
 **status** | **character**| Updated status of the pet | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **405** | Invalid input |  -  |

# **upload_file**
> ModelApiResponse upload_file(pet_id, additional_metadata = var.additional_metadata, file = var.file)

uploads an image



### Example
```R
library(petstore)

# uploads an image
#
# prepare function argument(s)
var_pet_id <- 56 # integer | ID of pet to update
var_additional_metadata <- "additional_metadata_example" # character | Additional data to pass to server (Optional)
var_file <- File.new('/path/to/file') # data.frame | file to upload (Optional)

api_instance <- PetApi$new()
# Configure OAuth2 access token for authorization: petstore_auth
api_instance$api_client$access_token <- Sys.getenv("ACCESS_TOKEN")
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$upload_file(var_pet_id, additional_metadata = var_additional_metadata, file = var_file, data_file = "result.txt"),
             api_instance$upload_file(var_pet_id, additional_metadata = var_additional_metadata, file = var_file),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `upload_file`:")
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
 **pet_id** | **integer**| ID of pet to update | 
 **additional_metadata** | **character**| Additional data to pass to server | [optional] 
 **file** | **data.frame**| file to upload | [optional] 

### Return type

[**ModelApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

