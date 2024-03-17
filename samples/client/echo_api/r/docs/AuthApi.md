# AuthApi

All URIs are relative to *http://localhost:3000*

Method | HTTP request | Description
------------- | ------------- | -------------
[**TestAuthHttpBasic**](AuthApi.md#TestAuthHttpBasic) | **POST** /auth/http/basic | To test HTTP basic authentication
[**TestAuthHttpBearer**](AuthApi.md#TestAuthHttpBearer) | **POST** /auth/http/bearer | To test HTTP bearer authentication


# **TestAuthHttpBasic**
> character TestAuthHttpBasic()

To test HTTP basic authentication

To test HTTP basic authentication

### Example
```R
library(openapi)

# To test HTTP basic authentication
#

api_instance <- AuthApi$new()
# Configure HTTP basic authorization: http_auth
api_instance$api_client$username <- Sys.getenv("USERNAME")
api_instance$api_client$password <- Sys.getenv("PASSWORD")
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestAuthHttpBasic(data_file = "result.txt")
result <- api_instance$TestAuthHttpBasic()
dput(result)
```

### Parameters
This endpoint does not need any parameter.

### Return type

**character**

### Authorization

[http_auth](../README.md#http_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

# **TestAuthHttpBearer**
> character TestAuthHttpBearer()

To test HTTP bearer authentication

To test HTTP bearer authentication

### Example
```R
library(openapi)

# To test HTTP bearer authentication
#

api_instance <- AuthApi$new()
# Configure HTTP bearer authorization: http_bearer_auth
api_instance$api_client$bearer_token <- Sys.getenv("BEARER_TOKEN")
# to save the result into a file, simply add the optional `data_file` parameter, e.g.
# result <- api_instance$TestAuthHttpBearer(data_file = "result.txt")
result <- api_instance$TestAuthHttpBearer()
dput(result)
```

### Parameters
This endpoint does not need any parameter.

### Return type

**character**

### Authorization

[http_bearer_auth](../README.md#http_bearer_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: text/plain

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful operation |  -  |

