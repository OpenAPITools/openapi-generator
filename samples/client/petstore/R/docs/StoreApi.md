# StoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**DeleteOrder**](StoreApi.md#DeleteOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**GetInventory**](StoreApi.md#GetInventory) | **GET** /store/inventory | Returns pet inventories by status
[**GetOrderById**](StoreApi.md#GetOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID
[**PlaceOrder**](StoreApi.md#PlaceOrder) | **POST** /store/order | Place an order for a pet


# **DeleteOrder**
> DeleteOrder(order_id)

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example
```R
library(petstore)

# Delete purchase order by ID
#
# prepare function argument(s)
var_order_id <- "order_id_example" # character | ID of the order that needs to be deleted

api_instance <- StoreApi$new()
result <- tryCatch(
             api_instance$DeleteOrder(var_order_id),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `DeleteOrder`:")
  dput(result$ApiException$toString())
  # error object
  dput(result$ApiException$error_object$toJSONString())
}
# This endpoint doesn't return data
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order_id** | **character**| ID of the order that needs to be deleted | 

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
| **400** | Invalid ID supplied |  -  |
| **404** | Order not found |  -  |

# **GetInventory**
> map(integer) GetInventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```R
library(petstore)

# Returns pet inventories by status
#

api_instance <- StoreApi$new()
# Configure API key authorization: api_key
api_instance$api_client$api_keys["api_key"] <- Sys.getenv("API_KEY")
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$GetInventory(data_file = "result.txt"),
             api_instance$GetInventory(),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `GetInventory`:")
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
This endpoint does not need any parameter.

### Return type

**map(integer)**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |

# **GetOrderById**
> Order GetOrderById(order_id)

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions

### Example
```R
library(petstore)

# Find purchase order by ID
#
# prepare function argument(s)
var_order_id <- 56 # integer | ID of pet that needs to be fetched

api_instance <- StoreApi$new()
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$GetOrderById(var_order_id, data_file = "result.txt"),
             api_instance$GetOrderById(var_order_id),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `GetOrderById`:")
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
 **order_id** | **integer**| ID of pet that needs to be fetched | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid ID supplied |  -  |
| **404** | Order not found |  -  |

# **PlaceOrder**
> Order PlaceOrder(order)

Place an order for a pet



### Example
```R
library(petstore)

# Place an order for a pet
#
# prepare function argument(s)
var_order <- Order$new(123, 123, 123, "shipDate_example", "placed", "complete_example") # Order | order placed for purchasing the pet

api_instance <- StoreApi$new()
result <- tryCatch(
             # to save the result into a file, simply add the optional `data_file` parameter, e.g.
             # api_instance$PlaceOrder(var_order, data_file = "result.txt"),
             api_instance$PlaceOrder(var_order),
             ApiException = function(ex) ex
          )
# In case of error, print the error object
if (!is.null(result$ApiException)) {
  print("Exception occurs when calling `PlaceOrder`:")
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
 **order** | [**Order**](Order.md)| order placed for purchasing the pet | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | successful operation |  -  |
| **400** | Invalid Order |  -  |

