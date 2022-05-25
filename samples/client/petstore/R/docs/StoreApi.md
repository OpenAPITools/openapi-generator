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

var.order_id <- 'order_id_example' # character | ID of the order that needs to be deleted

#Delete purchase order by ID
api.instance <- StoreApi$new()
api.instance$DeleteOrder(var.order_id)
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


#Returns pet inventories by status
api.instance <- StoreApi$new()
# Configure API key authorization: api_key
api.instance$apiClient$apiKeys['api_key'] <- 'TODO_YOUR_API_KEY';
result <- api.instance$GetInventory()
dput(result)
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

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example
```R
library(petstore)

var.order_id <- 56 # integer | ID of pet that needs to be fetched

#Find purchase order by ID
api.instance <- StoreApi$new()
result <- api.instance$GetOrderById(var.order_id)
dput(result)
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

var.order <- Order$new(123, 123, 123, "shipDate_example", "placed", "complete_example") # Order | order placed for purchasing the pet

#Place an order for a pet
api.instance <- StoreApi$new()
result <- api.instance$PlaceOrder(var.order)
dput(result)
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

