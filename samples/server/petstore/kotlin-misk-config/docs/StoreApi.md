# StoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**deleteOrder**](StoreApi.md#deleteOrder) | **Delete** /store/order/{orderId} | Delete purchase order by ID
[**getInventory**](StoreApi.md#getInventory) | **Get** /store/inventory | Returns pet inventories by status
[**getOrderById**](StoreApi.md#getOrderById) | **Get** /store/order/{orderId} | Find purchase order by ID
[**placeOrder**](StoreApi.md#placeOrder) | **Post** /store/order | Place an order for a pet


<a name="deleteOrder"></a>
# **deleteOrder**
> deleteOrder(orderId)

Delete purchase order by ID

For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors

### Example
```kotlin
// Import classes:
//import org.openapitools.infrastructure.*
//import org.openapitools.server.api.model.*

val apiInstance = StoreApi()
val orderId : kotlin.String = orderId_example // kotlin.String | ID of the order that needs to be deleted
try {
    apiInstance.deleteOrder(orderId)
} catch (e: ClientException) {
    println("4xx response calling StoreApi#deleteOrder")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling StoreApi#deleteOrder")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **kotlin.String**| ID of the order that needs to be deleted |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="getInventory"></a>
# **getInventory**
> kotlin.collections.Map&lt;kotlin.String, kotlin.Int&gt; getInventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```kotlin
// Import classes:
//import org.openapitools.infrastructure.*
//import org.openapitools.server.api.model.*

val apiInstance = StoreApi()
try {
    val result : kotlin.collections.Map<kotlin.String, kotlin.Int> = apiInstance.getInventory()
    println(result)
} catch (e: ClientException) {
    println("4xx response calling StoreApi#getInventory")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling StoreApi#getInventory")
    e.printStackTrace()
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**kotlin.collections.Map&lt;kotlin.String, kotlin.Int&gt;**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: APPLICATION_JSON

<a name="getOrderById"></a>
# **getOrderById**
> Order getOrderById(orderId)

Find purchase order by ID

For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generate exceptions

### Example
```kotlin
// Import classes:
//import org.openapitools.infrastructure.*
//import org.openapitools.server.api.model.*

val apiInstance = StoreApi()
val orderId : kotlin.Long = 789 // kotlin.Long | ID of pet that needs to be fetched
try {
    val result : Order = apiInstance.getOrderById(orderId)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling StoreApi#getOrderById")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling StoreApi#getOrderById")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **kotlin.Long**| ID of pet that needs to be fetched |

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: APPLICATION_XML, APPLICATION_JSON

<a name="placeOrder"></a>
# **placeOrder**
> Order placeOrder(order)

Place an order for a pet



### Example
```kotlin
// Import classes:
//import org.openapitools.infrastructure.*
//import org.openapitools.server.api.model.*

val apiInstance = StoreApi()
val order : Order =  // Order | order placed for purchasing the pet
try {
    val result : Order = apiInstance.placeOrder(order)
    println(result)
} catch (e: ClientException) {
    println("4xx response calling StoreApi#placeOrder")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling StoreApi#placeOrder")
    e.printStackTrace()
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

 - **Content-Type**: APPLICATION_JSON
 - **Accept**: APPLICATION_XML, APPLICATION_JSON

