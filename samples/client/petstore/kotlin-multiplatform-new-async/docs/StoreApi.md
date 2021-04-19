# StoreApi

All URIs are by default relative to `http://petstore.swagger.io/v2`

Method | HTTP request | Description
------------- | ------------- | -------------
[`deleteOrder`](#deleteOrder) | `DELETE` /store/order/{orderId} | Delete purchase order by ID
[`getInventory`](#getInventory) | `GET` /store/inventory | Returns pet inventories by status
[`getOrderById`](#getOrderById) | `GET` /store/order/{orderId} | Find purchase order by ID
[`placeOrder`](#placeOrder) | `POST` /store/order | Place an order for a pet


# **deleteOrder**
> deleteOrder(orderId)

Delete purchase order by ID

For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.StoreApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val storeApi = StoreApi()
val orderId: kotlin.String = orderId_example 
try {
    storeApi.deleteOrder(
        orderId,
    )
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

`Unit`

### Authorization

No authorization required

### HTTP request headers

```
```

# **getInventory**
> kotlin.collections.Map&lt;kotlin.String, kotlin.Int&gt; getInventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.StoreApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val storeApi = StoreApi()
try {
    val result: kotlin.collections.Map<kotlin.String, kotlin.Int> = storeApi.getInventory(
    )
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

`kotlin.collections.Map&lt;kotlin.String, kotlin.Int&gt;`

### Authorization

Configure api_key (API key):
```kotlin
apiClient.configureApiKey("api_key") {
    key("your-api-key")
    keyPrefix("YourPrefix ") // Optional
}
```

### HTTP request headers

```
Accept: application/json
```

# **getOrderById**
> Order getOrderById(orderId)

Find purchase order by ID

For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.StoreApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val storeApi = StoreApi()
val orderId: kotlin.Long = 789 
try {
    val result: Order = storeApi.getOrderById(
        orderId,
    )
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

[`Order`](Order.md)

### Authorization

No authorization required

### HTTP request headers

```
Accept: application/xml, application/json
```

# **placeOrder**
> Order placeOrder(body)

Place an order for a pet

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.StoreApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val storeApi = StoreApi()
val body: Order =  
try {
    val result: Order = storeApi.placeOrder(
        body,
    )
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
 **body** | [**Order**](Order.md)| order placed for purchasing the pet |

### Return type

[`Order`](Order.md)

### Authorization

No authorization required

### HTTP request headers

```
Accept: application/xml, application/json
```

