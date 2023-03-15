# StoreController

All URIs are relative to `"/v2"`

The controller class is defined in **[StoreController.java](../../src/main/java/org/openapitools/controller/StoreController.java)**

Method | HTTP request | Description
------------- | ------------- | -------------
[**deleteOrder**](#deleteOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**getInventory**](#getInventory) | **GET** /store/inventory | Returns pet inventories by status
[**getOrderById**](#getOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID
[**placeOrder**](#placeOrder) | **POST** /store/order | Place an order for a pet

<a name="deleteOrder"></a>
# **deleteOrder**
```java
Mono<Object> StoreController.deleteOrder(orderId)
```

Delete purchase order by ID

For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**orderId** | `String` | ID of the order that needs to be deleted |



### HTTP request headers
 - **Accepts Content-Type**: Not defined
 - **Produces Content-Type**: Not defined

<a name="getInventory"></a>
# **getInventory**
```java
Mono<Map<String, Integer>> StoreController.getInventory()
```

Returns pet inventories by status

Returns a map of status codes to quantities


### Return type
`Map&lt;String, Integer&gt;`

### Authorization
* **api_key**

### HTTP request headers
 - **Accepts Content-Type**: Not defined
 - **Produces Content-Type**: `application/json`

<a name="getOrderById"></a>
# **getOrderById**
```java
Mono<Order> StoreController.getOrderById(orderId)
```

Find purchase order by ID

For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**orderId** | `Long` | ID of pet that needs to be fetched |

### Return type
[**Order**](../../docs/models/Order.md)


### HTTP request headers
 - **Accepts Content-Type**: Not defined
 - **Produces Content-Type**: `application/xml`, `application/json`

<a name="placeOrder"></a>
# **placeOrder**
```java
Mono<Order> StoreController.placeOrder(order)
```

Place an order for a pet



### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
**order** | [**Order**](../../docs/models/Order.md) | order placed for purchasing the pet |

### Return type
[**Order**](../../docs/models/Order.md)


### HTTP request headers
 - **Accepts Content-Type**: `application/json`
 - **Produces Content-Type**: `application/xml`, `application/json`

