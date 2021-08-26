# StoreApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**deleteOrder**](StoreApi.md#deleteOrder) | **DELETE** /store/order/{order_id} | Delete purchase order by ID
[**getInventory**](StoreApi.md#getInventory) | **GET** /store/inventory | Returns pet inventories by status
[**getOrderById**](StoreApi.md#getOrderById) | **GET** /store/order/{order_id} | Find purchase order by ID
[**placeOrder**](StoreApi.md#placeOrder) | **POST** /store/order | Place an order for a pet


## Creating StoreApi

To initiate an instance of `StoreApi`, you can use micronaut's `ApplicationContext`:
```java
/* imports
import io.micronaut.runtime.Micronaut;
import io.micronaut.context.ApplicationContext;
import org.openapitools.api.StoreApi;
*/

ApplicationContext context = Micronaut.run(/* ... */);
PetApi apiInstance = context.getBean(StoreApi.class);
```

Or the `@Inject` annotation:
```java
@Singleton
class MyClass {
    @Inject
    StoreApi storeApi;

    /* ... use the injected variable */
}
```
Note that the class needs to be annotated with one of Micronaut's [scope annotations](https://docs.micronaut.io/latest/guide/#scopes) like `Singleton` in order to be processed.

More information can be found inside [Inversion of Control guide section](https://docs.micronaut.io/latest/guide/#ioc).

<a name="deleteOrder"></a>
# **deleteOrder**
```java
Mono<Void> StoreApi.deleteOrder(orderId)
```

Delete purchase order by ID

For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | `String`| ID of the order that needs to be deleted |






### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="getInventory"></a>
# **getInventory**
```java
Mono<Map<String, Integer>> StoreApi.getInventory()
```

Returns pet inventories by status

Returns a map of status codes to quantities



### Return type
`Map&lt;String, Integer&gt;`

### Authorization
* **[api_key](auth.md#api_key)**

### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: `application/json`

<a name="getOrderById"></a>
# **getOrderById**
```java
Mono<Order> StoreApi.getOrderById(orderId)
```

Find purchase order by ID

For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | `Long`| ID of pet that needs to be fetched |


### Return type
[**Order**](Order.md)



### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: `application/xml`, `application/json`

<a name="placeOrder"></a>
# **placeOrder**
```java
Mono<Order> StoreApi.placeOrder(_body)
```

Place an order for a pet

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_body** | [**Order**](Order.md)| order placed for purchasing the pet |


### Return type
[**Order**](Order.md)



### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: `application/xml`, `application/json`

