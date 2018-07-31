# SwagStoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**deleteOrder**](SwagStoreApi.md#deleteOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**getInventory**](SwagStoreApi.md#getInventory) | **GET** /store/inventory | Returns pet inventories by status
[**getOrderById**](SwagStoreApi.md#getOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID
[**placeOrder**](SwagStoreApi.md#placeOrder) | **POST** /store/order | Place an order for a pet


<a name="deleteOrder"></a>
# **deleteOrder**
> deleteOrder(orderId)

Delete purchase order by ID

For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors

### Example
```java
SwagStoreApi api = new SwagStoreApi();

Map<String, Object> params = new Map<String, Object>{
    'orderId' => 'orderId_example'
};

try {
    // cross your fingers
    api.deleteOrder(params);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **String**| ID of the order that needs to be deleted |

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="getInventory"></a>
# **getInventory**
> Map&lt;String, Integer&gt; getInventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```java
SwagStoreApi api = new SwagStoreApi();
SwagClient client = api.getClient();

// Configure API key authorization: api_key
ApiKeyAuth api_key = (ApiKeyAuth) client.getAuthentication('api_key');
api_key.setApiKey('YOUR API KEY');

try {
    // cross your fingers
    Map<String, Integer> result = api.getInventory();
    System.debug(result);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**Map&lt;String, Integer&gt;**](Map.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="getOrderById"></a>
# **getOrderById**
> SwagOrder getOrderById(orderId)

Find purchase order by ID

For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions

### Example
```java
SwagStoreApi api = new SwagStoreApi();

Map<String, Object> params = new Map<String, Object>{
    'orderId' => 2147483648L
};

try {
    // cross your fingers
    SwagOrder result = api.getOrderById(params);
    System.debug(result);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **Long**| ID of pet that needs to be fetched |

### Return type

[**SwagOrder**](SwagOrder.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

<a name="placeOrder"></a>
# **placeOrder**
> SwagOrder placeOrder(body)

Place an order for a pet



### Example
```java
SwagStoreApi api = new SwagStoreApi();

Map<String, Object> params = new Map<String, Object>{
    'body' => SwagOrder.getExample()
};

try {
    // cross your fingers
    SwagOrder result = api.placeOrder(params);
    System.debug(result);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**SwagOrder**](Order.md)| order placed for purchasing the pet |

### Return type

[**SwagOrder**](SwagOrder.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

