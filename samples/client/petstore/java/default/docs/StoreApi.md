# StoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**deleteOrder**](StoreApi.md#deleteOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**findOrdersByStatus**](StoreApi.md#findOrdersByStatus) | **GET** /store/findByStatus | Finds orders by status
[**getInventory**](StoreApi.md#getInventory) | **GET** /store/inventory | Returns pet inventories by status
[**getInventoryInObject**](StoreApi.md#getInventoryInObject) | **GET** /store/inventory?response=arbitrary_object | Fake endpoint to test arbitrary object return by &#39;Get inventory&#39;
[**getOrderById**](StoreApi.md#getOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID
[**placeOrder**](StoreApi.md#placeOrder) | **POST** /store/order | Place an order for a pet


<a name="deleteOrder"></a>
# **deleteOrder**
> deleteOrder(orderId)

Delete purchase order by ID

For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors

### Example
```java
// Import classes:
//import io.swagger.client.ApiException;
//import io.swagger.client.api.StoreApi;


StoreApi apiInstance = new StoreApi();
String orderId = "orderId_example"; // String | ID of the order that needs to be deleted
try {
    apiInstance.deleteOrder(orderId);
} catch (ApiException e) {
    System.err.println("Exception when calling StoreApi#deleteOrder");
    e.printStackTrace();
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

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

<a name="findOrdersByStatus"></a>
# **findOrdersByStatus**
> List&lt;Order&gt; findOrdersByStatus(status)

Finds orders by status

A single status value can be provided as a string

### Example
```java
// Import classes:
//import io.swagger.client.ApiClient;
//import io.swagger.client.ApiException;
//import io.swagger.client.Configuration;
//import io.swagger.client.auth.*;
//import io.swagger.client.api.StoreApi;

ApiClient defaultClient = Configuration.getDefaultApiClient();

// Configure API key authorization: test_api_client_id
ApiKeyAuth test_api_client_id = (ApiKeyAuth) defaultClient.getAuthentication("test_api_client_id");
test_api_client_id.setApiKey("YOUR API KEY");
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//test_api_client_id.setApiKeyPrefix("Token");

// Configure API key authorization: test_api_client_secret
ApiKeyAuth test_api_client_secret = (ApiKeyAuth) defaultClient.getAuthentication("test_api_client_secret");
test_api_client_secret.setApiKey("YOUR API KEY");
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//test_api_client_secret.setApiKeyPrefix("Token");

StoreApi apiInstance = new StoreApi();
String status = "placed"; // String | Status value that needs to be considered for query
try {
    List<Order> result = apiInstance.findOrdersByStatus(status);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling StoreApi#findOrdersByStatus");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | **String**| Status value that needs to be considered for query | [optional] [default to placed]

### Return type

[**List&lt;Order&gt;**](Order.md)

### Authorization

[test_api_client_id](../README.md#test_api_client_id), [test_api_client_secret](../README.md#test_api_client_secret)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

<a name="getInventory"></a>
# **getInventory**
> Map&lt;String, Integer&gt; getInventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```java
// Import classes:
//import io.swagger.client.ApiClient;
//import io.swagger.client.ApiException;
//import io.swagger.client.Configuration;
//import io.swagger.client.auth.*;
//import io.swagger.client.api.StoreApi;

ApiClient defaultClient = Configuration.getDefaultApiClient();

// Configure API key authorization: api_key
ApiKeyAuth api_key = (ApiKeyAuth) defaultClient.getAuthentication("api_key");
api_key.setApiKey("YOUR API KEY");
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//api_key.setApiKeyPrefix("Token");

StoreApi apiInstance = new StoreApi();
try {
    Map<String, Integer> result = apiInstance.getInventory();
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling StoreApi#getInventory");
    e.printStackTrace();
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**Map&lt;String, Integer&gt;**](Map.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

<a name="getInventoryInObject"></a>
# **getInventoryInObject**
> Object getInventoryInObject()

Fake endpoint to test arbitrary object return by &#39;Get inventory&#39;

Returns an arbitrary object which is actually a map of status codes to quantities

### Example
```java
// Import classes:
//import io.swagger.client.ApiClient;
//import io.swagger.client.ApiException;
//import io.swagger.client.Configuration;
//import io.swagger.client.auth.*;
//import io.swagger.client.api.StoreApi;

ApiClient defaultClient = Configuration.getDefaultApiClient();

// Configure API key authorization: api_key
ApiKeyAuth api_key = (ApiKeyAuth) defaultClient.getAuthentication("api_key");
api_key.setApiKey("YOUR API KEY");
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//api_key.setApiKeyPrefix("Token");

StoreApi apiInstance = new StoreApi();
try {
    Object result = apiInstance.getInventoryInObject();
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling StoreApi#getInventoryInObject");
    e.printStackTrace();
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**Object**

### Authorization

[api_key](../README.md#api_key)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

<a name="getOrderById"></a>
# **getOrderById**
> Order getOrderById(orderId)

Find purchase order by ID

For valid response try integer IDs with value &lt;= 5 or &gt; 10. Other values will generated exceptions

### Example
```java
// Import classes:
//import io.swagger.client.ApiClient;
//import io.swagger.client.ApiException;
//import io.swagger.client.Configuration;
//import io.swagger.client.auth.*;
//import io.swagger.client.api.StoreApi;

ApiClient defaultClient = Configuration.getDefaultApiClient();

// Configure API key authorization: test_api_key_query
ApiKeyAuth test_api_key_query = (ApiKeyAuth) defaultClient.getAuthentication("test_api_key_query");
test_api_key_query.setApiKey("YOUR API KEY");
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//test_api_key_query.setApiKeyPrefix("Token");

// Configure API key authorization: test_api_key_header
ApiKeyAuth test_api_key_header = (ApiKeyAuth) defaultClient.getAuthentication("test_api_key_header");
test_api_key_header.setApiKey("YOUR API KEY");
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//test_api_key_header.setApiKeyPrefix("Token");

StoreApi apiInstance = new StoreApi();
String orderId = "orderId_example"; // String | ID of pet that needs to be fetched
try {
    Order result = apiInstance.getOrderById(orderId);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling StoreApi#getOrderById");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **String**| ID of pet that needs to be fetched | 

### Return type

[**Order**](Order.md)

### Authorization

[test_api_key_query](../README.md#test_api_key_query), [test_api_key_header](../README.md#test_api_key_header)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

<a name="placeOrder"></a>
# **placeOrder**
> Order placeOrder(body)

Place an order for a pet



### Example
```java
// Import classes:
//import io.swagger.client.ApiClient;
//import io.swagger.client.ApiException;
//import io.swagger.client.Configuration;
//import io.swagger.client.auth.*;
//import io.swagger.client.api.StoreApi;

ApiClient defaultClient = Configuration.getDefaultApiClient();

// Configure API key authorization: test_api_client_id
ApiKeyAuth test_api_client_id = (ApiKeyAuth) defaultClient.getAuthentication("test_api_client_id");
test_api_client_id.setApiKey("YOUR API KEY");
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//test_api_client_id.setApiKeyPrefix("Token");

// Configure API key authorization: test_api_client_secret
ApiKeyAuth test_api_client_secret = (ApiKeyAuth) defaultClient.getAuthentication("test_api_client_secret");
test_api_client_secret.setApiKey("YOUR API KEY");
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//test_api_client_secret.setApiKeyPrefix("Token");

StoreApi apiInstance = new StoreApi();
Order body = new Order(); // Order | order placed for purchasing the pet
try {
    Order result = apiInstance.placeOrder(body);
    System.out.println(result);
} catch (ApiException e) {
    System.err.println("Exception when calling StoreApi#placeOrder");
    e.printStackTrace();
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Order**](Order.md)| order placed for purchasing the pet | [optional] 

### Return type

[**Order**](Order.md)

### Authorization

[test_api_client_id](../README.md#test_api_client_id), [test_api_client_secret](../README.md#test_api_client_secret)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

