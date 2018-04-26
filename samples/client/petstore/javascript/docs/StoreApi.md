# SwaggerPetstore.StoreApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**deleteOrder**](StoreApi.md#deleteOrder) | **DELETE** /store/order/{order_id} | Delete purchase order by ID
[**getInventory**](StoreApi.md#getInventory) | **GET** /store/inventory | Returns pet inventories by status
[**getOrderById**](StoreApi.md#getOrderById) | **GET** /store/order/{order_id} | Find purchase order by ID
[**placeOrder**](StoreApi.md#placeOrder) | **POST** /store/order | Place an order for a pet


<a name="deleteOrder"></a>
# **deleteOrder**
> deleteOrder(orderId)

Delete purchase order by ID

For valid response try integer IDs with value &lt; 1000. Anything above 1000 or nonintegers will generate API errors

### Example
```javascript
var SwaggerPetstore = require('swagger_petstore');

var apiInstance = new SwaggerPetstore.StoreApi();

var orderId = "orderId_example"; // String | ID of the order that needs to be deleted


var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
};
apiInstance.deleteOrder(orderId, callback);
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

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

<a name="getInventory"></a>
# **getInventory**
> {&#39;String&#39;: &#39;Number&#39;} getInventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```javascript
var SwaggerPetstore = require('swagger_petstore');
var defaultClient = SwaggerPetstore.ApiClient.instance;

// Configure API key authorization: api_key
var api_key = defaultClient.authentications['api_key'];
api_key.apiKey = 'YOUR API KEY';
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//api_key.apiKeyPrefix = 'Token';

var apiInstance = new SwaggerPetstore.StoreApi();

var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
apiInstance.getInventory(callback);
```

### Parameters
This endpoint does not need any parameter.

### Return type

**{&#39;String&#39;: &#39;Number&#39;}**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

<a name="getOrderById"></a>
# **getOrderById**
> Order getOrderById(orderId)

Find purchase order by ID

For valid response try integer IDs with value &lt;&#x3D; 5 or &gt; 10. Other values will generated exceptions

### Example
```javascript
var SwaggerPetstore = require('swagger_petstore');

var apiInstance = new SwaggerPetstore.StoreApi();

var orderId = 789; // Number | ID of pet that needs to be fetched


var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
apiInstance.getOrderById(orderId, callback);
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **Number**| ID of pet that needs to be fetched | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

<a name="placeOrder"></a>
# **placeOrder**
> Order placeOrder(body)

Place an order for a pet



### Example
```javascript
var SwaggerPetstore = require('swagger_petstore');

var apiInstance = new SwaggerPetstore.StoreApi();

var body = new SwaggerPetstore.Order(); // Order | order placed for purchasing the pet


var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
apiInstance.placeOrder(body, callback);
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Order**](Order.md)| order placed for purchasing the pet | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

