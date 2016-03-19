# SwaggerPetstore.StoreApi

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
```javascript
var SwaggerPetstore = require('swagger-petstore');

var api = new SwaggerPetstore.StoreApi()

var orderId = "orderId_example"; // {String} ID of the order that needs to be deleted


var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
};
api.deleteOrder(orderId, callback);
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
> [Order] findOrdersByStatus(opts)

Finds orders by status

A single status value can be provided as a string

### Example
```javascript
var SwaggerPetstore = require('swagger-petstore');
var defaultClient = SwaggerPetstore.ApiClient.default;

// Configure API key authorization: test_api_client_id
var test_api_client_id = defaultClient.authentications['test_api_client_id'];
test_api_client_id.apiKey = "YOUR API KEY"
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//test_api_client_id.apiKeyPrefix['x-test_api_client_id'] = "Token"

// Configure API key authorization: test_api_client_secret
var test_api_client_secret = defaultClient.authentications['test_api_client_secret'];
test_api_client_secret.apiKey = "YOUR API KEY"
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//test_api_client_secret.apiKeyPrefix['x-test_api_client_secret'] = "Token"

var api = new SwaggerPetstore.StoreApi()

var opts = { 
  'status': "placed" // {String} Status value that needs to be considered for query
};

var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
api.findOrdersByStatus(opts, callback);
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | **String**| Status value that needs to be considered for query | [optional] [default to placed]

### Return type

[**[Order]**](Order.md)

### Authorization

[test_api_client_id](../README.md#test_api_client_id), [test_api_client_secret](../README.md#test_api_client_secret)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

<a name="getInventory"></a>
# **getInventory**
> {&#39;String&#39;: &#39;Integer&#39;} getInventory

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```javascript
var SwaggerPetstore = require('swagger-petstore');
var defaultClient = SwaggerPetstore.ApiClient.default;

// Configure API key authorization: api_key
var api_key = defaultClient.authentications['api_key'];
api_key.apiKey = "YOUR API KEY"
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//api_key.apiKeyPrefix['api_key'] = "Token"

var api = new SwaggerPetstore.StoreApi()

var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
api.getInventory(callback);
```

### Parameters
This endpoint does not need any parameter.

### Return type

**{&#39;String&#39;: &#39;Integer&#39;}**

### Authorization

[api_key](../README.md#api_key)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

<a name="getInventoryInObject"></a>
# **getInventoryInObject**
> Object getInventoryInObject

Fake endpoint to test arbitrary object return by &#39;Get inventory&#39;

Returns an arbitrary object which is actually a map of status codes to quantities

### Example
```javascript
var SwaggerPetstore = require('swagger-petstore');
var defaultClient = SwaggerPetstore.ApiClient.default;

// Configure API key authorization: api_key
var api_key = defaultClient.authentications['api_key'];
api_key.apiKey = "YOUR API KEY"
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//api_key.apiKeyPrefix['api_key'] = "Token"

var api = new SwaggerPetstore.StoreApi()

var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
api.getInventoryInObject(callback);
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
```javascript
var SwaggerPetstore = require('swagger-petstore');
var defaultClient = SwaggerPetstore.ApiClient.default;

// Configure API key authorization: test_api_key_header
var test_api_key_header = defaultClient.authentications['test_api_key_header'];
test_api_key_header.apiKey = "YOUR API KEY"
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//test_api_key_header.apiKeyPrefix['test_api_key_header'] = "Token"

// Configure API key authorization: test_api_key_query
var test_api_key_query = defaultClient.authentications['test_api_key_query'];
test_api_key_query.apiKey = "YOUR API KEY"
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//test_api_key_query.apiKeyPrefix['test_api_key_query'] = "Token"

var api = new SwaggerPetstore.StoreApi()

var orderId = "orderId_example"; // {String} ID of pet that needs to be fetched


var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
api.getOrderById(orderId, callback);
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **String**| ID of pet that needs to be fetched | 

### Return type

[**Order**](Order.md)

### Authorization

[test_api_key_header](../README.md#test_api_key_header), [test_api_key_query](../README.md#test_api_key_query)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

<a name="placeOrder"></a>
# **placeOrder**
> Order placeOrder(opts)

Place an order for a pet



### Example
```javascript
var SwaggerPetstore = require('swagger-petstore');
var defaultClient = SwaggerPetstore.ApiClient.default;

// Configure API key authorization: test_api_client_id
var test_api_client_id = defaultClient.authentications['test_api_client_id'];
test_api_client_id.apiKey = "YOUR API KEY"
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//test_api_client_id.apiKeyPrefix['x-test_api_client_id'] = "Token"

// Configure API key authorization: test_api_client_secret
var test_api_client_secret = defaultClient.authentications['test_api_client_secret'];
test_api_client_secret.apiKey = "YOUR API KEY"
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//test_api_client_secret.apiKeyPrefix['x-test_api_client_secret'] = "Token"

var api = new SwaggerPetstore.StoreApi()

var opts = { 
  'body': new SwaggerPetstore.Order() // {Order} order placed for purchasing the pet
};

var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
api.placeOrder(opts, callback);
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

