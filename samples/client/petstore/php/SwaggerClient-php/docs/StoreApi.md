# Swagger\Client\StoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**deleteOrder**](StoreApi.md#deleteOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**findOrdersByStatus**](StoreApi.md#findOrdersByStatus) | **GET** /store/findByStatus | Finds orders by status
[**getInventory**](StoreApi.md#getInventory) | **GET** /store/inventory | Returns pet inventories by status
[**getInventoryInObject**](StoreApi.md#getInventoryInObject) | **GET** /store/inventory?response=arbitrary_object | Fake endpoint to test arbitrary object return by &#39;Get inventory&#39;
[**getOrderById**](StoreApi.md#getOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID
[**placeOrder**](StoreApi.md#placeOrder) | **POST** /store/order | Place an order for a pet


# **deleteOrder**
> deleteOrder($order_id)

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example 
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\StoreApi();
$order_id = "order_id_example"; // string | ID of the order that needs to be deleted

try { 
    $api_instance->deleteOrder($order_id);
} catch (Exception $e) {
    echo 'Exception when calling StoreApi->deleteOrder: ', $e->getMessage(), "\n";
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order_id** | **string**| ID of the order that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **findOrdersByStatus**
> \Swagger\Client\Model\Order[] findOrdersByStatus($status)

Finds orders by status

A single status value can be provided as a string

### Example 
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

// Configure API key authorization: test_api_client_id
Swagger\Client\Configuration::getDefaultConfiguration()->setApiKey('x-test_api_client_id', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
// Swagger\Client\Configuration::getDefaultConfiguration()->setApiKeyPrefix('x-test_api_client_id', 'BEARER');
// Configure API key authorization: test_api_client_secret
Swagger\Client\Configuration::getDefaultConfiguration()->setApiKey('x-test_api_client_secret', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
// Swagger\Client\Configuration::getDefaultConfiguration()->setApiKeyPrefix('x-test_api_client_secret', 'BEARER');

$api_instance = new Swagger\Client\Api\StoreApi();
$status = "placed"; // string | Status value that needs to be considered for query

try { 
    $result = $api_instance->findOrdersByStatus($status);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling StoreApi->findOrdersByStatus: ', $e->getMessage(), "\n";
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | **string**| Status value that needs to be considered for query | [optional] [default to placed]

### Return type

[**\Swagger\Client\Model\Order[]**](Order.md)

### Authorization

[test_api_client_id](../README.md#test_api_client_id), [test_api_client_secret](../README.md#test_api_client_secret)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getInventory**
> map[string,int] getInventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example 
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

// Configure API key authorization: api_key
Swagger\Client\Configuration::getDefaultConfiguration()->setApiKey('api_key', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
// Swagger\Client\Configuration::getDefaultConfiguration()->setApiKeyPrefix('api_key', 'BEARER');

$api_instance = new Swagger\Client\Api\StoreApi();

try { 
    $result = $api_instance->getInventory();
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling StoreApi->getInventory: ', $e->getMessage(), "\n";
}
?>
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**map[string,int]**](map.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getInventoryInObject**
> object getInventoryInObject()

Fake endpoint to test arbitrary object return by 'Get inventory'

Returns an arbitrary object which is actually a map of status codes to quantities

### Example 
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

// Configure API key authorization: api_key
Swagger\Client\Configuration::getDefaultConfiguration()->setApiKey('api_key', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
// Swagger\Client\Configuration::getDefaultConfiguration()->setApiKeyPrefix('api_key', 'BEARER');

$api_instance = new Swagger\Client\Api\StoreApi();

try { 
    $result = $api_instance->getInventoryInObject();
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling StoreApi->getInventoryInObject: ', $e->getMessage(), "\n";
}
?>
```

### Parameters
This endpoint does not need any parameter.

### Return type

**object**

### Authorization

[api_key](../README.md#api_key)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getOrderById**
> \Swagger\Client\Model\Order getOrderById($order_id)

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example 
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

// Configure API key authorization: test_api_key_header
Swagger\Client\Configuration::getDefaultConfiguration()->setApiKey('test_api_key_header', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
// Swagger\Client\Configuration::getDefaultConfiguration()->setApiKeyPrefix('test_api_key_header', 'BEARER');
// Configure API key authorization: test_api_key_query
Swagger\Client\Configuration::getDefaultConfiguration()->setApiKey('test_api_key_query', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
// Swagger\Client\Configuration::getDefaultConfiguration()->setApiKeyPrefix('test_api_key_query', 'BEARER');

$api_instance = new Swagger\Client\Api\StoreApi();
$order_id = "order_id_example"; // string | ID of pet that needs to be fetched

try { 
    $result = $api_instance->getOrderById($order_id);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling StoreApi->getOrderById: ', $e->getMessage(), "\n";
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order_id** | **string**| ID of pet that needs to be fetched | 

### Return type

[**\Swagger\Client\Model\Order**](Order.md)

### Authorization

[test_api_key_header](../README.md#test_api_key_header), [test_api_key_query](../README.md#test_api_key_query)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **placeOrder**
> \Swagger\Client\Model\Order placeOrder($body)

Place an order for a pet



### Example 
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

// Configure API key authorization: test_api_client_id
Swagger\Client\Configuration::getDefaultConfiguration()->setApiKey('x-test_api_client_id', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
// Swagger\Client\Configuration::getDefaultConfiguration()->setApiKeyPrefix('x-test_api_client_id', 'BEARER');
// Configure API key authorization: test_api_client_secret
Swagger\Client\Configuration::getDefaultConfiguration()->setApiKey('x-test_api_client_secret', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
// Swagger\Client\Configuration::getDefaultConfiguration()->setApiKeyPrefix('x-test_api_client_secret', 'BEARER');

$api_instance = new Swagger\Client\Api\StoreApi();
$body = new \Swagger\Client\Model\Order(); // \Swagger\Client\Model\Order | order placed for purchasing the pet

try { 
    $result = $api_instance->placeOrder($body);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling StoreApi->placeOrder: ', $e->getMessage(), "\n";
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**\Swagger\Client\Model\Order**](\Swagger\Client\Model\Order.md)| order placed for purchasing the pet | [optional] 

### Return type

[**\Swagger\Client\Model\Order**](Order.md)

### Authorization

[test_api_client_id](../README.md#test_api_client_id), [test_api_client_secret](../README.md#test_api_client_secret)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

