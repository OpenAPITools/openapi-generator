# \StoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**delete_order**](StoreApi.md#delete_order) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**find_orders_by_status**](StoreApi.md#find_orders_by_status) | **GET** /store/findByStatus | Finds orders by status
[**get_inventory**](StoreApi.md#get_inventory) | **GET** /store/inventory | Returns pet inventories by status
[**get_inventory_in_object**](StoreApi.md#get_inventory_in_object) | **GET** /store/inventory?response=arbitrary_object | Fake endpoint to test arbitrary object return by &#39;Get inventory&#39;
[**get_order_by_id**](StoreApi.md#get_order_by_id) | **GET** /store/order/{orderId} | Find purchase order by ID
[**place_order**](StoreApi.md#place_order) | **POST** /store/order | Place an order for a pet


# **delete_order**
> delete_order($order_id)

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example 
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new \Api\StoreApi();
$order_id = order_id_example; // str | ID of the order that needs to be deleted

try { 
    $api_instance->delete_order($order_id);
} catch (Exception $e) {
    echo 'Exception when calling StoreApi->delete_order: ', $e->getMessage(), "\n";
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order_id** | **str**| ID of the order that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **find_orders_by_status**
> list[Order] find_orders_by_status($status)

Finds orders by status

A single status value can be provided as a string

### Example 
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

// Configure API key authorization: test_api_client_id
\Configuration::getDefaultConfiguration()->setApiKey('x-test_api_client_id', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
// \Configuration::getDefaultConfiguration()->setApiKeyPrefix('x-test_api_client_id', 'BEARER');
// Configure API key authorization: test_api_client_secret
\Configuration::getDefaultConfiguration()->setApiKey('x-test_api_client_secret', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
// \Configuration::getDefaultConfiguration()->setApiKeyPrefix('x-test_api_client_secret', 'BEARER');

$api_instance = new \Api\StoreApi();
$status = placed; // str | Status value that needs to be considered for query

try { 
    $result = $api_instance->find_orders_by_status($status);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling StoreApi->find_orders_by_status: ', $e->getMessage(), "\n";
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | **str**| Status value that needs to be considered for query | [optional] [default to placed]

### Return type

[**list[Order]**](Order.md)

### Authorization

[test_api_client_id](../README.md#test_api_client_id), [test_api_client_secret](../README.md#test_api_client_secret)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_inventory**
> dict(str, int) get_inventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example 
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

// Configure API key authorization: api_key
\Configuration::getDefaultConfiguration()->setApiKey('api_key', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
// \Configuration::getDefaultConfiguration()->setApiKeyPrefix('api_key', 'BEARER');

$api_instance = new \Api\StoreApi();

try { 
    $result = $api_instance->get_inventory();
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling StoreApi->get_inventory: ', $e->getMessage(), "\n";
}
?>
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**dict(str, int)**](dict.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_inventory_in_object**
> object get_inventory_in_object()

Fake endpoint to test arbitrary object return by 'Get inventory'

Returns an arbitrary object which is actually a map of status codes to quantities

### Example 
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

// Configure API key authorization: api_key
\Configuration::getDefaultConfiguration()->setApiKey('api_key', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
// \Configuration::getDefaultConfiguration()->setApiKeyPrefix('api_key', 'BEARER');

$api_instance = new \Api\StoreApi();

try { 
    $result = $api_instance->get_inventory_in_object();
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling StoreApi->get_inventory_in_object: ', $e->getMessage(), "\n";
}
?>
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**object**](object.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_order_by_id**
> Order get_order_by_id($order_id)

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example 
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

// Configure API key authorization: test_api_key_header
\Configuration::getDefaultConfiguration()->setApiKey('test_api_key_header', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
// \Configuration::getDefaultConfiguration()->setApiKeyPrefix('test_api_key_header', 'BEARER');
// Configure API key authorization: test_api_key_query
\Configuration::getDefaultConfiguration()->setApiKey('test_api_key_query', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
// \Configuration::getDefaultConfiguration()->setApiKeyPrefix('test_api_key_query', 'BEARER');

$api_instance = new \Api\StoreApi();
$order_id = order_id_example; // str | ID of pet that needs to be fetched

try { 
    $result = $api_instance->get_order_by_id($order_id);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling StoreApi->get_order_by_id: ', $e->getMessage(), "\n";
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order_id** | **str**| ID of pet that needs to be fetched | 

### Return type

[**Order**](Order.md)

### Authorization

[test_api_key_header](../README.md#test_api_key_header), [test_api_key_query](../README.md#test_api_key_query)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **place_order**
> Order place_order($body)

Place an order for a pet



### Example 
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

// Configure API key authorization: test_api_client_id
\Configuration::getDefaultConfiguration()->setApiKey('x-test_api_client_id', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
// \Configuration::getDefaultConfiguration()->setApiKeyPrefix('x-test_api_client_id', 'BEARER');
// Configure API key authorization: test_api_client_secret
\Configuration::getDefaultConfiguration()->setApiKey('x-test_api_client_secret', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
// \Configuration::getDefaultConfiguration()->setApiKeyPrefix('x-test_api_client_secret', 'BEARER');

$api_instance = new \Api\StoreApi();
$body = new Order(); // Order | order placed for purchasing the pet

try { 
    $result = $api_instance->place_order($body);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling StoreApi->place_order: ', $e->getMessage(), "\n";
}
?>
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

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

