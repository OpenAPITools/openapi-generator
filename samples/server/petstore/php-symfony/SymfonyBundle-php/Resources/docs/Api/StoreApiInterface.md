# OpenAPI\Server\Api\StoreApiInterface

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**deleteOrder**](StoreApiInterface.md#deleteOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**getInventory**](StoreApiInterface.md#getInventory) | **GET** /store/inventory | Returns pet inventories by status
[**getOrderById**](StoreApiInterface.md#getOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID
[**placeOrder**](StoreApiInterface.md#placeOrder) | **POST** /store/order | Place an order for a pet


## Service Declaration
```yaml
# src/Acme/MyBundle/Resources/services.yml
services:
    # ...
    acme.my_bundle.api.store:
        class: Acme\MyBundle\Api\StoreApi
        tags:
            - { name: "open_api_server.api", api: "store" }
    # ...
```

## **deleteOrder**
> deleteOrder($orderId)

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/StoreApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\StoreApiInterface;

class StoreApi implements StoreApiInterface
{

    // ...

    /**
     * Implementation of StoreApiInterface#deleteOrder
     */
    public function deleteOrder($orderId)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **string**| ID of the order that needs to be deleted |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **getInventory**
> int getInventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/StoreApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\StoreApiInterface;

class StoreApi implements StoreApiInterface
{

    /**
     * Configure API key authorization: api_key
     */
    public function setapi_key($apiKey)
    {
        // Retrieve logged in user from $apiKey ...
    }

    // ...

    /**
     * Implementation of StoreApiInterface#getInventory
     */
    public function getInventory()
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**int**

### Authorization

[api_key](../../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **getOrderById**
> OpenAPI\Server\Model\Order getOrderById($orderId)

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/StoreApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\StoreApiInterface;

class StoreApi implements StoreApiInterface
{

    // ...

    /**
     * Implementation of StoreApiInterface#getOrderById
     */
    public function getOrderById($orderId)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **int**| ID of pet that needs to be fetched |

### Return type

[**OpenAPI\Server\Model\Order**](../Model/Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **placeOrder**
> OpenAPI\Server\Model\Order placeOrder($order)

Place an order for a pet



### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/StoreApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\StoreApiInterface;

class StoreApi implements StoreApiInterface
{

    // ...

    /**
     * Implementation of StoreApiInterface#placeOrder
     */
    public function placeOrder(Order $order)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order** | [**OpenAPI\Server\Model\Order**](../Model/Order.md)| order placed for purchasing the pet |

### Return type

[**OpenAPI\Server\Model\Order**](../Model/Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

