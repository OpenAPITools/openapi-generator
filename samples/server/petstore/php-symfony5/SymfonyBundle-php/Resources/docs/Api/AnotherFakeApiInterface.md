# OpenAPI\Server\Api\AnotherFakeApiInterface

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**call123TestSpecialTags**](AnotherFakeApiInterface.md#call123TestSpecialTags) | **PATCH** /another-fake/dummy | To test special tags


## Service Declaration
```yaml
# src/Acme/MyBundle/Resources/services.yml
services:
    # ...
    acme.my_bundle.api.anotherFake:
        class: Acme\MyBundle\Api\AnotherFakeApi
        tags:
            - { name: "open_api_server.api", api: "anotherFake" }
    # ...
```

## **call123TestSpecialTags**
> OpenAPI\Server\Model\Client call123TestSpecialTags($client)

To test special tags

To test special tags and operation ID starting with number

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/AnotherFakeApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\AnotherFakeApiInterface;

class AnotherFakeApi implements AnotherFakeApiInterface
{

    // ...

    /**
     * Implementation of AnotherFakeApiInterface#call123TestSpecialTags
     */
    public function call123TestSpecialTags(Client $client)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | [**OpenAPI\Server\Model\Client**](../Model/Client.md)| client model |

### Return type

[**OpenAPI\Server\Model\Client**](../Model/Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

