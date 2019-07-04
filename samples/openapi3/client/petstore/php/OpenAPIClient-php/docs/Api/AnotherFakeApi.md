# OpenAPI\Client\AnotherFakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**call123TestSpecialTags**](AnotherFakeApi.md#call123TestSpecialTags) | **PATCH** /another-fake/dummy | To test special tags



## call123TestSpecialTags

> \OpenAPI\Client\Model\Client call123TestSpecialTags($client)

To test special tags

To test special tags and operation ID starting with number

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');


$apiInstance = new OpenAPI\Client\Api\AnotherFakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$client = new \OpenAPI\Client\Model\Client(); // \OpenAPI\Client\Model\Client | client model

try {
    $result = $apiInstance->call123TestSpecialTags($client);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling AnotherFakeApi->call123TestSpecialTags: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | [**\OpenAPI\Client\Model\Client**](../Model/Client.md)| client model |

### Return type

[**\OpenAPI\Client\Model\Client**](../Model/Client.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../../README.md#documentation-for-models)
[[Back to README]](../../README.md)

