# OpenAPI\Client\FakeClassnameTags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testClassname**](FakeClassnameTags123Api.md#testClassname) | **PATCH** /fake_classname_test | To test class name in snake case



## testClassname

> \OpenAPI\Client\Model\Client testClassname($client)

To test class name in snake case

To test class name in snake case

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');


// Configure API key authorization: api_key_query
$config = OpenAPI\Client\Configuration::getDefaultConfiguration()->setApiKey('api_key_query', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
// $config = OpenAPI\Client\Configuration::getDefaultConfiguration()->setApiKeyPrefix('api_key_query', 'Bearer');


$apiInstance = new OpenAPI\Client\Api\FakeClassnameTags123Api(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);
$client = new \OpenAPI\Client\Model\Client(); // \OpenAPI\Client\Model\Client | client model

try {
    $result = $apiInstance->testClassname($client);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeClassnameTags123Api->testClassname: ', $e->getMessage(), PHP_EOL;
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

[api_key_query](../../README.md#api_key_query)

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../../README.md#documentation-for-models)
[[Back to README]](../../README.md)

