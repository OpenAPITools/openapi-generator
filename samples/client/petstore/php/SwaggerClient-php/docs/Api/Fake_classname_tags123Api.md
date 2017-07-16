# Swagger\Client\Fake_classname_tags123Api

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testClassname**](Fake_classname_tags123Api.md#testClassname) | **PATCH** /fake_classname_test | To test class name in snake case


# **testClassname**
> \Swagger\Client\Model\Client testClassname($body)

To test class name in snake case

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\Fake_classname_tags123Api(new \Http\Adapter\Guzzle6\Client());
$body = new \Swagger\Client\Model\Client(); // \Swagger\Client\Model\Client | client model

try {
    $result = $api_instance->testClassname($body);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling Fake_classname_tags123Api->testClassname: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**\Swagger\Client\Model\Client**](../Model/Client.md)| client model |

### Return type

[**\Swagger\Client\Model\Client**](../Model/Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

