# OpenAPITools\Client\FakeApi

All URIs are relative to *petstore.swagger.io *_/ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r/v2 *_/ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testCodeInjectEndRnNR**](FakeApi.md#testCodeInjectEndRnNR) | **PUT** /fake | To test code injection *_/ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r


# **testCodeInjectEndRnNR**
> testCodeInjectEndRnNR($unknown_base_type)

To test code injection *_/ ' \" =end -- \\r\\n \\n \\r

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$apiInstance = new OpenAPITools\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$unknown_base_type = new \OpenAPITools\Client\Model\UNKNOWN_BASE_TYPE(); // object | 

try {
    $apiInstance->testCodeInjectEndRnNR($unknown_base_type);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testCodeInjectEndRnNR: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **unknown_base_type** | [**object**](../Model/UNKNOWN_BASE_TYPE.md)|  | [optional]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, *_/  \" =end --
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

