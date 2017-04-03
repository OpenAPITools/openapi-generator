# Swagger\Client\FakeApi

All URIs are relative to *https://petstore.swagger.io *_/ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r/v2 *_/ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testCodeInjectEndRnNR**](FakeApi.md#testCodeInjectEndRnNR) | **PUT** /fake | To test code injection *_/ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r


# **testCodeInjectEndRnNR**
> testCodeInjectEndRnNR($test_code_inject____end____rn_n_r)

To test code injection *_/ ' \" =end -- \\r\\n \\n \\r

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\FakeApi(new \Http\Adapter\Guzzle6\Client());
$test_code_inject____end____rn_n_r = "test_code_inject____end____rn_n_r_example"; // string | To test code injection *_/ ' \" =end -- \\r\\n \\n \\r

try {
    $api_instance->testCodeInjectEndRnNR($test_code_inject____end____rn_n_r);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testCodeInjectEndRnNR: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **test_code_inject____end____rn_n_r** | **string**| To test code injection *_/ &#39; \&quot; &#x3D;end -- \\r\\n \\n \\r | [optional]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, *_/  \" =end --
 - **Accept**: application/json, *_/  \" =end --

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

