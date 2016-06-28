# Swagger\Client\FakeApi

All URIs are relative to *https://petstore.swagger.io */ &#39; &quot; &#x3D;end/v2 */ &#39; &quot; &#x3D;end*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testCodeInjectEnd**](FakeApi.md#testCodeInjectEnd) | **PUT** /fake | To test code injection  &#39; \&quot; &#x3D;end


# **testCodeInjectEnd**
> testCodeInjectEnd($test_code_inject____end)

To test code injection  ' \" =end

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\FakeApi();
$test_code_inject____end = "test_code_inject____end_example"; // string | To test code injection  ' \" =end

try {
    $api_instance->testCodeInjectEnd($test_code_inject____end);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testCodeInjectEnd: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **test_code_inject____end** | **string**| To test code injection  &#39; \&quot; &#x3D;end | [optional]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json, */  " =end
 - **Accept**: application/json, */  " =end

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

