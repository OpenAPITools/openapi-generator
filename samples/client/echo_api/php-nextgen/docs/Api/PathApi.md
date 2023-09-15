# OpenAPI\Client\PathApi

All URIs are relative to http://localhost:3000, except if the operation defines another base path.

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testsPathStringPathStringIntegerPathInteger()**](PathApi.md#testsPathStringPathStringIntegerPathInteger) | **GET** /path/string/{path_string}/integer/{path_integer} | Test path parameter(s) |


## `testsPathStringPathStringIntegerPathInteger()`

```php
testsPathStringPathStringIntegerPathInteger($path_string, $path_integer): string
```

Test path parameter(s)

Test path parameter(s)

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\PathApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$path_string = 'path_string_example'; // string
$path_integer = 56; // int

try {
    $result = $apiInstance->testsPathStringPathStringIntegerPathInteger($path_string, $path_integer);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling PathApi->testsPathStringPathStringIntegerPathInteger: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **path_string** | **string**|  | |
| **path_integer** | **int**|  | |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `text/plain`

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)
