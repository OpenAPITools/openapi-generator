# OpenAPI\Client\PathApi

All URIs are relative to http://localhost:3000, except if the operation defines another base path.

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath()**](PathApi.md#testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath) | **GET** /path/string/{path_string}/integer/{path_integer}/{enum_nonref_string_path}/{enum_ref_string_path} | Test path parameter(s) |


## `testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath()`

```php
testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath($path_string, $path_integer, $enum_nonref_string_path, $enum_ref_string_path): string
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
$enum_nonref_string_path = 'enum_nonref_string_path_example'; // string
$enum_ref_string_path = new \OpenAPI\Client\Model\StringEnumRef(); // StringEnumRef

try {
    $result = $apiInstance->testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath($path_string, $path_integer, $enum_nonref_string_path, $enum_ref_string_path);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling PathApi->testsPathStringPathStringIntegerPathIntegerEnumNonrefStringPathEnumRefStringPath: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **path_string** | **string**|  | |
| **path_integer** | **int**|  | |
| **enum_nonref_string_path** | **string**|  | |
| **enum_ref_string_path** | [**StringEnumRef**](../Model/.md)|  | |

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
