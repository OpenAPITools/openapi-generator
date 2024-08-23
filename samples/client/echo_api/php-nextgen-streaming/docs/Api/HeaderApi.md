# OpenAPI\Client\HeaderApi

All URIs are relative to http://localhost:3000, except if the operation defines another base path.

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testHeaderIntegerBooleanStringEnums()**](HeaderApi.md#testHeaderIntegerBooleanStringEnums) | **GET** /header/integer/boolean/string/enums | Test header parameter(s) |


## `testHeaderIntegerBooleanStringEnums()`

```php
testHeaderIntegerBooleanStringEnums($integer_header, $boolean_header, $string_header, $enum_nonref_string_header, $enum_ref_string_header): string
```

Test header parameter(s)

Test header parameter(s)

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\HeaderApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$integer_header = 56; // int
$boolean_header = True; // bool
$string_header = 'string_header_example'; // string
$enum_nonref_string_header = 'enum_nonref_string_header_example'; // string
$enum_ref_string_header = new \OpenAPI\Client\Model\StringEnumRef(); // StringEnumRef

try {
    $result = $apiInstance->testHeaderIntegerBooleanStringEnums($integer_header, $boolean_header, $string_header, $enum_nonref_string_header, $enum_ref_string_header);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling HeaderApi->testHeaderIntegerBooleanStringEnums: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **integer_header** | **int**|  | [optional] |
| **boolean_header** | **bool**|  | [optional] |
| **string_header** | **string**|  | [optional] |
| **enum_nonref_string_header** | **string**|  | [optional] |
| **enum_ref_string_header** | [**StringEnumRef**](../Model/.md)|  | [optional] |

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
