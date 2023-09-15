# OpenAPI\Client\FormApi

All URIs are relative to http://localhost:3000, except if the operation defines another base path.

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testFormIntegerBooleanString()**](FormApi.md#testFormIntegerBooleanString) | **POST** /form/integer/boolean/string | Test form parameter(s) |
| [**testFormOneof()**](FormApi.md#testFormOneof) | **POST** /form/oneof | Test form parameter(s) for oneOf schema |


## `testFormIntegerBooleanString()`

```php
testFormIntegerBooleanString($integer_form, $boolean_form, $string_form): string
```

Test form parameter(s)

Test form parameter(s)

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\FormApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$integer_form = 56; // int
$boolean_form = True; // bool
$string_form = 'string_form_example'; // string

try {
    $result = $apiInstance->testFormIntegerBooleanString($integer_form, $boolean_form, $string_form);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FormApi->testFormIntegerBooleanString: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **integer_form** | **int**|  | [optional] |
| **boolean_form** | **bool**|  | [optional] |
| **string_form** | **string**|  | [optional] |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/x-www-form-urlencoded`
- **Accept**: `text/plain`

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `testFormOneof()`

```php
testFormOneof($form1, $form2, $form3, $form4, $id, $name): string
```

Test form parameter(s) for oneOf schema

Test form parameter(s) for oneOf schema

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\FormApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$form1 = 'form1_example'; // string
$form2 = 56; // int
$form3 = 'form3_example'; // string
$form4 = True; // bool
$id = 56; // int
$name = 'name_example'; // string

try {
    $result = $apiInstance->testFormOneof($form1, $form2, $form3, $form4, $id, $name);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FormApi->testFormOneof: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **form1** | **string**|  | [optional] |
| **form2** | **int**|  | [optional] |
| **form3** | **string**|  | [optional] |
| **form4** | **bool**|  | [optional] |
| **id** | **int**|  | [optional] |
| **name** | **string**|  | [optional] |

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: `application/x-www-form-urlencoded`
- **Accept**: `text/plain`

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)
