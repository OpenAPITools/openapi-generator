# OpenAPI\Client\QueryApi

All URIs are relative to http://localhost:3000, except if the operation defines another base path.

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testEnumRefString()**](QueryApi.md#testEnumRefString) | **GET** /query/enum_ref_string | Test query parameter(s) |
| [**testQueryDatetimeDateString()**](QueryApi.md#testQueryDatetimeDateString) | **GET** /query/datetime/date/string | Test query parameter(s) |
| [**testQueryIntegerBooleanString()**](QueryApi.md#testQueryIntegerBooleanString) | **GET** /query/integer/boolean/string | Test query parameter(s) |
| [**testQueryStyleDeepObjectExplodeTrueObject()**](QueryApi.md#testQueryStyleDeepObjectExplodeTrueObject) | **GET** /query/style_deepObject/explode_true/object | Test query parameter(s) |
| [**testQueryStyleDeepObjectExplodeTrueObjectAllOf()**](QueryApi.md#testQueryStyleDeepObjectExplodeTrueObjectAllOf) | **GET** /query/style_deepObject/explode_true/object/allOf | Test query parameter(s) |
| [**testQueryStyleFormExplodeFalseArrayInteger()**](QueryApi.md#testQueryStyleFormExplodeFalseArrayInteger) | **GET** /query/style_form/explode_false/array_integer | Test query parameter(s) |
| [**testQueryStyleFormExplodeFalseArrayString()**](QueryApi.md#testQueryStyleFormExplodeFalseArrayString) | **GET** /query/style_form/explode_false/array_string | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueArrayString()**](QueryApi.md#testQueryStyleFormExplodeTrueArrayString) | **GET** /query/style_form/explode_true/array_string | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueObject()**](QueryApi.md#testQueryStyleFormExplodeTrueObject) | **GET** /query/style_form/explode_true/object | Test query parameter(s) |
| [**testQueryStyleFormExplodeTrueObjectAllOf()**](QueryApi.md#testQueryStyleFormExplodeTrueObjectAllOf) | **GET** /query/style_form/explode_true/object/allOf | Test query parameter(s) |


## `testEnumRefString()`

```php
testEnumRefString($enum_nonref_string_query, $enum_ref_string_query): string
```

Test query parameter(s)

Test query parameter(s)

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\QueryApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$enum_nonref_string_query = 'enum_nonref_string_query_example'; // string
$enum_ref_string_query = new \OpenAPI\Client\Model\\OpenAPI\Client\Model\StringEnumRef(); // \OpenAPI\Client\Model\StringEnumRef

try {
    $result = $apiInstance->testEnumRefString($enum_nonref_string_query, $enum_ref_string_query);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling QueryApi->testEnumRefString: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **enum_nonref_string_query** | **string**|  | [optional] |
| **enum_ref_string_query** | [**\OpenAPI\Client\Model\StringEnumRef**](../Model/.md)|  | [optional] |

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

## `testQueryDatetimeDateString()`

```php
testQueryDatetimeDateString($datetime_query, $date_query, $string_query): string
```

Test query parameter(s)

Test query parameter(s)

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\QueryApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$datetime_query = new \DateTime('2013-10-20T19:20:30+01:00'); // \DateTime
$date_query = new \DateTime('2013-10-20T19:20:30+01:00'); // \DateTime
$string_query = 'string_query_example'; // string

try {
    $result = $apiInstance->testQueryDatetimeDateString($datetime_query, $date_query, $string_query);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling QueryApi->testQueryDatetimeDateString: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **datetime_query** | **\DateTime**|  | [optional] |
| **date_query** | **\DateTime**|  | [optional] |
| **string_query** | **string**|  | [optional] |

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

## `testQueryIntegerBooleanString()`

```php
testQueryIntegerBooleanString($integer_query, $boolean_query, $string_query): string
```

Test query parameter(s)

Test query parameter(s)

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\QueryApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$integer_query = 56; // int
$boolean_query = True; // bool
$string_query = 'string_query_example'; // string

try {
    $result = $apiInstance->testQueryIntegerBooleanString($integer_query, $boolean_query, $string_query);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling QueryApi->testQueryIntegerBooleanString: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **integer_query** | **int**|  | [optional] |
| **boolean_query** | **bool**|  | [optional] |
| **string_query** | **string**|  | [optional] |

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

## `testQueryStyleDeepObjectExplodeTrueObject()`

```php
testQueryStyleDeepObjectExplodeTrueObject($query_object): string
```

Test query parameter(s)

Test query parameter(s)

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\QueryApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$query_object = new \OpenAPI\Client\Model\\OpenAPI\Client\Model\Pet(); // \OpenAPI\Client\Model\Pet

try {
    $result = $apiInstance->testQueryStyleDeepObjectExplodeTrueObject($query_object);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling QueryApi->testQueryStyleDeepObjectExplodeTrueObject: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **query_object** | [**\OpenAPI\Client\Model\Pet**](../Model/.md)|  | [optional] |

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

## `testQueryStyleDeepObjectExplodeTrueObjectAllOf()`

```php
testQueryStyleDeepObjectExplodeTrueObjectAllOf($query_object): string
```

Test query parameter(s)

Test query parameter(s)

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\QueryApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$query_object = new \OpenAPI\Client\Model\\OpenAPI\Client\Model\TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter(); // \OpenAPI\Client\Model\TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter

try {
    $result = $apiInstance->testQueryStyleDeepObjectExplodeTrueObjectAllOf($query_object);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling QueryApi->testQueryStyleDeepObjectExplodeTrueObjectAllOf: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **query_object** | [**\OpenAPI\Client\Model\TestQueryStyleDeepObjectExplodeTrueObjectAllOfQueryObjectParameter**](../Model/.md)|  | [optional] |

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

## `testQueryStyleFormExplodeFalseArrayInteger()`

```php
testQueryStyleFormExplodeFalseArrayInteger($query_object): string
```

Test query parameter(s)

Test query parameter(s)

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\QueryApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$query_object = array(56); // int[]

try {
    $result = $apiInstance->testQueryStyleFormExplodeFalseArrayInteger($query_object);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling QueryApi->testQueryStyleFormExplodeFalseArrayInteger: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **query_object** | [**int[]**](../Model/int.md)|  | [optional] |

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

## `testQueryStyleFormExplodeFalseArrayString()`

```php
testQueryStyleFormExplodeFalseArrayString($query_object): string
```

Test query parameter(s)

Test query parameter(s)

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\QueryApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$query_object = array('query_object_example'); // string[]

try {
    $result = $apiInstance->testQueryStyleFormExplodeFalseArrayString($query_object);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling QueryApi->testQueryStyleFormExplodeFalseArrayString: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **query_object** | [**string[]**](../Model/string.md)|  | [optional] |

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

## `testQueryStyleFormExplodeTrueArrayString()`

```php
testQueryStyleFormExplodeTrueArrayString($query_object): string
```

Test query parameter(s)

Test query parameter(s)

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\QueryApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$query_object = new \OpenAPI\Client\Model\\OpenAPI\Client\Model\TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter(); // \OpenAPI\Client\Model\TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter

try {
    $result = $apiInstance->testQueryStyleFormExplodeTrueArrayString($query_object);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling QueryApi->testQueryStyleFormExplodeTrueArrayString: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **query_object** | [**\OpenAPI\Client\Model\TestQueryStyleFormExplodeTrueArrayStringQueryObjectParameter**](../Model/.md)|  | [optional] |

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

## `testQueryStyleFormExplodeTrueObject()`

```php
testQueryStyleFormExplodeTrueObject($query_object): string
```

Test query parameter(s)

Test query parameter(s)

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\QueryApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$query_object = new \OpenAPI\Client\Model\\OpenAPI\Client\Model\Pet(); // \OpenAPI\Client\Model\Pet

try {
    $result = $apiInstance->testQueryStyleFormExplodeTrueObject($query_object);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling QueryApi->testQueryStyleFormExplodeTrueObject: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **query_object** | [**\OpenAPI\Client\Model\Pet**](../Model/.md)|  | [optional] |

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

## `testQueryStyleFormExplodeTrueObjectAllOf()`

```php
testQueryStyleFormExplodeTrueObjectAllOf($query_object): string
```

Test query parameter(s)

Test query parameter(s)

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');



$apiInstance = new OpenAPI\Client\Api\QueryApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$query_object = new \OpenAPI\Client\Model\\OpenAPI\Client\Model\DataQuery(); // \OpenAPI\Client\Model\DataQuery

try {
    $result = $apiInstance->testQueryStyleFormExplodeTrueObjectAllOf($query_object);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling QueryApi->testQueryStyleFormExplodeTrueObjectAllOf: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

| Name | Type | Description  | Notes |
| ------------- | ------------- | ------------- | ------------- |
| **query_object** | [**\OpenAPI\Client\Model\DataQuery**](../Model/.md)|  | [optional] |

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
