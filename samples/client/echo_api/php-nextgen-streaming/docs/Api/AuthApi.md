# OpenAPI\Client\AuthApi

All URIs are relative to http://localhost:3000, except if the operation defines another base path.

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**testAuthHttpBasic()**](AuthApi.md#testAuthHttpBasic) | **POST** /auth/http/basic | To test HTTP basic authentication |
| [**testAuthHttpBearer()**](AuthApi.md#testAuthHttpBearer) | **POST** /auth/http/bearer | To test HTTP bearer authentication |


## `testAuthHttpBasic()`

```php
testAuthHttpBasic(): string
```

To test HTTP basic authentication

To test HTTP basic authentication

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');


// Configure HTTP basic authorization: http_auth
$config = OpenAPI\Client\Configuration::getDefaultConfiguration()
              ->setUsername('YOUR_USERNAME')
              ->setPassword('YOUR_PASSWORD');


$apiInstance = new OpenAPI\Client\Api\AuthApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);

try {
    $result = $apiInstance->testAuthHttpBasic();
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling AuthApi->testAuthHttpBasic: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

This endpoint does not need any parameter.

### Return type

**string**

### Authorization

[http_auth](../../README.md#http_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `text/plain`

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)

## `testAuthHttpBearer()`

```php
testAuthHttpBearer(): string
```

To test HTTP bearer authentication

To test HTTP bearer authentication

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');


// Configure Bearer authorization: http_bearer_auth
$config = OpenAPI\Client\Configuration::getDefaultConfiguration()->setAccessToken('YOUR_ACCESS_TOKEN');


$apiInstance = new OpenAPI\Client\Api\AuthApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);

try {
    $result = $apiInstance->testAuthHttpBearer();
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling AuthApi->testAuthHttpBearer: ', $e->getMessage(), PHP_EOL;
}
```

### Parameters

This endpoint does not need any parameter.

### Return type

**string**

### Authorization

[http_bearer_auth](../../README.md#http_bearer_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: `text/plain`

[[Back to top]](#) [[Back to API list]](../../README.md#endpoints)
[[Back to Model list]](../../README.md#models)
[[Back to README]](../../README.md)
