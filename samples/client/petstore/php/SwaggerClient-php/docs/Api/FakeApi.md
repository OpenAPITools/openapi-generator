# Swagger\Client\FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testClientModel**](FakeApi.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트
[**testEnumQueryParameters**](FakeApi.md#testEnumQueryParameters) | **GET** /fake | To test enum query parameters


# **testClientModel**
> \Swagger\Client\Model\Client testClientModel($body)

To test \"client\" model

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\FakeApi();
$body = new \Swagger\Client\Model\Client(); // \Swagger\Client\Model\Client | client model

try {
    $result = $api_instance->testClientModel($body);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testClientModel: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**\Swagger\Client\Model\Client**](../Model/\Swagger\Client\Model\Client.md)| client model |

### Return type

[**\Swagger\Client\Model\Client**](../Model/Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **testEndpointParameters**
> testEndpointParameters($number, $double, $string, $byte, $integer, $int32, $int64, $float, $binary, $date, $date_time, $password)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\FakeApi();
$number = 3.4; // float | None
$double = 1.2; // double | None
$string = "string_example"; // string | None
$byte = "B"; // string | None
$integer = 56; // int | None
$int32 = 56; // int | None
$int64 = 789; // int | None
$float = 3.4; // float | None
$binary = "B"; // string | None
$date = new \DateTime(); // \DateTime | None
$date_time = new \DateTime(); // \DateTime | None
$password = "password_example"; // string | None

try {
    $api_instance->testEndpointParameters($number, $double, $string, $byte, $integer, $int32, $int64, $float, $binary, $date, $date_time, $password);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testEndpointParameters: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **float**| None |
 **double** | **double**| None |
 **string** | **string**| None |
 **byte** | **string**| None |
 **integer** | **int**| None | [optional]
 **int32** | **int**| None | [optional]
 **int64** | **int**| None | [optional]
 **float** | **float**| None | [optional]
 **binary** | **string**| None | [optional]
 **date** | **\DateTime**| None | [optional]
 **date_time** | **\DateTime**| None | [optional]
 **password** | **string**| None | [optional]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: application/xml; charset=utf-8, application/json; charset=utf-8

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **testEnumQueryParameters**
> testEnumQueryParameters($enum_query_string, $enum_query_integer, $enum_query_double)

To test enum query parameters

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\FakeApi();
$enum_query_string = "-efg"; // string | Query parameter enum test (string)
$enum_query_integer = 3.4; // float | Query parameter enum test (double)
$enum_query_double = 1.2; // double | Query parameter enum test (double)

try {
    $api_instance->testEnumQueryParameters($enum_query_string, $enum_query_integer, $enum_query_double);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testEnumQueryParameters: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_query_string** | **string**| Query parameter enum test (string) | [optional] [default to -efg]
 **enum_query_integer** | **float**| Query parameter enum test (double) | [optional]
 **enum_query_double** | **double**| Query parameter enum test (double) | [optional]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

