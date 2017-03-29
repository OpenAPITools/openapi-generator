# Swagger\Client\FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testClientModel**](FakeApi.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트
[**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** /fake | To test enum parameters


# **testClientModel**
> \Swagger\Client\Model\Client testClientModel($body)

To test \"client\" model

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
> testEndpointParameters($number, $double, $pattern_without_delimiter, $byte, $integer, $int32, $int64, $float, $string, $binary, $date, $date_time, $password, $callback)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

// Configure HTTP basic authorization: http_basic_test
Swagger\Client\Configuration::getDefaultConfiguration()->setUsername('YOUR_USERNAME');
Swagger\Client\Configuration::getDefaultConfiguration()->setPassword('YOUR_PASSWORD');

$api_instance = new Swagger\Client\Api\FakeApi();
$number = 3.4; // float | None
$double = 1.2; // double | None
$pattern_without_delimiter = "pattern_without_delimiter_example"; // string | None
$byte = "B"; // string | None
$integer = 56; // int | None
$int32 = 56; // int | None
$int64 = 789; // int | None
$float = 3.4; // float | None
$string = "string_example"; // string | None
$binary = "B"; // string | None
$date = new \DateTime(); // \DateTime | None
$date_time = new \DateTime(); // \DateTime | None
$password = "password_example"; // string | None
$callback = "callback_example"; // string | None

try {
    $api_instance->testEndpointParameters($number, $double, $pattern_without_delimiter, $byte, $integer, $int32, $int64, $float, $string, $binary, $date, $date_time, $password, $callback);
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
 **pattern_without_delimiter** | **string**| None |
 **byte** | **string**| None |
 **integer** | **int**| None | [optional]
 **int32** | **int**| None | [optional]
 **int64** | **int**| None | [optional]
 **float** | **float**| None | [optional]
 **string** | **string**| None | [optional]
 **binary** | **string**| None | [optional]
 **date** | **\DateTime**| None | [optional]
 **date_time** | **\DateTime**| None | [optional]
 **password** | **string**| None | [optional]
 **callback** | **string**| None | [optional]

### Return type

void (empty response body)

### Authorization

[http_basic_test](../../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: application/xml; charset=utf-8, application/json; charset=utf-8

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **testEnumParameters**
> testEnumParameters($enum_form_string_array, $enum_form_string, $enum_header_string_array, $enum_header_string, $enum_query_string_array, $enum_query_string, $enum_query_integer, $enum_query_double)

To test enum parameters

To test enum parameters

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\FakeApi();
$enum_form_string_array = array("enum_form_string_array_example"); // string[] | Form parameter enum test (string array)
$enum_form_string = "-efg"; // string | Form parameter enum test (string)
$enum_header_string_array = array("enum_header_string_array_example"); // string[] | Header parameter enum test (string array)
$enum_header_string = "-efg"; // string | Header parameter enum test (string)
$enum_query_string_array = array("enum_query_string_array_example"); // string[] | Query parameter enum test (string array)
$enum_query_string = "-efg"; // string | Query parameter enum test (string)
$enum_query_integer = 56; // int | Query parameter enum test (double)
$enum_query_double = 1.2; // double | Query parameter enum test (double)

try {
    $api_instance->testEnumParameters($enum_form_string_array, $enum_form_string, $enum_header_string_array, $enum_header_string, $enum_query_string_array, $enum_query_string, $enum_query_integer, $enum_query_double);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testEnumParameters: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_form_string_array** | [**string[]**](../Model/string.md)| Form parameter enum test (string array) | [optional]
 **enum_form_string** | **string**| Form parameter enum test (string) | [optional] [default to -efg]
 **enum_header_string_array** | [**string[]**](../Model/string.md)| Header parameter enum test (string array) | [optional]
 **enum_header_string** | **string**| Header parameter enum test (string) | [optional] [default to -efg]
 **enum_query_string_array** | [**string[]**](../Model/string.md)| Query parameter enum test (string array) | [optional]
 **enum_query_string** | **string**| Query parameter enum test (string) | [optional] [default to -efg]
 **enum_query_integer** | **int**| Query parameter enum test (double) | [optional]
 **enum_query_double** | **double**| Query parameter enum test (double) | [optional]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: */*
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

