# Swagger\Client\FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** /fake/outer/boolean | 
[**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** /fake/outer/composite | 
[**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **POST** /fake/outer/number | 
[**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **POST** /fake/outer/string | 
[**testClientModel**](FakeApi.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트
[**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** /fake | To test enum parameters
[**testInlineAdditionalProperties**](FakeApi.md#testInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData**](FakeApi.md#testJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data


# **fakeOuterBooleanSerialize**
> \Swagger\Client\Model\OuterBoolean fakeOuterBooleanSerialize($UNKNOWN_PARAM_NAME)



Test serialization of outer boolean types

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$apiInstance = new Swagger\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$UNKNOWN_PARAM_NAME = new \Swagger\Client\Model\null(); //  | Input boolean as post body

try {
    $result = $apiInstance->fakeOuterBooleanSerialize($UNKNOWN_PARAM_NAME);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->fakeOuterBooleanSerialize: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **UNKNOWN_PARAM_NAME** | [****](../Model/.md)| Input boolean as post body | [optional]

### Return type

[**\Swagger\Client\Model\OuterBoolean**](../Model/OuterBoolean.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **fakeOuterCompositeSerialize**
> \Swagger\Client\Model\OuterComposite fakeOuterCompositeSerialize($outer_composite)



Test serialization of object with outer number type

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$apiInstance = new Swagger\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$outer_composite = new \Swagger\Client\Model\OuterComposite(); // \Swagger\Client\Model\OuterComposite | Input composite as post body

try {
    $result = $apiInstance->fakeOuterCompositeSerialize($outer_composite);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->fakeOuterCompositeSerialize: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outer_composite** | [**\Swagger\Client\Model\OuterComposite**](../Model/OuterComposite.md)| Input composite as post body | [optional]

### Return type

[**\Swagger\Client\Model\OuterComposite**](../Model/OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **fakeOuterNumberSerialize**
> \Swagger\Client\Model\OuterNumber fakeOuterNumberSerialize($UNKNOWN_PARAM_NAME)



Test serialization of outer number types

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$apiInstance = new Swagger\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$UNKNOWN_PARAM_NAME = new \Swagger\Client\Model\null(); //  | Input number as post body

try {
    $result = $apiInstance->fakeOuterNumberSerialize($UNKNOWN_PARAM_NAME);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->fakeOuterNumberSerialize: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **UNKNOWN_PARAM_NAME** | [****](../Model/.md)| Input number as post body | [optional]

### Return type

[**\Swagger\Client\Model\OuterNumber**](../Model/OuterNumber.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **fakeOuterStringSerialize**
> \Swagger\Client\Model\OuterString fakeOuterStringSerialize($UNKNOWN_PARAM_NAME)



Test serialization of outer string types

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$apiInstance = new Swagger\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$UNKNOWN_PARAM_NAME = new \Swagger\Client\Model\null(); //  | Input string as post body

try {
    $result = $apiInstance->fakeOuterStringSerialize($UNKNOWN_PARAM_NAME);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->fakeOuterStringSerialize: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **UNKNOWN_PARAM_NAME** | [****](../Model/.md)| Input string as post body | [optional]

### Return type

[**\Swagger\Client\Model\OuterString**](../Model/OuterString.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **testClientModel**
> \Swagger\Client\Model\Client testClientModel($client)

To test \"client\" model

To test \"client\" model

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$apiInstance = new Swagger\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$client = new \Swagger\Client\Model\Client(); // \Swagger\Client\Model\Client | client model

try {
    $result = $apiInstance->testClientModel($client);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testClientModel: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | [**\Swagger\Client\Model\Client**](../Model/Client.md)| client model |

### Return type

[**\Swagger\Client\Model\Client**](../Model/Client.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **testEndpointParameters**
> testEndpointParameters()

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

// Configure HTTP basic authorization: http_basic_test
$config = Swagger\Client\Configuration::getDefaultConfiguration()
              ->setUsername('YOUR_USERNAME')
              ->setPassword('YOUR_PASSWORD');


$apiInstance = new Swagger\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);

try {
    $apiInstance->testEndpointParameters();
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testEndpointParameters: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters
This endpoint does not need any parameter.

### Return type

void (empty response body)

### Authorization

[http_basic_test](../../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **testEnumParameters**
> testEnumParameters($enum_header_string_array, $enum_header_string, $enum_query_string_array, $enum_query_string, $enum_query_integer)

To test enum parameters

To test enum parameters

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$apiInstance = new Swagger\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$enum_header_string_array = array("enum_header_string_array_example"); // string[] | Header parameter enum test (string array)
$enum_header_string = "enum_header_string_example"; // string | Header parameter enum test (string)
$enum_query_string_array = array("enum_query_string_array_example"); // string[] | Query parameter enum test (string array)
$enum_query_string = "enum_query_string_example"; // string | Query parameter enum test (string)
$enum_query_integer = 56; // int | Query parameter enum test (double)

try {
    $apiInstance->testEnumParameters($enum_header_string_array, $enum_header_string, $enum_query_string_array, $enum_query_string, $enum_query_integer);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testEnumParameters: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enum_header_string_array** | [**string[]**](../Model/string.md)| Header parameter enum test (string array) | [optional]
 **enum_header_string** | **string**| Header parameter enum test (string) | [optional]
 **enum_query_string_array** | [**string[]**](../Model/string.md)| Query parameter enum test (string array) | [optional]
 **enum_query_string** | **string**| Query parameter enum test (string) | [optional]
 **enum_query_integer** | **int**| Query parameter enum test (double) | [optional]

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **testInlineAdditionalProperties**
> testInlineAdditionalProperties($UNKNOWN_PARAM_NAME)

test inline additionalProperties

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$apiInstance = new Swagger\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$UNKNOWN_PARAM_NAME = new \Swagger\Client\Model\map(); // map | request body

try {
    $apiInstance->testInlineAdditionalProperties($UNKNOWN_PARAM_NAME);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testInlineAdditionalProperties: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **UNKNOWN_PARAM_NAME** | [**map**](../Model/map.md)| request body |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **testJsonFormData**
> testJsonFormData()

test json serialization of form data

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$apiInstance = new Swagger\Client\Api\FakeApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);

try {
    $apiInstance->testJsonFormData();
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testJsonFormData: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters
This endpoint does not need any parameter.

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

