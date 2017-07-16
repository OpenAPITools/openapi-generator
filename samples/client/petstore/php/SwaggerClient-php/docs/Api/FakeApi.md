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
[**testJsonFormData**](FakeApi.md#testJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data


# **fakeOuterBooleanSerialize**
> \Swagger\Client\Model\OuterBoolean fakeOuterBooleanSerialize($body)



Test serialization of outer boolean types

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\FakeApi(new \Http\Adapter\Guzzle6\Client());
$body = new \Swagger\Client\Model\OuterBoolean(); // \Swagger\Client\Model\OuterBoolean | Input boolean as post body

try {
    $result = $api_instance->fakeOuterBooleanSerialize($body);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->fakeOuterBooleanSerialize: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**\Swagger\Client\Model\OuterBoolean**](../Model/OuterBoolean.md)| Input boolean as post body | [optional]

### Return type

[**\Swagger\Client\Model\OuterBoolean**](../Model/OuterBoolean.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **fakeOuterCompositeSerialize**
> \Swagger\Client\Model\OuterComposite fakeOuterCompositeSerialize($body)



Test serialization of object with outer number type

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\FakeApi(new \Http\Adapter\Guzzle6\Client());
$body = new \Swagger\Client\Model\OuterComposite(); // \Swagger\Client\Model\OuterComposite | Input composite as post body

try {
    $result = $api_instance->fakeOuterCompositeSerialize($body);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->fakeOuterCompositeSerialize: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**\Swagger\Client\Model\OuterComposite**](../Model/OuterComposite.md)| Input composite as post body | [optional]

### Return type

[**\Swagger\Client\Model\OuterComposite**](../Model/OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **fakeOuterNumberSerialize**
> \Swagger\Client\Model\OuterNumber fakeOuterNumberSerialize($body)



Test serialization of outer number types

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\FakeApi(new \Http\Adapter\Guzzle6\Client());
$body = new \Swagger\Client\Model\OuterNumber(); // \Swagger\Client\Model\OuterNumber | Input number as post body

try {
    $result = $api_instance->fakeOuterNumberSerialize($body);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->fakeOuterNumberSerialize: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**\Swagger\Client\Model\OuterNumber**](../Model/OuterNumber.md)| Input number as post body | [optional]

### Return type

[**\Swagger\Client\Model\OuterNumber**](../Model/OuterNumber.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **fakeOuterStringSerialize**
> \Swagger\Client\Model\OuterString fakeOuterStringSerialize($body)



Test serialization of outer string types

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\FakeApi(new \Http\Adapter\Guzzle6\Client());
$body = new \Swagger\Client\Model\OuterString(); // \Swagger\Client\Model\OuterString | Input string as post body

try {
    $result = $api_instance->fakeOuterStringSerialize($body);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->fakeOuterStringSerialize: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**\Swagger\Client\Model\OuterString**](../Model/OuterString.md)| Input string as post body | [optional]

### Return type

[**\Swagger\Client\Model\OuterString**](../Model/OuterString.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

# **testClientModel**
> \Swagger\Client\Model\Client testClientModel($body)

To test \"client\" model

To test \"client\" model

### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\FakeApi(new \Http\Adapter\Guzzle6\Client());
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
 **body** | [**\Swagger\Client\Model\Client**](../Model/Client.md)| client model |

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

$api_instance = new Swagger\Client\Api\FakeApi(new \Http\Adapter\Guzzle6\Client());
$number = 3.4; // float | None
$double = 1.2; // double | None
$pattern_without_delimiter = "pattern_without_delimiter_example"; // string | None
$byte = "byte_example"; // string | None
$integer = 56; // int | None
$int32 = 56; // int | None
$int64 = 789; // int | None
$float = 3.4; // float | None
$string = "string_example"; // string | None
$binary = "B"; // string | None
$date = new \DateTime("2013-10-20"); // \DateTime | None
$date_time = new \DateTime("2013-10-20T19:20:30+01:00"); // \DateTime | None
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

$api_instance = new Swagger\Client\Api\FakeApi(new \Http\Adapter\Guzzle6\Client());
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

# **testJsonFormData**
> testJsonFormData($param, $param2)

test json serialization of form data



### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$api_instance = new Swagger\Client\Api\FakeApi(new \Http\Adapter\Guzzle6\Client());
$param = "param_example"; // string | field1
$param2 = "param2_example"; // string | field2

try {
    $api_instance->testJsonFormData($param, $param2);
} catch (Exception $e) {
    echo 'Exception when calling FakeApi->testJsonFormData: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **string**| field1 |
 **param2** | **string**| field2 |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

