# Swagger\Client\FakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters


# **testEndpointParameters**
> testEndpointParameters($number, $double, $string, $byte, $integer, $int32, $int64, $float, $binary, $date, $date_time, $password)

Fake endpoint for testing various parameters

Fake endpoint for testing various parameters

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
    echo 'Exception when calling FakeApi->testEndpointParameters: ', $e->getMessage(), "\n";
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

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

