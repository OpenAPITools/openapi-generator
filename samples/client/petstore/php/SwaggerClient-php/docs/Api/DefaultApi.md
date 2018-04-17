# Swagger\Client\DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testBodyWithQueryParams**](DefaultApi.md#testBodyWithQueryParams) | **PUT** /fake/body-with-query-params | 


# **testBodyWithQueryParams**
> testBodyWithQueryParams($body, $query)



### Example
```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');

$apiInstance = new Swagger\Client\Api\DefaultApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client()
);
$body = new \Swagger\Client\Model\User(); // \Swagger\Client\Model\User | 
$query = "query_example"; // string | 

try {
    $apiInstance->testBodyWithQueryParams($body, $query);
} catch (Exception $e) {
    echo 'Exception when calling DefaultApi->testBodyWithQueryParams: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**\Swagger\Client\Model\User**](../Model/User.md)|  |
 **query** | **string**|  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

